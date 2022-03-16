package main

import (
	"fmt"
	"log"
	"math/rand"
)

type Pokey struct {
	idx         int
	all_port_cb func() uint8
	chn1        *PokeyChannel
	chn2        *PokeyChannel
	chn3        *PokeyChannel
	chn4        *PokeyChannel

	poly4  int
	poly5  int
	poly9  int
	poly17 int

	is9bitPoly           bool
	isChn1MHzFreq        bool
	isChn3MHzFreq        bool
	isChn2ClockedByChn1  bool
	isChn4ClockedByChn3  bool
	isHighPassFilterChn1 bool
	isHighPassFilterChn2 bool
	is15KHzFreq          bool
}

type PokeyChannel struct {
	idx           int
	noiseSampling uint8
	volume        uint8
	volumeOnly    bool
	frequency     uint8
	pokey         *Pokey

	// Every APU cycle, decrease the hz counter
	// When 0, clock the channel & reset to hzMultiplier
	// 1 if 1.79MHz, 28 if 64KHz, 114 if 15KHz
	hzMultiplier int
	hzCounter    int
	// Everytime the above is clocked, dec freqCounter
	// When 0, change sample value & reset to frequency+1
	freqCounter uint8

	isHighPhase      bool
	isValidHighPhase bool
}

func (pokey *Pokey) print_state() {
	chn1 := pokey.chn1
	chn2 := pokey.chn2
	chn3 := pokey.chn3
	chn4 := pokey.chn4
	fmt.Printf(
		"Idx: %x, Chn1 is MHz: %t, Chn3 is MHz: %t, Chn2 clocked by Chn1: %t, Chn4 clocked by Chn3: %t, Is 15KHz: %t\n"+
			"Chn1: freq: %x, noise sampling: %x, volume: %x, vol-only: %t\n"+
			"Chn2: freq: %x, noise sampling: %x, volume: %x, vol-only: %t\n"+
			"Chn3: freq: %x, noise sampling: %x, volume: %x, vol-only: %t\n"+
			"Chn4: freq: %x, noise sampling: %x, volume: %x, vol-only: %t\n\n",
		pokey.idx, pokey.isChn1MHzFreq, pokey.isChn3MHzFreq, pokey.isChn2ClockedByChn1, pokey.isChn4ClockedByChn3, pokey.is15KHzFreq,
		chn1.frequency, chn1.noiseSampling, chn1.volume, chn1.volumeOnly,
		chn2.frequency, chn2.noiseSampling, chn2.volume, chn2.volumeOnly,
		chn3.frequency, chn3.noiseSampling, chn3.volume, chn3.volumeOnly,
		chn4.frequency, chn4.noiseSampling, chn4.volume, chn4.volumeOnly,
	)
}

func (chn *PokeyChannel) flip_high_phase() {
	chn.isHighPhase = !chn.isHighPhase
	chn.isValidHighPhase = false
	if chn.isHighPhase && chn.noise_sample_pulsed() {
		chn.isValidHighPhase = true
	}
}

func (chn *PokeyChannel) dec_freq_timers() {
	pokey := chn.pokey
	chn2 := pokey.chn2
	chn4 := pokey.chn4

	// check for borrows
	if chn.freqCounter == 0 {
		if chn.idx == 1 && pokey.isChn2ClockedByChn1 {
			chn2.freqCounter--
		}
		if chn.idx == 3 && pokey.isChn4ClockedByChn3 {
			chn4.freqCounter--
		}
	}

	chn.freqCounter--
	if chn.freqCounter == 0 {
		if chn.idx == 1 && pokey.isChn2ClockedByChn1 {
			if chn2.freqCounter == 0 {
				chn.freqCounter = chn.frequency
				chn2.freqCounter = chn2.frequency
				if uint16(chn.freqCounter)+7 > 0x100 {
					chn2.freqCounter++
				}
				chn.freqCounter += 7
				chn2.flip_high_phase()
			}
			return
		} else if chn.idx == 3 && pokey.isChn4ClockedByChn3 {
			if chn4.freqCounter == 0 {
				chn.freqCounter = chn.frequency
				chn4.freqCounter = chn4.frequency
				if uint16(chn.freqCounter)+7 > 0x100 {
					chn4.freqCounter++
				}
				chn.freqCounter += 7
				chn4.flip_high_phase()
			}
			return
		}
		chn.freqCounter = chn.frequency + 1
		chn.flip_high_phase()
	}
}

func (chn *PokeyChannel) dec_timers() {
	chn.hzCounter--
	if chn.hzCounter == 0 {
		chn.hzCounter = chn.hzMultiplier
		chn.dec_freq_timers()
	}
}

func NewPokey(idx int) *Pokey {
	pokey := Pokey{
		idx:    idx,
		poly9:  0x1ff,
		poly17: 0x1ffff,
	}
	chn1 := PokeyChannel{
		idx:          1,
		pokey:        &pokey,
		hzMultiplier: 28,
		hzCounter:    28,
		frequency:    1,
		freqCounter:  1,
	}
	chn2 := PokeyChannel{
		idx:          2,
		pokey:        &pokey,
		hzMultiplier: 28,
		hzCounter:    28,
		frequency:    1,
		freqCounter:  1,
	}
	chn3 := PokeyChannel{
		idx:          3,
		pokey:        &pokey,
		hzMultiplier: 28,
		hzCounter:    28,
		frequency:    1,
		freqCounter:  1,
	}
	chn4 := PokeyChannel{
		idx:          4,
		pokey:        &pokey,
		hzMultiplier: 28,
		hzCounter:    28,
		frequency:    1,
		freqCounter:  1,
	}
	pokey.chn1 = &chn1
	pokey.chn2 = &chn2
	pokey.chn3 = &chn3
	pokey.chn4 = &chn4
	return &pokey
}

func (pokey *Pokey) read_port(addr uint8) uint8 {
	// Doesn't read from pots (0-7), kbcode (9), serial port input (0xd),
	// irq status (0xe),  serial port 4 key status (0xf)
	// todo: reads from rng at 1:4f86, 2:b06e, 2:b087, 2:b125, 2:b164, 2:e793
	if addr == 8 {
		// Machine ports
		return pokey.all_port_cb()
	} else if addr == 0xa {
		// RNG
		// todo: it's more complex than this
		return uint8(rand.Intn(256))
	}
	log.Fatalf("pokey read: %x", addr)
	return 0
}

func (pokey *Pokey) write_port(addr uint8, val uint8) {
	// Doesn't write to timers (9), reset status (0xa), serial (0xd), irq enable (0xe)
	if addr == 0 {
		// Channel 1 freq
		pokey.chn1.frequency = val
		return
	}
	if addr == 1 {
		// Channel 1 control
		pokey.chn1.noiseSampling = val >> 5
		pokey.chn1.volume = val & 0xf
		pokey.chn1.volumeOnly = (val & 0x10) != 0
		return
	}
	if addr == 2 {
		// Channel 2 freq
		pokey.chn2.frequency = val
		return
	}
	if addr == 3 {
		// Channel 2 control
		pokey.chn2.noiseSampling = val >> 5
		pokey.chn2.volume = val & 0xf
		pokey.chn2.volumeOnly = (val & 0x10) != 0
		return
	}
	if addr == 4 {
		// Channel 3 freq
		pokey.chn3.frequency = val
		return
	}
	if addr == 5 {
		// Channel 3 control
		pokey.chn3.noiseSampling = val >> 5
		pokey.chn3.volume = val & 0xf
		pokey.chn3.volumeOnly = (val & 0x10) != 0
		return
	}
	if addr == 6 {
		// Channel 4 freq
		pokey.chn4.frequency = val
		return
	}
	if addr == 7 {
		// Channel 4 control
		pokey.chn4.noiseSampling = val >> 5
		pokey.chn4.volume = val & 0xf
		pokey.chn4.volumeOnly = (val & 0x10) != 0
		return
	}
	if addr == 8 {
		// Audio control
		pokey.is9bitPoly = (val & 0x80) != 0
		pokey.isChn1MHzFreq = (val & 0x40) != 0
		pokey.isChn3MHzFreq = (val & 0x20) != 0
		pokey.isChn2ClockedByChn1 = (val & 0x10) != 0
		pokey.isChn4ClockedByChn3 = (val & 0x08) != 0
		pokey.isHighPassFilterChn1 = (val & 0x04) != 0 // todo
		pokey.isHighPassFilterChn2 = (val & 0x02) != 0 // todo
		pokey.is15KHzFreq = (val & 0x01) != 0

		if pokey.isHighPassFilterChn1 {
			log.Fatalf("implement high pass filter chn1")
		}
		if pokey.isHighPassFilterChn2 {
			log.Fatalf("implement high pass filter chn2")
		}

		baseHzMult := 28
		if pokey.is15KHzFreq {
			baseHzMult = 114
		}
		pokey.chn1.hzMultiplier = baseHzMult
		pokey.chn2.hzMultiplier = baseHzMult
		pokey.chn3.hzMultiplier = baseHzMult
		pokey.chn4.hzMultiplier = baseHzMult
		if pokey.isChn1MHzFreq {
			pokey.chn1.hzMultiplier = 1
		}
		if pokey.isChn3MHzFreq {
			pokey.chn3.hzMultiplier = 1
		}

		// log.Printf("Pokey: %d, Aud ctl: %x\n", pokey.idx, val)
		return
	}
	if addr == 0xb {
		// Start pot scan sequence
		// todo: no need to implement, this allows reading from Machine ports above
		//   and decays over 228 scanlines, but Machine port reading in Tetris
		//   is always paired with this reg
		return
	}
	if addr == 0xf {
		// Serial port 4 key control
		// todo: written to with 0 and 7
		// todo: ignore, it enables functionality like allport + keyboard
		// log.Printf("Serial port 4: $%x\n", val)
		return
	}
	log.Fatalf("pokey write: %x %x", addr, val)
}

func (pokey *Pokey) dec_timers() {
	pokey.chn1.dec_timers()
	if !pokey.isChn2ClockedByChn1 {
		pokey.chn2.dec_timers()
	}
	pokey.chn3.dec_timers()
	if !pokey.isChn4ClockedByChn3 {
		pokey.chn4.dec_timers()
	}

	new4bit := ((pokey.poly4 & 1) ^ 1) ^ ((pokey.poly4 >> 1) & 1)
	pokey.poly4 = (pokey.poly4 >> 1) + new4bit*0x08

	new5bit := ((pokey.poly5 & 1) ^ 1) ^ ((pokey.poly5 >> 2) & 1)
	pokey.poly5 = (pokey.poly5 >> 1) + new5bit*0x10

	new9bit := (pokey.poly9 & 1) ^ ((pokey.poly9 >> 5) & 1)
	pokey.poly9 = (pokey.poly9 >> 1) + new9bit*0x100

	new17bit7 := ((pokey.poly17 >> 8) & 1) ^ ((pokey.poly17 >> 13) & 1)
	new17bit16 := pokey.poly17 & 1
	pokey.poly17 = (pokey.poly17 >> 1) & 0xff7f
	pokey.poly17 |= (new17bit7 << 7) | (new17bit16 << 16)
}

func (chn *PokeyChannel) noise_sample_pulsed() bool {
	pokey := chn.pokey

	is5bitSelection := (chn.noiseSampling & 4) == 0
	is17bitSelection := (chn.noiseSampling & 2) == 0 // else 4-bit
	consider4or17bitSelect := (chn.noiseSampling & 1) == 0

	// inverted bit 0s
	if is5bitSelection && (pokey.poly5&1) == 1 {
		return false
	}

	if !consider4or17bitSelect {
		return true
	}

	if is17bitSelection {
		if pokey.is9bitPoly {
			if (pokey.poly9 & 1) == 0 {
				return false
			}
		} else {
			if (pokey.poly17 & 1) == 0 {
				return false
			}
		}
	} else {
		if (pokey.poly4 & 1) == 0 {
			return false
		}
	}

	return true
}

func (chn *PokeyChannel) get_value() uint8 {
	if chn.volumeOnly {
		return chn.volume
	}

	if !chn.isValidHighPhase {
		return 0
	}

	return chn.volume
}

func (pokey *Pokey) get_sample() int {
	// max vals of 0xf000
	chn1val := int(pokey.chn1.get_value()) * 0x1000
	chn2val := int(pokey.chn2.get_value()) * 0x1000
	chn3val := int(pokey.chn3.get_value()) * 0x1000
	chn4val := int(pokey.chn4.get_value()) * 0x1000
	return (chn1val + chn2val + chn3val + chn4val) / 4
}
