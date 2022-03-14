package main

type SL_STATE int

const (
	STATE_IDLE SL_STATE = iota
	STATE_ACTIVE
	STATE_BIT_LOAD
	STATE_BIT_SET
)

type Slapstic struct {
	rom         []uint8
	curr_bank   uint8
	loaded_bank uint8

	state  SL_STATE
	is_odd bool
}

func (slapstic *Slapstic) process_bank(addr uint16) {
	if slapstic.state == STATE_IDLE {
		if addr == 0x0000 {
			slapstic.state = STATE_ACTIVE
			return
		}
		return
	}

	if slapstic.state == STATE_ACTIVE {
		if addr < 0x100 {
			// simple banking
			switch addr {
			case 0x80:
				slapstic.curr_bank = 0
				slapstic.state = STATE_IDLE
			case 0x90:
				slapstic.curr_bank = 1
				slapstic.state = STATE_IDLE
			case 0xa0:
				slapstic.curr_bank = 2
				slapstic.state = STATE_IDLE
			case 0xb0:
				slapstic.curr_bank = 3
				slapstic.state = STATE_IDLE
			}
			return
		}
		if (addr & 0x1ff0) == 0x1540 {
			slapstic.state = STATE_BIT_LOAD
			return
		}
	}

	if slapstic.state == STATE_BIT_LOAD {
		if addr == 0x0000 {
			slapstic.state = STATE_ACTIVE
			return
		}

		if (addr & 0x1fcf) == 0x80 {
			slapstic.loaded_bank = slapstic.curr_bank
			slapstic.is_odd = true
			slapstic.state = STATE_BIT_SET
			return
		}
	}

	if slapstic.state == STATE_BIT_SET {
		if addr == 0x0000 {
			slapstic.state = STATE_ACTIVE
			return
		}

		if slapstic.is_odd {
			masked := addr & 0x1ff3
			if masked == 0x1540 {
				slapstic.loaded_bank &= 0xfe
				slapstic.is_odd = false
				return
			}
			if masked == 0x1541 {
				slapstic.loaded_bank |= 1
				slapstic.is_odd = false
				return
			}
			if masked == 0x1542 {
				slapstic.loaded_bank &= 0xfd
				slapstic.is_odd = false
				return
			}
			if masked == 0x1543 {
				slapstic.loaded_bank |= 2
				slapstic.is_odd = false
				return
			}
		}

		if !slapstic.is_odd {
			masked := addr & 0x1ff3
			if masked == 0x1540 {
				slapstic.loaded_bank |= 2
				slapstic.is_odd = true
				return
			}
			if masked == 0x1541 {
				slapstic.loaded_bank &= 0xfd
				slapstic.is_odd = true
				return
			}
			if masked == 0x1542 {
				slapstic.loaded_bank |= 1
				slapstic.is_odd = true
				return
			}
			if masked == 0x1543 {
				slapstic.loaded_bank &= 0xfe
				slapstic.is_odd = true
				return
			}
		}

		if (addr & 0x1ff8) == 0x1550 {
			slapstic.curr_bank = slapstic.loaded_bank
			slapstic.state = STATE_IDLE
			return
		}
	}
}

func (slapstic *Slapstic) read(addr uint16) uint8 {
	bank := uint16(slapstic.curr_bank & 1)

	if addr >= 0x2000 {
		slapstic.process_bank(addr - 0x2000)
	}

	return slapstic.rom[bank*0x4000+addr]
}

func (slapstic *Slapstic) init_slapstic() {
	slapstic.curr_bank = 3
	slapstic.state = STATE_IDLE
}
