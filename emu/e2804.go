package main

import "log"

type e2804 struct {
	mem    [0x200]uint8
	locked bool // ie oe == 0: read allowed, but not write
}

func (eeprom *e2804) read(addr uint16) uint8 {
	if !eeprom.locked {
		log.Fatal("Read attempted while unlocked")
	}
	return eeprom.mem[addr]
}

func (eeprom *e2804) write(addr uint16, val uint8) {
	if eeprom.locked {
		log.Fatal("Write attempted while locked")
	}
	eeprom.mem[addr] = val
	eeprom.locked = true
}

func (eeprom *e2804) init_eeprom() {
	eeprom.locked = true
	for i := 0; i < len(eeprom.mem); i++ {
		eeprom.mem[i] = 0xff
	}
}
