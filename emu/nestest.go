//go:build nestest
// +build nestest

package main

import (
	"embed"
	"log"
)

//go:embed nestest.nes
var f embed.FS

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	rom, err := f.ReadFile("nestest.nes")
	check(err)
	ram := make([]uint8, 0x800)

	cpu := m6502{}
	cpu.init_ops()
	cpu.read_cb = func(addr uint16) uint8 {
		if addr >= 0xc000 {
			return rom[addr-0xc000+0x10]
		}
		if addr < 0x800 {
			return ram[addr]
		}
		log.Fatalf("Invalid read from %x\n%s\n", addr, cpu.curr_state())
		return 0
	}
	cpu.write_cb = func(addr uint16, val uint8) {
		if addr < 0x800 {
			ram[addr] = val
			return
		}
		log.Fatalf("Invalid write to %x\n%s\n", addr, cpu.curr_state())
	}
	cpu.init_cpu()
	cpu.PC = 0xc000
	for {
		cpu.run_opcode()
	}
}
