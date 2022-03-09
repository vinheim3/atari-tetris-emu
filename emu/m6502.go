package main

import (
	"fmt"
	"log"
	"strings"
)

const (
	flagC       uint8  = 0x01 // Carry
	flagZ       uint8  = 0x02 // Zero
	flagI       uint8  = 0x04 // IRQ disable (not NMI)
	flagD       uint8  = 0x08 // If using decimal flag
	flagB       uint8  = 0x10 // When SR pushed, 1 if from BRK/PHP, 0 from hw, ignored from PLP/RTI
	flag5       uint8  = 0x20 // Ignored
	flagV       uint8  = 0x40 // Overflow - only when operands are the same sign, and result is different
	flagN       uint8  = 0x80 // Negative
	NMIVector   uint16 = 0xfffa
	ResetVector uint16 = 0xfffc
	IRQVector   uint16 = 0xfffe
)

type AddrMode int

const (
	modeInvalid AddrMode = iota
	modeA
	modeAbs
	modeAbsX
	modeAbsY
	modeImm  // no memwrite
	modeImpl // special
	modeInd  // special
	modeXInd // no memwrite
	modeIndY // no memwrite
	modeRel  // special
	modeZpg
	modeZpgX
	modeZpgY
)

type Opcode struct {
	encoding    string
	cycles      int
	instruction func()
}

type m6502 struct {
	PC     uint16
	A      uint8
	X      uint8
	Y      uint8
	SR     uint8
	SP     uint8 // pushes are high-low, with SP post-dec'd
	cycles int

	read_cb  func(uint16) uint8
	write_cb func(uint16, uint8)

	opcodes [256]Opcode

	considerExtraCycle bool
	ignoreRead         bool
}

func (cpu *m6502) is_flag_set(flag uint8) bool   { return (cpu.SR & flag) != 0 }
func (cpu *m6502) is_flag_clear(flag uint8) bool { return !cpu.is_flag_set(flag) }
func (cpu *m6502) set_flag(flag uint8)           { cpu.SR |= flag }
func (cpu *m6502) clear_flag(flag uint8)         { cpu.SR &= (flag ^ 0xff) }
func is_bit_set(val uint8, bit int) bool         { return (val & (1 << bit)) != 0 }

// func is_bit_clear(val uint8, bit int) bool       { return !is_bit_set(val, bit) }

func encoding(ins string, mode AddrMode) string {
	var suffix string
	switch mode {
	case modeA:
		suffix = "A"
	case modeAbs:
		suffix = "a16"
	case modeAbsX:
		suffix = "a16, x"
	case modeAbsY:
		suffix = "a16, y"
	case modeImm:
		suffix = "#"
	case modeImpl:
		suffix = ""
	case modeInd:
		suffix = "(a16)"
	case modeXInd:
		suffix = "(a16, x)"
	case modeIndY:
		suffix = "(a16), x"
	case modeRel:
		suffix = "r8"
	case modeZpg:
		suffix = "a8"
	case modeZpgX:
		suffix = "a8, x"
	case modeZpgY:
		suffix = "a8, y"
	default:
		log.Fatal("Invalid address mode")
	}

	return ins + " " + suffix
}

func (cpu *m6502) cond_set_flag(flag uint8, shouldSet bool) {
	if shouldSet {
		cpu.set_flag(flag)
	} else {
		cpu.clear_flag(flag)
	}
}

func (cpu *m6502) read_word(addr uint16) uint16 {
	return to_word(cpu.read_cb(addr), cpu.read_cb(addr+1))
}

func (cpu *m6502) read_page_wrapped_word(addr uint16) uint16 {
	if uint8(addr) == 0xff {
		return uint16(cpu.read_cb(addr)) + (uint16(cpu.read_cb(addr-0xff)) << 8)
	} else {
		return cpu.read_word(addr)
	}
}

func to_word(low uint8, high uint8) uint16 {
	return (uint16(high) << 8) + uint16(low)
}

func (cpu *m6502) read_pc_byte() uint8 {
	ret := cpu.read_cb(cpu.PC)
	cpu.PC++
	return ret
}

func (cpu *m6502) read_pc_word() uint16 {
	ret := cpu.read_word(cpu.PC)
	cpu.PC += 2
	return ret
}

func (cpu *m6502) get_operand(mode AddrMode) (uint8, func(uint8)) {
	if mode == modeA {
		return cpu.A, func(val uint8) { cpu.A = val }
	} else if mode == modeImm {
		return cpu.read_pc_byte(), nil
	}

	var val uint8
	var addr uint16

	switch mode {
	case modeAbs:
		addr = cpu.read_pc_word()
		if !cpu.ignoreRead {
			val = cpu.read_cb(addr)
		}
	case modeAbsX:
		baseAddr := cpu.read_pc_word()
		x16 := uint16(cpu.X)
		if cpu.considerExtraCycle {
			if ((baseAddr & 0xff) + x16) >= 0x100 {
				cpu.cycles++
			}
		}
		addr = baseAddr + x16
		if !cpu.ignoreRead {
			val = cpu.read_cb(addr)
		}
	case modeAbsY:
		baseAddr := cpu.read_pc_word()
		y16 := uint16(cpu.Y)
		if cpu.considerExtraCycle {
			if ((baseAddr & 0xff) + y16) >= 0x100 {
				cpu.cycles++
			}
		}
		addr = baseAddr + y16
		if !cpu.ignoreRead {
			val = cpu.read_cb(addr)
		}
	case modeXInd:
		addr = uint16(cpu.read_pc_byte() + cpu.X)
		addr = cpu.read_page_wrapped_word(addr)
		if !cpu.ignoreRead {
			val = cpu.read_cb(addr)
		}
	case modeIndY:
		baseAddr := cpu.read_page_wrapped_word(uint16(cpu.read_pc_byte()))
		y16 := uint16(cpu.Y)
		if cpu.considerExtraCycle {
			if ((baseAddr & 0xff) + y16) >= 0x100 {
				cpu.cycles++
			}
		}
		addr = baseAddr + y16
		if !cpu.ignoreRead {
			val = cpu.read_cb(addr)
		}
	case modeZpg:
		addr = uint16(cpu.read_pc_byte())
		if !cpu.ignoreRead {
			val = cpu.read_cb(addr)
		}
	case modeZpgX:
		addr = uint16(cpu.read_pc_byte() + cpu.X)
		if !cpu.ignoreRead {
			val = cpu.read_cb(addr)
		}
	case modeZpgY:
		addr = uint16(cpu.read_pc_byte() + cpu.Y)
		if !cpu.ignoreRead {
			val = cpu.read_cb(addr)
		}
	default:
		log.Fatal("Invalid address mode")

	}
	return val, func(val uint8) { cpu.write_cb(addr, val) }
}

func (cpu *m6502) cond_branch(cond bool) {
	rel := uint16(cpu.read_pc_byte())

	if cond {
		origPChigh := cpu.PC & 0xff00
		cpu.cycles++

		if rel < 0x80 {
			cpu.PC += rel
		} else {
			cpu.PC -= (0x100 - rel)
		}

		if origPChigh != (cpu.PC & 0xff00) {
			cpu.cycles++
		}
	}
}

func (cpu *m6502) push_byte(val uint8) {
	cpu.write_cb(0x100+uint16(cpu.SP), val)
	cpu.SP--
}

func (cpu *m6502) pull_byte() uint8 {
	cpu.SP++
	return cpu.read_cb(0x100 + uint16(cpu.SP))
}

func (cpu *m6502) call(addr uint16) {
	pushed_addr := cpu.PC - 1
	cpu.push_byte(uint8(pushed_addr >> 8))
	cpu.push_byte(uint8(pushed_addr))
	cpu.PC = addr
}

func (cpu *m6502) calli(pushedAddr uint16, extraFlags uint8) {
	cpu.push_byte(uint8(pushedAddr >> 8))
	cpu.push_byte(uint8(pushedAddr))
	cpu.push_byte(cpu.SR | extraFlags)
	irqAddr := cpu.read_word(IRQVector)
	cpu.PC = irqAddr
}

func (cpu *m6502) ret() {
	low := cpu.pull_byte()
	high := cpu.pull_byte()
	cpu.PC = to_word(low, high) + 1
}

func (cpu *m6502) reti() {
	cpu.plp()
	cpu.ret()
	cpu.PC--
}

func (cpu *m6502) _compare(reg uint8, val uint8) {
	// NZC
	cpu.cond_set_flag(flagC, reg >= val)
	reg -= val
	cpu.checkN(reg)
	cpu.checkZ(reg)
}

func (cpu *m6502) compare(mode AddrMode, reg uint8) {
	// NZC
	val, _ := cpu.get_operand(mode)
	cpu._compare(reg, val)
}

func (cpu *m6502) checkN(val uint8) { cpu.cond_set_flag(flagN, (val&0x80) != 0) }
func (cpu *m6502) checkZ(val uint8) { cpu.cond_set_flag(flagZ, val == 0) }
func (cpu *m6502) checkV(op1 uint8, op2 uint8, result uint8) {
	op1sign := op1 & 0x80
	sameSignOps := op1sign == (op2 & 0x80)
	diffSignRes := op1sign != (result & 0x80)
	cpu.cond_set_flag(flagV, sameSignOps && diffSignRes)
}

/* ----- Instructions start ----- */

func (cpu *m6502) aax(mode AddrMode) {
	_, write_op := cpu.get_operand(mode)
	result := cpu.A & cpu.X
	write_op(result)
}

func (cpu *m6502) _adc(val uint8) {
	// NZCV
	result := uint16(cpu.A) + uint16(val)
	if cpu.is_flag_set(flagC) {
		result++
	}
	res8 := uint8(result)

	cpu.checkV(cpu.A, val, res8)
	cpu.A = res8
	cpu.checkN(cpu.A)
	cpu.checkZ(cpu.A)
	cpu.cond_set_flag(flagC, result >= 0x100)
}

func (cpu *m6502) adc(mode AddrMode) {
	// NZCV
	cpu.considerExtraCycle = true
	val, _ := cpu.get_operand(mode)
	if cpu.is_flag_set(flagD) {
		// todo: tests + NV flags
		ln := (cpu.A & 0xf) + (val & 0xf)
		hn := (cpu.A >> 4) + (val >> 4)
		if cpu.is_flag_set(flagC) {
			ln++
		}
		if ln > 9 {
			hn++
			ln -= 10
		}
		if hn > 9 {
			hn -= 10
			cpu.set_flag(flagC)
		} else {
			cpu.clear_flag(flagC)
		}
		cpu.A = (hn << 4) + ln
		cpu.checkZ(cpu.A)
	} else {
		cpu._adc(val)
	}
}

func (cpu *m6502) and(mode AddrMode) {
	// NZ
	cpu.considerExtraCycle = true
	val, _ := cpu.get_operand(mode)
	cpu.A &= val
	cpu.checkN(cpu.A)
	cpu.checkZ(cpu.A)
}

func (cpu *m6502) asl(mode AddrMode) {
	// NZC
	val, write_op := cpu.get_operand(mode)
	cpu.cond_set_flag(flagC, is_bit_set(val, 7))
	val <<= 1
	write_op(val)
	cpu.checkN(val)
	cpu.checkZ(val)
}

func (cpu *m6502) bcc() { cpu.cond_branch(cpu.is_flag_clear(flagC)) }
func (cpu *m6502) bcs() { cpu.cond_branch(cpu.is_flag_set(flagC)) }
func (cpu *m6502) beq() { cpu.cond_branch(cpu.is_flag_set(flagZ)) }

func (cpu *m6502) bit(mode AddrMode) {
	// NZV
	val, _ := cpu.get_operand(mode)
	cpu.cond_set_flag(flagN, (val&flagN) != 0)
	cpu.checkZ(val & cpu.A)
	cpu.cond_set_flag(flagV, (val&flagV) != 0)
}

func (cpu *m6502) bmi() { cpu.cond_branch(cpu.is_flag_set(flagN)) }
func (cpu *m6502) bne() { cpu.cond_branch(cpu.is_flag_clear(flagZ)) }
func (cpu *m6502) bpl() { cpu.cond_branch(cpu.is_flag_clear(flagN)) }

func (cpu *m6502) brk() {
	cpu.calli(cpu.PC+1, flagB)
}

func (cpu *m6502) bvc() { cpu.cond_branch(cpu.is_flag_clear(flagV)) }
func (cpu *m6502) bvs() { cpu.cond_branch(cpu.is_flag_set(flagV)) }
func (cpu *m6502) clc() { cpu.clear_flag(flagC) }
func (cpu *m6502) cld() { cpu.clear_flag(flagD) }
func (cpu *m6502) cli() { cpu.clear_flag(flagI) }
func (cpu *m6502) clv() { cpu.clear_flag(flagV) }

func (cpu *m6502) cmp(mode AddrMode) { cpu.considerExtraCycle = true; cpu.compare(mode, cpu.A) }
func (cpu *m6502) cpx(mode AddrMode) { cpu.compare(mode, cpu.X) }
func (cpu *m6502) cpy(mode AddrMode) { cpu.compare(mode, cpu.Y) }

func (cpu *m6502) dcp(mode AddrMode) {
	// NZC
	val, write_op := cpu.get_operand(mode)
	val--
	write_op(val)
	cpu._compare(cpu.A, val)
}

func (cpu *m6502) dec(mode AddrMode) {
	// NZ
	val, write_op := cpu.get_operand(mode)
	val--
	cpu.checkN(val)
	cpu.checkZ(val)
	write_op(val)
}

func (cpu *m6502) dex() {
	// NZ
	cpu.X--
	cpu.checkN(cpu.X)
	cpu.checkZ(cpu.X)
}

func (cpu *m6502) dey() {
	// NZ
	cpu.Y--
	cpu.checkN(cpu.Y)
	cpu.checkZ(cpu.Y)
}

func (cpu *m6502) dop(mode AddrMode) { cpu.get_operand(mode) }

func (cpu *m6502) eor(mode AddrMode) {
	// NZ
	cpu.considerExtraCycle = true
	val, _ := cpu.get_operand(mode)
	cpu.A ^= val
	cpu.checkN(cpu.A)
	cpu.checkZ(cpu.A)
}

func (cpu *m6502) inc(mode AddrMode) {
	// NZ
	val, write_op := cpu.get_operand(mode)
	val++
	cpu.checkN(val)
	cpu.checkZ(val)
	write_op(val)
}

func (cpu *m6502) inx() {
	// NZ
	cpu.X++
	cpu.checkN(cpu.X)
	cpu.checkZ(cpu.X)
}

func (cpu *m6502) iny() {
	// NZ
	cpu.Y++
	cpu.checkN(cpu.Y)
	cpu.checkZ(cpu.Y)
}

func (cpu *m6502) isc(mode AddrMode) {
	// NZCV
	val, write_op := cpu.get_operand(mode)
	val++
	write_op(val)

	cpu._adc(val ^ 0xff)
}

func (cpu *m6502) jmp(mode AddrMode) {
	addr := cpu.read_pc_word()
	if mode == modeInd {
		addr = cpu.read_page_wrapped_word(addr)
	} else if mode != modeAbs {
		log.Fatal("Invalid jump address mode")
	}
	cpu.PC = addr
}

func (cpu *m6502) jsr() { cpu.call(cpu.read_pc_word()) }

func (cpu *m6502) lax(mode AddrMode) {
	// NZ
	cpu.considerExtraCycle = true
	val, _ := cpu.get_operand(mode)
	cpu.A = val
	cpu.X = val
	cpu.checkN(cpu.A)
	cpu.checkZ(cpu.A)
}

func (cpu *m6502) lda(mode AddrMode) {
	// NZ
	cpu.considerExtraCycle = true
	val, _ := cpu.get_operand(mode)
	cpu.A = val
	cpu.checkN(cpu.A)
	cpu.checkZ(cpu.A)
}

func (cpu *m6502) ldx(mode AddrMode) {
	// NZ
	cpu.considerExtraCycle = true
	val, _ := cpu.get_operand(mode)
	cpu.X = val
	cpu.checkN(cpu.X)
	cpu.checkZ(cpu.X)
}

func (cpu *m6502) ldy(mode AddrMode) {
	// NZ
	cpu.considerExtraCycle = true
	val, _ := cpu.get_operand(mode)
	cpu.Y = val
	cpu.checkN(cpu.Y)
	cpu.checkZ(cpu.Y)
}

func (cpu *m6502) lsr(mode AddrMode) {
	// N = 0, ZC
	val, write_op := cpu.get_operand(mode)
	cpu.cond_set_flag(flagC, is_bit_set(val, 0))
	val >>= 1
	write_op(val)
	cpu.clear_flag(flagN)
	cpu.checkZ(val)
}

func (cpu *m6502) nop() {}

func (cpu *m6502) ora(mode AddrMode) {
	// NZ
	cpu.considerExtraCycle = true
	val, _ := cpu.get_operand(mode)
	cpu.A |= val
	cpu.checkN(cpu.A)
	cpu.checkZ(cpu.A)
}

func (cpu *m6502) pha() { cpu.push_byte(cpu.A) }
func (cpu *m6502) php() { cpu.push_byte(cpu.SR | flagB) }

func (cpu *m6502) pla() {
	// NZ
	cpu.A = cpu.pull_byte()
	cpu.checkN(cpu.A)
	cpu.checkZ(cpu.A)
}
func (cpu *m6502) plp() {
	_flag5 := cpu.SR & flag5
	cpu.SR = cpu.pull_byte()&(0xff-(flag5|flagB)) | _flag5
}

func (cpu *m6502) rla(mode AddrMode) {
	// NZC
	val, write_op := cpu.get_operand(mode)
	origCarry := cpu.is_flag_set(flagC)
	cpu.cond_set_flag(flagC, is_bit_set(val, 7))
	val <<= 1
	if origCarry {
		val++
	}
	write_op(val)
	cpu.A &= val
	cpu.checkN(cpu.A)
	cpu.checkZ(cpu.A)
}

func (cpu *m6502) rol(mode AddrMode) {
	// NZC
	val, write_op := cpu.get_operand(mode)
	origCarry := cpu.is_flag_set(flagC)
	cpu.cond_set_flag(flagC, is_bit_set(val, 7))
	val <<= 1
	if origCarry {
		val++
	}
	write_op(val)
	cpu.checkN(val)
	cpu.checkZ(val)
}

func (cpu *m6502) ror(mode AddrMode) {
	// NZC
	val, write_op := cpu.get_operand(mode)
	origCarry := cpu.is_flag_set(flagC)
	cpu.cond_set_flag(flagC, is_bit_set(val, 0))
	val >>= 1
	if origCarry {
		val += 0x80
	}
	write_op(val)
	cpu.checkN(val)
	cpu.checkZ(val)
}

func (cpu *m6502) rra(mode AddrMode) {
	// NZCV
	val, write_op := cpu.get_operand(mode)
	origCarry := cpu.is_flag_set(flagC)
	cpu.cond_set_flag(flagC, is_bit_set(val, 0))
	val >>= 1
	if origCarry {
		val += 0x80
	}
	write_op(val)

	cpu._adc(val)
}

func (cpu *m6502) rti() { cpu.reti() }
func (cpu *m6502) rts() { cpu.ret() }

func (cpu *m6502) sbc(mode AddrMode) {
	// NZCV
	cpu.considerExtraCycle = true
	val, _ := cpu.get_operand(mode)

	cpu._adc(val ^ 0xff)
}

func (cpu *m6502) sec() { cpu.set_flag(flagC) }
func (cpu *m6502) sed() { cpu.set_flag(flagD) }
func (cpu *m6502) sei() { cpu.set_flag(flagI) }

func (cpu *m6502) slo(mode AddrMode) {
	// NZC
	val, write_op := cpu.get_operand(mode)
	cpu.cond_set_flag(flagC, is_bit_set(val, 7))
	val <<= 1
	write_op(val)
	cpu.A |= val
	cpu.checkN(cpu.A)
	cpu.checkZ(cpu.A)
}

func (cpu *m6502) sre(mode AddrMode) {
	// NZC
	val, write_op := cpu.get_operand(mode)
	cpu.cond_set_flag(flagC, is_bit_set(val, 0))
	val >>= 1
	write_op(val)
	cpu.A ^= val
	cpu.checkN(cpu.A)
	cpu.checkZ(cpu.A)
}

func (cpu *m6502) sta(mode AddrMode) {
	cpu.ignoreRead = true
	_, write_op := cpu.get_operand(mode)
	write_op(cpu.A)
}

func (cpu *m6502) stx(mode AddrMode) {
	cpu.ignoreRead = true
	_, write_op := cpu.get_operand(mode)
	write_op(cpu.X)
}

func (cpu *m6502) sty(mode AddrMode) {
	cpu.ignoreRead = true
	_, write_op := cpu.get_operand(mode)
	write_op(cpu.Y)
}

func (cpu *m6502) tax() {
	// NZ
	cpu.X = cpu.A
	cpu.checkN(cpu.X)
	cpu.checkZ(cpu.X)
}

func (cpu *m6502) tay() {
	// NZ
	cpu.Y = cpu.A
	cpu.checkN(cpu.Y)
	cpu.checkZ(cpu.Y)
}

func (cpu *m6502) top(mode AddrMode) {
	cpu.considerExtraCycle = true
	cpu.ignoreRead = true
	cpu.get_operand(mode)
}

func (cpu *m6502) tsx() {
	// NZ
	cpu.X = cpu.SP
	cpu.checkN(cpu.X)
	cpu.checkZ(cpu.X)
}

func (cpu *m6502) txa() {
	// NZ
	cpu.A = cpu.X
	cpu.checkN(cpu.A)
	cpu.checkZ(cpu.A)
}

func (cpu *m6502) txs() { cpu.SP = cpu.X }

func (cpu *m6502) tya() {
	// NZ
	cpu.A = cpu.Y
	cpu.checkN(cpu.A)
	cpu.checkZ(cpu.A)
}

/* ----- Instructions end ----- */

func (cpu *m6502) pc_in_range(addr1 uint16, addr2 uint16) bool {
	if cpu.PC >= addr1 && cpu.PC <= addr2 {
		return true
	}
	return false
}

func (cpu *m6502) curr_state() string {
	log := fmt.Sprintf(
		"%04x A:%02x X:%02x Y:%02x P:%02x SP:%02x CYC:%d\n",
		cpu.PC, cpu.A, cpu.X, cpu.Y, cpu.SR, cpu.SP, cpu.cycles)
	return strings.ToUpper(log)
}

func (cpu *m6502) print_state() {
	fmt.Print(cpu.curr_state())
}

func (cpu *m6502) run_opcode() {
	cpu.considerExtraCycle = false
	cpu.ignoreRead = false
	// cpu.print_state()
	op := cpu.read_pc_byte()
	cpu.opcodes[op].instruction()
	cpu.cycles += cpu.opcodes[op].cycles
}

func (cpu *m6502) run_irq() {
	cpu.calli(cpu.PC, 0)
	cpu.set_flag(flagI)
}

func (cpu *m6502) init_cpu() {
	cpu.PC = cpu.read_word(ResetVector)
	cpu.A = 0
	cpu.X = 0
	cpu.Y = 0
	cpu.SR = flagI | flag5
	cpu.cycles = 7
	cpu.SP = 0xfd
}

func (cpu *m6502) init_ops() {
	// todo: encoding using 2 tables

	// AAX
	cpu.opcodes[0x87].instruction = func() { cpu.aax(modeZpg) }
	cpu.opcodes[0x97].instruction = func() { cpu.aax(modeZpgY) }
	cpu.opcodes[0x83].instruction = func() { cpu.aax(modeXInd) }
	cpu.opcodes[0x8f].instruction = func() { cpu.aax(modeAbs) }

	// ADC
	cpu.opcodes[0x69].instruction = func() { cpu.adc(modeImm) }
	cpu.opcodes[0x65].instruction = func() { cpu.adc(modeZpg) }
	cpu.opcodes[0x75].instruction = func() { cpu.adc(modeZpgX) }
	cpu.opcodes[0x6d].instruction = func() { cpu.adc(modeAbs) }
	cpu.opcodes[0x7d].instruction = func() { cpu.adc(modeAbsX) }
	cpu.opcodes[0x79].instruction = func() { cpu.adc(modeAbsY) }
	cpu.opcodes[0x61].instruction = func() { cpu.adc(modeXInd) }
	cpu.opcodes[0x71].instruction = func() { cpu.adc(modeIndY) }

	// AND
	cpu.opcodes[0x29].instruction = func() { cpu.and(modeImm) }
	cpu.opcodes[0x25].instruction = func() { cpu.and(modeZpg) }
	cpu.opcodes[0x35].instruction = func() { cpu.and(modeZpgX) }
	cpu.opcodes[0x2d].instruction = func() { cpu.and(modeAbs) }
	cpu.opcodes[0x3d].instruction = func() { cpu.and(modeAbsX) }
	cpu.opcodes[0x39].instruction = func() { cpu.and(modeAbsY) }
	cpu.opcodes[0x21].instruction = func() { cpu.and(modeXInd) }
	cpu.opcodes[0x31].instruction = func() { cpu.and(modeIndY) }

	// ASL
	cpu.opcodes[0x0a].instruction = func() { cpu.asl(modeA) }
	cpu.opcodes[0x06].instruction = func() { cpu.asl(modeZpg) }
	cpu.opcodes[0x16].instruction = func() { cpu.asl(modeZpgX) }
	cpu.opcodes[0x0e].instruction = func() { cpu.asl(modeAbs) }
	cpu.opcodes[0x1e].instruction = func() { cpu.asl(modeAbsX) }

	// BCC, BCS, BEQ
	cpu.opcodes[0x90].instruction = cpu.bcc
	cpu.opcodes[0xb0].instruction = cpu.bcs
	cpu.opcodes[0xf0].instruction = cpu.beq

	// BIT
	cpu.opcodes[0x24].instruction = func() { cpu.bit(modeZpg) }
	cpu.opcodes[0x2c].instruction = func() { cpu.bit(modeAbs) }

	// BMI, BNE, BPL
	cpu.opcodes[0x30].instruction = cpu.bmi
	cpu.opcodes[0xd0].instruction = cpu.bne
	cpu.opcodes[0x10].instruction = cpu.bpl

	// BRK, BVC, BVS, CLC, CLD, CLI, CLV
	cpu.opcodes[0x00].instruction = cpu.brk
	cpu.opcodes[0x50].instruction = cpu.bvc
	cpu.opcodes[0x70].instruction = cpu.bvs
	cpu.opcodes[0x18].instruction = cpu.clc
	cpu.opcodes[0xd8].instruction = cpu.cld
	cpu.opcodes[0x58].instruction = cpu.cli
	cpu.opcodes[0xb8].instruction = cpu.clv

	// CMP
	cpu.opcodes[0xc9].instruction = func() { cpu.cmp(modeImm) }
	cpu.opcodes[0xc5].instruction = func() { cpu.cmp(modeZpg) }
	cpu.opcodes[0xd5].instruction = func() { cpu.cmp(modeZpgX) }
	cpu.opcodes[0xcd].instruction = func() { cpu.cmp(modeAbs) }
	cpu.opcodes[0xdd].instruction = func() { cpu.cmp(modeAbsX) }
	cpu.opcodes[0xd9].instruction = func() { cpu.cmp(modeAbsY) }
	cpu.opcodes[0xc1].instruction = func() { cpu.cmp(modeXInd) }
	cpu.opcodes[0xd1].instruction = func() { cpu.cmp(modeIndY) }

	// CPX
	cpu.opcodes[0xe0].instruction = func() { cpu.cpx(modeImm) }
	cpu.opcodes[0xe4].instruction = func() { cpu.cpx(modeZpg) }
	cpu.opcodes[0xec].instruction = func() { cpu.cpx(modeAbs) }

	// CPY
	cpu.opcodes[0xc0].instruction = func() { cpu.cpy(modeImm) }
	cpu.opcodes[0xc4].instruction = func() { cpu.cpy(modeZpg) }
	cpu.opcodes[0xcc].instruction = func() { cpu.cpy(modeAbs) }

	// DCP
	cpu.opcodes[0xc7].instruction = func() { cpu.dcp(modeZpg) }
	cpu.opcodes[0xd7].instruction = func() { cpu.dcp(modeZpgX) }
	cpu.opcodes[0xcf].instruction = func() { cpu.dcp(modeAbs) }
	cpu.opcodes[0xdf].instruction = func() { cpu.dcp(modeAbsX) }
	cpu.opcodes[0xdb].instruction = func() { cpu.dcp(modeAbsY) }
	cpu.opcodes[0xc3].instruction = func() { cpu.dcp(modeXInd) }
	cpu.opcodes[0xd3].instruction = func() { cpu.dcp(modeIndY) }

	// DEC
	cpu.opcodes[0xc6].instruction = func() { cpu.dec(modeZpg) }
	cpu.opcodes[0xd6].instruction = func() { cpu.dec(modeZpgX) }
	cpu.opcodes[0xce].instruction = func() { cpu.dec(modeAbs) }
	cpu.opcodes[0xde].instruction = func() { cpu.dec(modeAbsX) }

	// DEX, DEY
	cpu.opcodes[0xca].instruction = cpu.dex
	cpu.opcodes[0x88].instruction = cpu.dey

	// DOP
	cpu.opcodes[0x04].instruction = func() { cpu.dop(modeZpg) }
	cpu.opcodes[0x14].instruction = func() { cpu.dop(modeZpgX) }
	cpu.opcodes[0x34].instruction = func() { cpu.dop(modeZpgX) }
	cpu.opcodes[0x44].instruction = func() { cpu.dop(modeZpg) }
	cpu.opcodes[0x54].instruction = func() { cpu.dop(modeZpgX) }
	cpu.opcodes[0x64].instruction = func() { cpu.dop(modeZpg) }
	cpu.opcodes[0x74].instruction = func() { cpu.dop(modeZpgX) }
	cpu.opcodes[0x80].instruction = func() { cpu.dop(modeImm) }
	cpu.opcodes[0x82].instruction = func() { cpu.dop(modeImm) }
	cpu.opcodes[0x89].instruction = func() { cpu.dop(modeImm) }
	cpu.opcodes[0xc2].instruction = func() { cpu.dop(modeImm) }
	cpu.opcodes[0xd4].instruction = func() { cpu.dop(modeZpgX) }
	cpu.opcodes[0xe2].instruction = func() { cpu.dop(modeImm) }
	cpu.opcodes[0xf4].instruction = func() { cpu.dop(modeZpgX) }

	// EOR
	cpu.opcodes[0x49].instruction = func() { cpu.eor(modeImm) }
	cpu.opcodes[0x45].instruction = func() { cpu.eor(modeZpg) }
	cpu.opcodes[0x55].instruction = func() { cpu.eor(modeZpgX) }
	cpu.opcodes[0x4d].instruction = func() { cpu.eor(modeAbs) }
	cpu.opcodes[0x5d].instruction = func() { cpu.eor(modeAbsX) }
	cpu.opcodes[0x59].instruction = func() { cpu.eor(modeAbsY) }
	cpu.opcodes[0x41].instruction = func() { cpu.eor(modeXInd) }
	cpu.opcodes[0x51].instruction = func() { cpu.eor(modeIndY) }

	// INC
	cpu.opcodes[0xe6].instruction = func() { cpu.inc(modeZpg) }
	cpu.opcodes[0xf6].instruction = func() { cpu.inc(modeZpgX) }
	cpu.opcodes[0xee].instruction = func() { cpu.inc(modeAbs) }
	cpu.opcodes[0xfe].instruction = func() { cpu.inc(modeAbsX) }

	// INX, INY
	cpu.opcodes[0xe8].instruction = cpu.inx
	cpu.opcodes[0xc8].instruction = cpu.iny

	// ISC
	cpu.opcodes[0xe7].instruction = func() { cpu.isc(modeZpg) }
	cpu.opcodes[0xf7].instruction = func() { cpu.isc(modeZpgX) }
	cpu.opcodes[0xef].instruction = func() { cpu.isc(modeAbs) }
	cpu.opcodes[0xff].instruction = func() { cpu.isc(modeAbsX) }
	cpu.opcodes[0xfb].instruction = func() { cpu.isc(modeAbsY) }
	cpu.opcodes[0xe3].instruction = func() { cpu.isc(modeXInd) }
	cpu.opcodes[0xf3].instruction = func() { cpu.isc(modeIndY) }

	// JMP
	cpu.opcodes[0x4c].instruction = func() { cpu.jmp(modeAbs) }
	cpu.opcodes[0x6c].instruction = func() { cpu.jmp(modeInd) }

	// JSR
	cpu.opcodes[0x20].instruction = cpu.jsr

	// LAX
	cpu.opcodes[0xa7].instruction = func() { cpu.lax(modeZpg) }
	cpu.opcodes[0xb7].instruction = func() { cpu.lax(modeZpgY) }
	cpu.opcodes[0xaf].instruction = func() { cpu.lax(modeAbs) }
	cpu.opcodes[0xbf].instruction = func() { cpu.lax(modeAbsY) }
	cpu.opcodes[0xa3].instruction = func() { cpu.lax(modeXInd) }
	cpu.opcodes[0xb3].instruction = func() { cpu.lax(modeIndY) }

	// LDA
	cpu.opcodes[0xa9].instruction = func() { cpu.lda(modeImm) }
	cpu.opcodes[0xa5].instruction = func() { cpu.lda(modeZpg) }
	cpu.opcodes[0xb5].instruction = func() { cpu.lda(modeZpgX) }
	cpu.opcodes[0xad].instruction = func() { cpu.lda(modeAbs) }
	cpu.opcodes[0xbd].instruction = func() { cpu.lda(modeAbsX) }
	cpu.opcodes[0xb9].instruction = func() { cpu.lda(modeAbsY) }
	cpu.opcodes[0xa1].instruction = func() { cpu.lda(modeXInd) }
	cpu.opcodes[0xb1].instruction = func() { cpu.lda(modeIndY) }

	// LDX
	cpu.opcodes[0xa2].instruction = func() { cpu.ldx(modeImm) }
	cpu.opcodes[0xa6].instruction = func() { cpu.ldx(modeZpg) }
	cpu.opcodes[0xb6].instruction = func() { cpu.ldx(modeZpgY) }
	cpu.opcodes[0xae].instruction = func() { cpu.ldx(modeAbs) }
	cpu.opcodes[0xbe].instruction = func() { cpu.ldx(modeAbsY) }

	// LDY
	cpu.opcodes[0xa0].instruction = func() { cpu.ldy(modeImm) }
	cpu.opcodes[0xa4].instruction = func() { cpu.ldy(modeZpg) }
	cpu.opcodes[0xb4].instruction = func() { cpu.ldy(modeZpgX) }
	cpu.opcodes[0xac].instruction = func() { cpu.ldy(modeAbs) }
	cpu.opcodes[0xbc].instruction = func() { cpu.ldy(modeAbsX) }

	// LSR
	cpu.opcodes[0x4a].instruction = func() { cpu.lsr(modeA) }
	cpu.opcodes[0x46].instruction = func() { cpu.lsr(modeZpg) }
	cpu.opcodes[0x56].instruction = func() { cpu.lsr(modeZpgX) }
	cpu.opcodes[0x4e].instruction = func() { cpu.lsr(modeAbs) }
	cpu.opcodes[0x5e].instruction = func() { cpu.lsr(modeAbsX) }

	// NOP
	cpu.opcodes[0xea].instruction = cpu.nop
	cpu.opcodes[0x1a].instruction = cpu.nop
	cpu.opcodes[0x3a].instruction = cpu.nop
	cpu.opcodes[0x5a].instruction = cpu.nop
	cpu.opcodes[0x7a].instruction = cpu.nop
	cpu.opcodes[0xda].instruction = cpu.nop
	cpu.opcodes[0xfa].instruction = cpu.nop

	// ORA
	cpu.opcodes[0x09].instruction = func() { cpu.ora(modeImm) }
	cpu.opcodes[0x05].instruction = func() { cpu.ora(modeZpg) }
	cpu.opcodes[0x15].instruction = func() { cpu.ora(modeZpgX) }
	cpu.opcodes[0x0d].instruction = func() { cpu.ora(modeAbs) }
	cpu.opcodes[0x1d].instruction = func() { cpu.ora(modeAbsX) }
	cpu.opcodes[0x19].instruction = func() { cpu.ora(modeAbsY) }
	cpu.opcodes[0x01].instruction = func() { cpu.ora(modeXInd) }
	cpu.opcodes[0x11].instruction = func() { cpu.ora(modeIndY) }

	// PHA, PHP, PLA, PLP
	cpu.opcodes[0x48].instruction = cpu.pha
	cpu.opcodes[0x08].instruction = cpu.php
	cpu.opcodes[0x68].instruction = cpu.pla
	cpu.opcodes[0x28].instruction = cpu.plp

	// RLA
	cpu.opcodes[0x27].instruction = func() { cpu.rla(modeZpg) }
	cpu.opcodes[0x37].instruction = func() { cpu.rla(modeZpgX) }
	cpu.opcodes[0x2f].instruction = func() { cpu.rla(modeAbs) }
	cpu.opcodes[0x3f].instruction = func() { cpu.rla(modeAbsX) }
	cpu.opcodes[0x3b].instruction = func() { cpu.rla(modeAbsY) }
	cpu.opcodes[0x23].instruction = func() { cpu.rla(modeXInd) }
	cpu.opcodes[0x33].instruction = func() { cpu.rla(modeIndY) }

	// ROL
	cpu.opcodes[0x2a].instruction = func() { cpu.rol(modeA) }
	cpu.opcodes[0x26].instruction = func() { cpu.rol(modeZpg) }
	cpu.opcodes[0x36].instruction = func() { cpu.rol(modeZpgX) }
	cpu.opcodes[0x2e].instruction = func() { cpu.rol(modeAbs) }
	cpu.opcodes[0x3e].instruction = func() { cpu.rol(modeAbsX) }

	// ROR
	cpu.opcodes[0x6a].instruction = func() { cpu.ror(modeA) }
	cpu.opcodes[0x66].instruction = func() { cpu.ror(modeZpg) }
	cpu.opcodes[0x76].instruction = func() { cpu.ror(modeZpgX) }
	cpu.opcodes[0x6e].instruction = func() { cpu.ror(modeAbs) }
	cpu.opcodes[0x7e].instruction = func() { cpu.ror(modeAbsX) }

	// RRA
	cpu.opcodes[0x67].instruction = func() { cpu.rra(modeZpg) }
	cpu.opcodes[0x77].instruction = func() { cpu.rra(modeZpgX) }
	cpu.opcodes[0x6f].instruction = func() { cpu.rra(modeAbs) }
	cpu.opcodes[0x7f].instruction = func() { cpu.rra(modeAbsX) }
	cpu.opcodes[0x7b].instruction = func() { cpu.rra(modeAbsY) }
	cpu.opcodes[0x63].instruction = func() { cpu.rra(modeXInd) }
	cpu.opcodes[0x73].instruction = func() { cpu.rra(modeIndY) }

	// RTI, RTS
	cpu.opcodes[0x40].instruction = cpu.rti
	cpu.opcodes[0x60].instruction = cpu.rts

	// SBC
	cpu.opcodes[0xe9].instruction = func() { cpu.sbc(modeImm) }
	cpu.opcodes[0xeb].instruction = func() { cpu.sbc(modeImm) }
	cpu.opcodes[0xe5].instruction = func() { cpu.sbc(modeZpg) }
	cpu.opcodes[0xf5].instruction = func() { cpu.sbc(modeZpgX) }
	cpu.opcodes[0xed].instruction = func() { cpu.sbc(modeAbs) }
	cpu.opcodes[0xfd].instruction = func() { cpu.sbc(modeAbsX) }
	cpu.opcodes[0xf9].instruction = func() { cpu.sbc(modeAbsY) }
	cpu.opcodes[0xe1].instruction = func() { cpu.sbc(modeXInd) }
	cpu.opcodes[0xf1].instruction = func() { cpu.sbc(modeIndY) }

	// SEC, SED, SEI
	cpu.opcodes[0x38].instruction = cpu.sec
	cpu.opcodes[0xf8].instruction = cpu.sed
	cpu.opcodes[0x78].instruction = cpu.sei

	// SLO
	cpu.opcodes[0x07].instruction = func() { cpu.slo(modeZpg) }
	cpu.opcodes[0x17].instruction = func() { cpu.slo(modeZpgX) }
	cpu.opcodes[0x0f].instruction = func() { cpu.slo(modeAbs) }
	cpu.opcodes[0x1f].instruction = func() { cpu.slo(modeAbsX) }
	cpu.opcodes[0x1b].instruction = func() { cpu.slo(modeAbsY) }
	cpu.opcodes[0x03].instruction = func() { cpu.slo(modeXInd) }
	cpu.opcodes[0x13].instruction = func() { cpu.slo(modeIndY) }

	// SRE
	cpu.opcodes[0x47].instruction = func() { cpu.sre(modeZpg) }
	cpu.opcodes[0x57].instruction = func() { cpu.sre(modeZpgX) }
	cpu.opcodes[0x4f].instruction = func() { cpu.sre(modeAbs) }
	cpu.opcodes[0x5f].instruction = func() { cpu.sre(modeAbsX) }
	cpu.opcodes[0x5b].instruction = func() { cpu.sre(modeAbsY) }
	cpu.opcodes[0x43].instruction = func() { cpu.sre(modeXInd) }
	cpu.opcodes[0x53].instruction = func() { cpu.sre(modeIndY) }

	// STA
	cpu.opcodes[0x85].instruction = func() { cpu.sta(modeZpg) }
	cpu.opcodes[0x95].instruction = func() { cpu.sta(modeZpgX) }
	cpu.opcodes[0x8d].instruction = func() { cpu.sta(modeAbs) }
	cpu.opcodes[0x9d].instruction = func() { cpu.sta(modeAbsX) }
	cpu.opcodes[0x99].instruction = func() { cpu.sta(modeAbsY) }
	cpu.opcodes[0x81].instruction = func() { cpu.sta(modeXInd) }
	cpu.opcodes[0x91].instruction = func() { cpu.sta(modeIndY) }

	// STX
	cpu.opcodes[0x86].instruction = func() { cpu.stx(modeZpg) }
	cpu.opcodes[0x96].instruction = func() { cpu.stx(modeZpgY) }
	cpu.opcodes[0x8e].instruction = func() { cpu.stx(modeAbs) }

	// STY
	cpu.opcodes[0x84].instruction = func() { cpu.sty(modeZpg) }
	cpu.opcodes[0x94].instruction = func() { cpu.sty(modeZpgX) }
	cpu.opcodes[0x8c].instruction = func() { cpu.sty(modeAbs) }

	// TAX, TAY
	cpu.opcodes[0xaa].instruction = cpu.tax
	cpu.opcodes[0xa8].instruction = cpu.tay

	// TOP
	cpu.opcodes[0x0c].instruction = func() { cpu.top(modeAbs) }
	cpu.opcodes[0x1c].instruction = func() { cpu.top(modeAbsX) }
	cpu.opcodes[0x3c].instruction = func() { cpu.top(modeAbsX) }
	cpu.opcodes[0x5c].instruction = func() { cpu.top(modeAbsX) }
	cpu.opcodes[0x7c].instruction = func() { cpu.top(modeAbsX) }
	cpu.opcodes[0xdc].instruction = func() { cpu.top(modeAbsX) }
	cpu.opcodes[0xfc].instruction = func() { cpu.top(modeAbsX) }

	// TSX, TXA, TXS, TYA
	cpu.opcodes[0xba].instruction = cpu.tsx
	cpu.opcodes[0x8a].instruction = cpu.txa
	cpu.opcodes[0x9a].instruction = cpu.txs
	cpu.opcodes[0x98].instruction = cpu.tya

	cycles := []int{
		7, 6, 0, 8, 3, 3, 5, 5, 3, 2, 2, 0, 4, 4, 6, 6,
		2, 5, 0, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
		6, 6, 0, 8, 3, 3, 5, 5, 4, 2, 2, 0, 4, 4, 6, 6,
		2, 5, 0, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
		6, 6, 0, 8, 3, 3, 5, 5, 3, 2, 2, 0, 3, 4, 6, 6,
		2, 5, 0, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
		6, 6, 0, 8, 3, 3, 5, 5, 4, 2, 2, 0, 5, 4, 6, 6,
		2, 5, 0, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
		2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 0, 4, 4, 4, 4,
		2, 6, 0, 0, 4, 4, 4, 4, 2, 5, 2, 0, 0, 5, 0, 0,
		2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 0, 4, 4, 4, 4,
		2, 5, 0, 5, 4, 4, 4, 4, 2, 4, 2, 0, 4, 4, 4, 4,
		2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 0, 4, 4, 6, 6,
		2, 5, 0, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
		2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
		2, 5, 0, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
	}
	for i := 0; i < 256; i++ {
		cpu.opcodes[i].cycles = cycles[i]
	}
}
