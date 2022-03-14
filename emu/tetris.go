//go:build tetris
// +build tetris

package main

import (
	"embed"
	"log"

	"github.com/hajimehoshi/ebiten/v2"
	"github.com/hajimehoshi/ebiten/v2/audio"
	"github.com/hajimehoshi/ebiten/v2/inpututil"
)

const (
	MASTER_CLOCK            float32 = 14318181
	CPU_CLOCK               float32 = MASTER_CLOCK / 8
	CPU_CLOCKS_PER_FRAME    float32 = CPU_CLOCK / 60
	CPU_CLOCKS_PER_SCANLINE float32 = CPU_CLOCKS_PER_FRAME / 256
	PPU_CLOCK               float32 = MASTER_CLOCK / 2
	APU_CLOCK               float32 = MASTER_CLOCK / 8

	GAME_SCREEN_WIDTH  = 336
	GAME_SCREEN_HEIGHT = 240
	TILE_WIDTH         = GAME_SCREEN_WIDTH / 8
	TILE_HEIGHT        = GAME_SCREEN_HEIGHT / 8

	TILEMAP_SCREEN_WIDTH  = 512
	TILEMAP_SCREEN_HEIGHT = 256
	TILEMAP_TILE_WIDTH    = TILEMAP_SCREEN_WIDTH / 8
	TILEMAP_TILE_HEIGHT   = TILEMAP_SCREEN_HEIGHT / 8

	DISPLAY_WIDTH  = GAME_SCREEN_WIDTH
	DISPLAY_HEIGHT = GAME_SCREEN_HEIGHT
	// DISPLAY_WIDTH  = 512 // to cover both screens
	// DISPLAY_HEIGHT = 496 // both heights combined
)

type Game struct {
	cpu           *m6502
	apu           *APU
	pokey1        *Pokey
	pokey2        *Pokey
	rom           []byte
	tiles         []byte
	slapstic_bank uint16 // 0(or 2) and 1 (or 3) (selects 0-0x3fff, or 0x0x4000-0x7fff)
	eeprom        *e2804
	slapstic      *Slapstic

	wram         [0x1000]uint8 // 0x0000-0x0fff
	vram         [0x1000]uint8 // 0x1000-0x1fff
	palram       [0x100]uint8  // 0x2000-0x20ff
	palettes_rgb [0x100]Palette
	// eeprom - 0x2400-0x25ff
	// pokey1 - 0x2800-0x280f
	// pokey2 - 0x2810-0x281f
	// watchdog - 0x3000
	// eeprom - 0x3400
	// todo: irq ack - 0x3800
	// todo: coin count - 0x3c00
	// slapstic bank - 0x4000-0x7fff

	int_line            bool
	int_risen           bool
	clocks_for_next_irq int

	display     []byte
	audioCtx    *audio.Context
	audioPlayer *audio.Player

	tilemap_pals [TILEMAP_TILE_WIDTH * TILEMAP_TILE_HEIGHT][16]Palette

	// in 0
	COIN2   uint8 // 0x01
	COIN1   uint8 // 0x02
	VBLANK  uint8 // 0x40
	SERVICE uint8 // 0x80

	// in 1
	P1_BTN   uint8 // 0x01
	P1_DOWN  uint8 // 0x02
	P1_RIGHT uint8 // 0x04
	P1_LEFT  uint8 // 0x08
	P2_BTN   uint8 // 0x10
	P2_DOWN  uint8 // 0x20
	P2_RIGHT uint8 // 0x40
	P2_LEFT  uint8 // 0x80

	started bool
}

func (g *Game) reset() {
	g.cpu.init_cpu()
	g.slapstic.init_slapstic()
	g.eeprom.reset_eeprom()
	for i := 0; i < len(g.wram); i++ {
		g.wram[i] = 0
	}
	for i := 0; i < len(g.vram); i++ {
		g.vram[i] = 0
	}
	for i := 0; i < len(g.palram); i++ {
		g.palram[i] = 0
	}
	g.int_line = false
	g.int_risen = false
	g.clocks_for_next_irq = clocks_per_irq_line[0]
	for i := 0; i < len(g.display); i++ {
		g.display[i] = 0
	}
}

type Palette struct {
	red   uint8
	blue  uint8
	green uint8
}

type APU struct {
	audioBuffer []byte
	clockTimer  float32
}

func (apu *APU) Read(p []byte) (n int, err error) {
	// log.Println(len(apu.audioBuffer))
	if len(apu.audioBuffer) > 0 {
		n = copy(p, apu.audioBuffer)
		apu.audioBuffer = apu.audioBuffer[n:]
		return n, nil
	}
	emptyBuf := make([]byte, len(p))
	n = copy(p, emptyBuf)
	return n, nil
}

func (apu *APU) add_cycles(cycles int) {
	apu.clockTimer += 44100 * float32(cycles)
	if apu.clockTimer >= APU_CLOCK {
		apu.clockTimer -= APU_CLOCK
		pokey1sample := game.pokey1.get_sample()
		pokey2sample := game.pokey2.get_sample()
		avg := (pokey1sample + pokey2sample) / 2
		hb := byte(avg >> 8)
		lb := byte(avg)
		apu.audioBuffer = append(apu.audioBuffer, lb, hb, lb, hb)
	}

	for i := 0; i < cycles; i++ {
		game.pokey1.dec_timers()
		game.pokey2.dec_timers()
	}
}

//go:embed rom.bin
//go:embed tiles.bin
var f embed.FS
var game Game
var clocks_per_irq_line [8]int
var isJS bool

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func (g *Game) get_scanline() int {
	return int(float32(g.cpu.cycles) / CPU_CLOCKS_PER_SCANLINE)
}

func port_hold_handler(key ebiten.Key, varChanged *uint8, pressVal uint8, releaseVal uint8) {
	if inpututil.IsKeyJustPressed(key) {
		*varChanged = pressVal
	} else if inpututil.IsKeyJustReleased(key) {
		*varChanged = releaseVal
	}
}

func port_toggle_handler(key ebiten.Key, varChanged *uint8, val1 uint8, val2 uint8) {
	if inpututil.IsKeyJustPressed(key) {
		if *varChanged == val1 {
			*varChanged = val2
		} else {
			*varChanged = val1
		}
	}
}

func (g *Game) poll_keys() {
	port_hold_handler(ebiten.KeyZ, &g.COIN1, 0x02, 0)
	port_hold_handler(ebiten.KeyX, &g.COIN2, 0x01, 0)

	port_hold_handler(ebiten.KeyW, &g.P1_BTN, 0x01, 0)
	port_hold_handler(ebiten.KeyS, &g.P1_DOWN, 0x02, 0)
	port_hold_handler(ebiten.KeyD, &g.P1_RIGHT, 0x04, 0)
	port_hold_handler(ebiten.KeyA, &g.P1_LEFT, 0x08, 0)
	port_hold_handler(ebiten.KeyI, &g.P2_BTN, 0x10, 0)
	port_hold_handler(ebiten.KeyK, &g.P2_DOWN, 0x20, 0)
	port_hold_handler(ebiten.KeyL, &g.P2_RIGHT, 0x40, 0)
	port_hold_handler(ebiten.KeyJ, &g.P2_LEFT, 0x80, 0)

	if !isJS {
		port_toggle_handler(ebiten.KeyC, &g.SERVICE, 0, 0x80)
	}
}

func (g *Game) curr_irq_scanline() int {
	var ret int
	for {
		if g.cpu.cycles < clocks_per_irq_line[ret] {
			return ret - 1
		}
		ret++
		if ret == 8 {
			return 7
		}
	}
}

func (g *Game) exec_vm() {
	cpu := g.cpu
	g.clocks_for_next_irq = clocks_per_irq_line[0]
	cpu.cycles = 0
	for {
		prevCycles := cpu.cycles
		cpu.run_opcode()

		if g.int_risen && cpu.is_flag_clear(flagI) {
			g.int_risen = false
			cpu.run_irq()
		}

		// Frequency timer
		cyclesDone := cpu.cycles - prevCycles
		g.apu.add_cycles(cyclesDone)

		// check irqs
		if g.clocks_for_next_irq != 0 && cpu.cycles > int(g.clocks_for_next_irq) {
			curr_irq_scanline := g.curr_irq_scanline()
			next_int_line := (curr_irq_scanline & 1) != 0
			if !g.int_line && next_int_line {
				g.int_risen = true
			}
			g.int_line = next_int_line
			if curr_irq_scanline == 7 {
				g.clocks_for_next_irq = 0
			} else {
				g.clocks_for_next_irq = clocks_per_irq_line[curr_irq_scanline+1]
			}
		}
		// check for frame end
		if g.clocks_for_next_irq == 0 && float32(cpu.cycles) > CPU_CLOCKS_PER_FRAME {
			break
		}
	}
}

func (g *Game) plot_pixel(x int, y int, nybble byte, pals [16]Palette) {
	displayOffs := (y*DISPLAY_WIDTH + x) * 4

	comps := pals[nybble]
	g.display[displayOffs] = comps.red
	g.display[displayOffs+1] = comps.green
	g.display[displayOffs+2] = comps.blue
	g.display[displayOffs+3] = 0xff
}

func (g *Game) plot_tile_row(x int, y int, b0 byte, b1 byte, b2 byte, b3 byte, pals [16]Palette) {
	g.plot_pixel(x, y, b0>>4, pals)
	g.plot_pixel(x+1, y, b0&0xf, pals)
	g.plot_pixel(x+2, y, b1>>4, pals)
	g.plot_pixel(x+3, y, b1&0xf, pals)
	g.plot_pixel(x+4, y, b2>>4, pals)
	g.plot_pixel(x+5, y, b2&0xf, pals)
	g.plot_pixel(x+6, y, b3>>4, pals)
	g.plot_pixel(x+7, y, b3&0xf, pals)
}

func (g *Game) update_display() {
	// reset tilemap pals
	var defaultPals [16]Palette
	for i := 0; i < 16; i++ {
		palVal := uint8(i * 16)
		defaultPals[i].red = palVal
		defaultPals[i].green = palVal
		defaultPals[i].blue = palVal
	}

	for i := 0; i < len(g.tilemap_pals); i++ {
		g.tilemap_pals[i] = defaultPals
	}

	// calculate palettes
	for i, col := range g.palram {
		g.palettes_rgb[i].red = (col >> 5) * 36
		g.palettes_rgb[i].green = ((col >> 2) & 7) * 36
		g.palettes_rgb[i].blue = (col & 3) * 85
	}

	var currPals [16]Palette
	// update game display
	for row := 0; row < TILE_HEIGHT; row++ {
		for col := 0; col < TILE_WIDTH; col++ {
			offs := (row*64 + col) * 2
			tileIdxLo := g.vram[offs]
			tileAttr := g.vram[offs+1]
			tileIdx := uint16(tileIdxLo) + ((uint16(tileAttr) & 7) << 8)

			palette := (tileAttr & 0xf0) >> 4
			var i uint8
			for i = 0; i < 16; i++ {
				currPals[i] = g.palettes_rgb[palette*16+i]
			}
			g.tilemap_pals[tileIdx] = currPals

			tileDataOffs := tileIdx * 0x20
			baseX := col * 8
			baseY := row * 8
			for yi := 0; yi < 8; yi++ {
				g.plot_tile_row(
					baseX, baseY+yi,
					g.tiles[tileDataOffs], g.tiles[tileDataOffs+1],
					g.tiles[tileDataOffs+2], g.tiles[tileDataOffs+3], currPals)
				tileDataOffs += 4
			}
		}
	}

	// update tilemap display
	if DISPLAY_WIDTH == 512 {
		for row := 0; row < TILEMAP_TILE_HEIGHT; row++ {
			for col := 0; col < TILEMAP_TILE_WIDTH; col++ {
				tileIdx := row*TILEMAP_TILE_WIDTH + col
				pals := g.tilemap_pals[tileIdx]

				tileDataOffs := tileIdx * 0x20
				baseX := col * 8
				baseY := row*8 + GAME_SCREEN_HEIGHT
				for yi := 0; yi < 8; yi++ {
					g.plot_tile_row(
						baseX, baseY+yi,
						g.tiles[tileDataOffs], g.tiles[tileDataOffs+1],
						g.tiles[tileDataOffs+2], g.tiles[tileDataOffs+3], pals)
					tileDataOffs += 4
				}
			}
		}
	}
}

func (g *Game) Update() error {
	g.poll_keys()
	if g.started {
		g.exec_vm()
	}
	g.update_display()

	if !isJS {
		if inpututil.IsKeyJustPressed(ebiten.KeyBackspace) {
			g.reset()
		}
		if inpututil.IsKeyJustPressed(ebiten.KeySpace) {
			g.StartEmu()
		}
	}

	return nil
}

func (g *Game) Draw(screen *ebiten.Image) {
	screen.ReplacePixels(g.display)
}

func (g *Game) Layout(outsideWidth, outsideHeight int) (int, int) {
	return DISPLAY_WIDTH, DISPLAY_HEIGHT
}

func (g *Game) StartEmu() {
	if g.started {
		return
	}
	g.started = true
	g.audioPlayer.Play()
}

func main() {
	// Get roms
	rom, err := f.ReadFile("rom.bin")
	check(err)
	tiles, err := f.ReadFile("tiles.bin")
	check(err)

	// Initialize components
	eeprom := e2804{}
	eeprom.init_eeprom()

	slapstic := Slapstic{
		rom: rom,
	}
	slapstic.init_slapstic()

	cpu := m6502{}
	cpu.init_ops()

	pokey1 := NewPokey(1)
	pokey1.all_port_cb = func() uint8 {
		game.VBLANK = 0x40
		if float32(cpu.cycles) >= 240*CPU_CLOCKS_PER_SCANLINE {
			game.VBLANK = 0
		}
		return game.VBLANK | game.SERVICE | game.COIN2 | game.COIN1
	}
	pokey2 := NewPokey(2)
	pokey2.all_port_cb = func() uint8 {
		return game.P1_BTN | game.P1_DOWN | game.P1_RIGHT | game.P1_LEFT | game.P2_BTN | game.P2_DOWN | game.P2_RIGHT | game.P2_LEFT
	}

	cpu.read_cb = func(addr uint16) uint8 {
		if addr < 0x1000 {
			return game.wram[addr]
		}
		if addr < 0x2000 {
			return game.vram[addr%0x1000]
		}
		if addr < 0x2100 {
			return game.palram[addr%0x100]
		}
		if addr >= 0x2400 && addr < 0x2600 {
			return eeprom.read(addr - 0x2400)
		}
		if addr >= 0x2800 && addr < 0x2810 {
			return pokey1.read_port(uint8(addr))
		}
		if addr >= 0x2810 && addr < 0x2820 {
			return pokey2.read_port(uint8(addr - 0x10))
		}
		if addr >= 0x4000 && addr < 0x8000 {
			return slapstic.read(addr - 0x4000)
		}
		if addr >= 0x8000 {
			return game.rom[addr]
		}
		log.Fatalf("Reading from address %x", addr)
		return 0
	}
	cpu.write_cb = func(addr uint16, val uint8) {
		if addr < 0x1000 {
			game.wram[addr] = val
		} else if addr < 0x2000 {
			game.vram[addr%0x1000] = val
		} else if addr < 0x2100 {
			game.palram[addr%0x100] = val
		} else if addr >= 0x2400 && addr < 0x2600 {
			eeprom.write(addr-0x2400, val)
		} else if addr >= 0x2800 && addr < 0x2810 {
			pokey1.write_port(uint8(addr), val)
		} else if addr >= 0x2810 && addr < 0x2820 {
			pokey2.write_port(uint8(addr-0x10), val)
		} else if addr == 0x3000 {
			// watchdog
			return
		} else if addr == 0x3400 {
			eeprom.locked = false
		} else if addr == 0x3800 {
			game.int_line = false
		} else if addr == 0x3c00 {
			// coin counter
			return
		} else {
			log.Fatalf("Writing to address %x", addr)
		}
	}

	apu := APU{}

	game = Game{
		cpu:      &cpu,
		apu:      &apu,
		pokey1:   pokey1,
		pokey2:   pokey2,
		rom:      rom,
		tiles:    tiles,
		eeprom:   &eeprom,
		slapstic: &slapstic,
		display:  make([]byte, DISPLAY_WIDTH*DISPLAY_HEIGHT*4),
		audioCtx: audio.NewContext(44100),
	}
	cpu.init_cpu()

	game.audioPlayer, err = audio.NewPlayer(game.audioCtx, &apu)
	check(err)

	for i := 0; i < 8; i++ {
		clocks_per_irq_line[i] = int(float32(i*32+16) * CPU_CLOCKS_PER_SCANLINE)
	}
	game.clocks_for_next_irq = clocks_per_irq_line[0]

	// start machine
	if DISPLAY_WIDTH == 512 {
		ebiten.SetWindowSize(DISPLAY_WIDTH*1.5, DISPLAY_HEIGHT*1.5)
	} else {
		ebiten.SetWindowSize(DISPLAY_WIDTH*3, DISPLAY_HEIGHT*3)
	}
	ebiten.SetWindowTitle("Tetris")

	if err := ebiten.RunGame(&game); err != nil {
		panic(err)
	}
}
