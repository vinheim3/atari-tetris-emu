//go:build tetris && wasm
// +build tetris,wasm

package main

import (
	"syscall/js"
)

func set_port_setting(port *uint8, val uint8) {
	*port = val
}

func setPort() js.Func {
	jsonFunc := js.FuncOf(func(this js.Value, args []js.Value) interface{} {
		portName := args[0].String()
		portVal := uint8(args[1].Int())
		if portName == "service" {
			set_port_setting(&game.SERVICE, portVal) // 0x80
		}
		return nil
	})
	return jsonFunc
}

func init() {
	js.Global().Set("setPort", setPort())
	isJS = true
}
