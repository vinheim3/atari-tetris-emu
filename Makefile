tetris.bin:
	go build -o $@ -tags tetris ./emu

tetris.wasm:
	GOOS=js GOARCH=wasm go build -tags "tetris wasm" -o $@ ./emu