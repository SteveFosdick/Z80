clientZ80.rom: clientZ80.asm
	tpasm -s -l clientZ80.lst -o intel clientZ80.hex clientZ80.asm
	objcopy -I ihex -O binary clientZ80.hex clientZ80.rom
