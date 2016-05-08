#!/bin/sh
#
set -o xtrace
ca65 --cpu 6502 -t vic20 --listing --include-dir . -o omega-fury.o basic-16k.s
ca65 --cpu 6502 -t vic20 --listing --include-dir . VIC-SSS-MMX.s
ld65 -C basic-16k.cfg -Ln omega-fury.sym -m omega-fury.map -o omega-fury.prg omega-fury.o VIC-SSS-MMX.o
set +o xtrace

echo -n "Press RETURN: " && read N

#mess -skip_gameinfo -skip_warnings -window vic20 -ram 24k -quik omega-fury.prg
xvic -ntsc -memory 16k -autostart omega-fury.prg

exit

