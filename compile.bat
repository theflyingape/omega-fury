@echo on

ca65.exe --cpu 6502 -t vic20 --listing --include-dir . vic-sss4.s
ca65.exe --cpu 6502 -t vic20 --listing --include-dir . -o omega-fury.o basic-16k.s
ld65.exe -C basic-16k.cfg -Ln omega-fury.sym -m omega-fury.map -o omega-fury.prg omega-fury.o vic-sss4.o

pause

set SRC=%CD%
pushd %~dp0
REM cd ..\..
REM xvic -ntsc -sound -memory 16k -sound -joydev1 2 -autostart %SRC%\omega-fury.prg
REM mess -inipath etc -video d3d -newui -ramsize 24k vic20 -quik %SRC%\omega-fury.prg
popd
exit
