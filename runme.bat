@echo off

:MENU
color 17
cls

echo     MESS VIC 20 MENU
echo     ================
echo N)  NTSC  +16K  +VICMon  (SYS 24576)
echo O)  NTSC  +16K  +HesMon  (autostart)
echo P)  PAL   +16K  +VICMon  (SYS 24576)
echo Q)  PAL   +16K  +HesMon  (autostart)
echo --
echo Type RUN after the READY prompt.
echo --
choice /C NOPQ /D N /T 10 /M "Which config? " /N

set CHOICE=%ERRORLEVEL%

if %CHOICE% EQU  1  mess -debug -window -video d3d -newui -skip_gameinfo -skip_warnings -rompath roms vic20 -ramsize 24k -quik omega-fury.prg
if %CHOICE% EQU  2  mess -debug -window -video d3d -newui -natural -skip_gameinfo -skip_warnings -rompath roms vic20 -ramsize 24k -cart1 roms\HesMon.a0 -quik omega-fury.prg
if %CHOICE% EQU  3  mess -debug -window -video d3d -newui -natural -skip_gameinfo -skip_warnings -rompath roms vic20pal -ramsize 24k -cart1 roms\VICMon.60 -quik omega-fury.prg
if %CHOICE% EQU  4  mess -debug -window -video d3d -newui -natural -skip_gameinfo -skip_warnings -rompath roms vic20pal -ramsize 24k -cart1 roms\HesMon.a0 -quik omega-fury.prg

exit
