;*********************************************************************
; COMMODORE VIC 20 BOOT USING BASIC 2.0
; written by Robert Hurst <robert@hurst-ri.us>
; updated version: 30-Oct-2011
;
		.fileopt author,	"Robert Hurst"
        .fileopt comment,	"Omega Fury"
        .fileopt compiler,	"VIC 20 ASSEMBLER"

		.include "VIC-SSS-MMX.h"

;*********************************************************************
; Commodore BASIC 2.0 program
;
; LOAD "omega-fury.prg",8
; RUN
;
		.segment "BASIC"

		.word	RUN		; load address
RUN:	.word	@end	; next line link
		.word	2011	; line number
		.byte	$9E		; BASIC token: SYS
		.byte	<(MAIN / 1000 .mod 10) + $30
		.byte	<(MAIN / 100 .mod 10) + $30
		.byte	<(MAIN / 10 .mod 10) + $30
		.byte	<(MAIN / 1 .mod 10) + $30
		.byte	0		; end of line
@end:	.word	0		; end of program

;*********************************************************************
; Starting entry point for this program
;
		.segment "STARTUP"

MAIN:
		LDX $FFFC
		LDY $FFFD
		STX $0318
		STY $0319		; enable RESTORE key as RESET
		LDA MACHINE
		CMP #$05
		BNE PAL
		;
		; NTSC setup
NTSC:	LDX #<@NTSC		; load the timer low-byte latches
		LDY #>@NTSC
		LDA #$75		; raster line 234/235
		BNE IRQSYNC
@NTSC = $4243			; (261 * 65 - 2)
		;
		; PAL setup
PAL:	LDX #<@PAL		; load the timer low-byte latches
		LDY #>@PAL
		LDA #$82		; raster line 260/261
@PAL = $5686			; (312 * 71 - 2)
		;
IRQSYNC:
		CMP VIC+$04
		BNE IRQSYNC
		STX $9126		; load T1 latch low
		STY $9125		; load T1 latch high, and transfer both to T1 counter


;*********************************************************************
; Now that all the VIC startup initialization stuff is completed,
; you can append one-time startup code/data here, i.e., like a splash
; title screen.  Then, you must jump to your CODE segment, linked
; outside of VIC's internal RAM address space ...
;
RUNONCE:
		; init VIC
		INC VIC			; adjust left border to accommodate one less column
		LDA VIC+$01
		SEC
		SBC #$04		; adjust top scan line to accomodate extra row
		STA VIC+$01
		LDA #$00+$15	; set for videoram @ $1400 with 21-columns
		STA VIC+$02		; video matrix address + columns
		LDA #$B0		; $B0 = 10110000 = 24-rows + 8x8 height
		STA VIC+$03		; rows / character height
		LDA #$DF		; set video @ $1400 and char table @ $1C00
		STA VIC+$05
		LDA #%11101100
		STA VIC+$0F		; lt blue screen / magenta border
		; reset sound channels
@cont:	LDA #$00
		TAY
@snd:	STA VIC+$0A,Y
		INY
		CPY #$04
		BNE @snd
		LDA #$1F		; white & highest
		STA VIC+$0E		; auxiliary color & volume
		LDA #$80
		STA SHIFTMODE	; locked
		;
		LDX #<SPLASHCOLOR
		LDY #>SPLASHCOLOR
		STX VECTORFG
		STY VECTORFG+1
		LDX #$00
		LDY #$94
		STX VECTORBG
		STY VECTORBG+1
		LDX #$02
		LDY #$00
@fill:	LDA (VECTORFG),Y
		STA (VECTORBG),Y
		INY
		BNE @fill
		INC VECTORFG+1
		INC VECTORBG+1
		DEX
		BNE @fill
		;
@loop:	;
@key:	LDA $028D
		AND #$02		; got C= key?
		BNE @go
		JSR GETIN
		CMP #$03		; got STOP ?
		BNE @no
		JMP RESET
@no:	LDA #$FF
		STA $9122
		LDA $9111
		AND #$20		; got FIRE ?
		BEQ @go
		JMP @loop
@go:	JMP RESTART

		.segment "SPLASH"
SPLASHDATA:
		.byte	$A0,$A0,$A0,$DA,$A0,$DA,$A0,$DA,$A0,$DA,$A0,$DA,$A0,$DA,$A0,$DA,$A0,$DA,$A0,$A0,$A0
		.byte	$A0,$97,$85,$8C,$83,$8F,$8D,$85,$A0,$96,$89,$83,$A0,$B2,$B0,$A0,$95,$93,$85,$92,$A0
		.byte	$A0,$A0,$A0,$DA,$A0,$DA,$A0,$DA,$A0,$DA,$A0,$DA,$A0,$DA,$A0,$DA,$A0,$DA,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$94,$88,$89,$93,$A0,$87,$81,$8D,$85,$A0,$92,$85,$91,$95,$89,$92,$85,$93,$A0,$A0
		.byte	$A0,$81,$A0,$8A,$8F,$99,$93,$94,$89,$83,$8B,$A0,$94,$8F,$A0,$90,$8C,$81,$99,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$00,$08,$10,$18,$20,$28,$30,$38,$40,$48,$50,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$01,$09,$11,$19,$21,$29,$31,$39,$41,$49,$51,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$02,$0A,$12,$1A,$22,$2A,$32,$3A,$42,$4A,$52,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$03,$0B,$13,$1B,$23,$2B,$33,$3B,$43,$4B,$53,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$04,$0C,$14,$1C,$24,$2C,$34,$3C,$44,$4C,$54,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$05,$0D,$15,$1D,$25,$2D,$35,$3D,$45,$4D,$55,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$06,$0E,$16,$1E,$26,$2E,$36,$3E,$46,$4E,$56,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$07,$0F,$17,$1F,$27,$2F,$37,$3F,$47,$4F,$57,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$BD,$8F,$8D,$85,$87,$81,$A0,$86,$95,$92,$99,$BD,$A0,$A0,$92,$B1,$B0,$AE,$B3,$B0,$A0
		.byte	$A0,$64,$B2,$B0,$B1,$B1,$A0,$92,$8F,$82,$85,$92,$94,$A0,$88,$95,$92,$93,$94,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$8D,$81,$84,$85,$A0,$89,$8E,$A0,$95,$93,$81,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$58,$5B,$5E,$61,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.byte	$A0,$90,$92,$85,$93,$93,$59,$5C,$5F,$62,$86,$8F,$92,$A0,$94,$89,$94,$8C,$85,$A0,$A0
		.byte	$A0,$A0,$A0,$A0,$A0,$A0,$5A,$5D,$60,$63,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0,$A0
		.res	8
SPLASHCOLOR:
		.byte	$00,$00,$00,$00,$00,$01,$00,$02,$00,$03,$00,$04,$00,$05,$00,$06,$00,$07,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$07,$00,$06,$00,$05,$00,$04,$00,$03,$00,$02,$00,$01,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$00,$00
		.byte	$00,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$0F,$0F,$0F,$08,$08,$08,$08,$08,$08,$08,$08,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$0F,$0F,$0F,$08,$08,$08,$08,$08,$08,$08,$08,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$08,$08,$0A,$0A,$0E,$0E,$0E,$0E,$08,$08,$08,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$08,$08,$0A,$0A,$0E,$0E,$0E,$0E,$0A,$0A,$0A,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$08,$08,$08,$08,$08,$08,$08,$08,$08,$08,$0A,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$08,$08,$08,$08,$08,$0A,$08,$08,$08,$08,$08,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$08,$08,$08,$08,$0A,$0A,$08,$08,$08,$08,$08,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$0A,$0A,$0A,$0A,$0A,$08,$08,$08,$08,$08,$08,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$02,$07,$07,$07,$07,$07,$07,$07,$07,$07,$07,$02,$00,$03,$03,$03,$03,$03,$03,$03,$00
		.byte	$00,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$06,$00,$00
		.byte	$00,$00,$00,$00,$00,$03,$03,$03,$03,$03,$03,$03,$00,$02,$01,$06,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$0E,$0E,$0E,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$0E,$0E,$0E,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.byte	$00,$00,$00,$00,$00,$00,$0E,$0E,$0E,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
		.res	8

;*********************************************************************
; VIC Software Sprite Stack 2010 (VIC-SSS-MMX)
;
; The above BASIC loader will be overwritten by SSS upon its
; initialization (SSSINIT).  The linker will fill this reserved space
; with values used for the dual video frame buffers, play field, and
; the sprite image buffers: 4096 - 6207 ($1000 - $1BFF)
;
; $1000 - $11FF		VICFRAME1 - first video buffer
; $1200 - $13FF		VICFRAME2 - second video buffer
; $1400 - $15FF		PLAYFIELD - write-pending screen buffer
; $1600 - $17FF		PLAYCOLOR - write-pending color / dirty buffer
; these address spaces are sized upon value of SPRITEMAX (16)
; $1800 - $19FF		Sprite image buffer
; $1A00 - $1AFF		Sprite registers
;
		.segment "SSSBUF"
SSSBUF:					; this can be resized smaller as required --
		.res 49 * 8		; if all 64-chars are used by sprites, that
						; exhausts all 128 custom characters for
						; double-buffering (x2)
;
; SPRITE REGISTERS
;
SPRITEBUFH:	.res SPRITEMAX	; pointer within sprite image buffer
SPRITEBUFL:	.res SPRITEMAX	; pointer within sprite image buffer
SPRITEC1H:	.res SPRITEMAX	; pointer within sprite display character pool
SPRITEC1L:	.res SPRITEMAX	; pointer within sprite display character pool
SPRITEC2H:	.res SPRITEMAX	; pointer within sprite display character pool
SPRITEC2L:	.res SPRITEMAX	; pointer within sprite display character pool
SPRITECOL:	.res SPRITEMAX	; 4-bit VIC color code
SPRITEDEF:	.res SPRITEMAX	; function/matrix definition (see explanation below)
SPRITEH:	.res SPRITEMAX	; number of raster lines (1-16)
SPRITEIMGH:	.res SPRITEMAX	; pointer to source graphic for rendering at 0,0
SPRITEIMGL:	.res SPRITEMAX	; pointer to source graphic for rendering at 0,0
SPRITEX:	.res SPRITEMAX	; horizontal pixel coordinate, visible >0 - <SSSCLIPX
SPRITEY:	.res SPRITEMAX	; vertical pixel coordinate, visible >0 - <SSSCLIPY
SPRITEZ:	.res SPRITEMAX	; bit 0: last rendered (0 = SPRITEC1; 1 = SPRITEC2)
							; bit 3: sprite collision
							; bit 4: sprite image is clipped by a static cell
							; bit 5: background is all SSSNULLs
							; bit 6: copy/merge into alternate sprite character pool
							; bit 7: copy/shift sprite image into its buffer
;
; SPRITEDEF is a bit-structure of these characteristics:
; - height		bit 0: 0 = 8px; 1 = 16px
; - width		bit 1: 0 = 8px; 1 = 16px
; - float Y		bit 2: flag: 0=fixed cell, 1=vertical float
; - float X		bit 3: flag: 0=fixed cell, 1=horizontal float
; - ghost		bit 5: flag: 0=merge image; 1=invert image
; - collision	bit 6: flag: 0=fast copy; 1=detect
; - enabled		bit 7: flag: 0=invisible; 1=visible
;
						; SSS runtime variables:
sss:		.res 24*2	; screen row index, computed from PLAYCOLS in SSSINIT
;
; other initialized data can be appended here:
;
			.segment "RODATA"
sssALLOC:	; 8x8, 16x8, 8x16, 16x16
			.byte	8,16,16,32	; fixed:	1,2,2,4
			.byte	16,24,32,48	; float Y:	2,3,4,6
			.byte	16,32,24,48	; float X:	2,4,3,6
			.byte	32,48,48,72	; both:		4,6,6,9
sssROWS:	.byte	1,2,1,2	; fixed
			.byte	2,3,2,3	; float Y
			.byte	1,2,1,2	; float X
			.byte	2,3,2,3	; both
sssCOLS:	.byte	1,1,2,2	; fixed
			.byte	1,1,2,2	; float Y
			.byte	2,2,3,3	; float X
			.byte	2,2,3,3	; both
;
; other initialized data can be appended here:
FRAME:		.byte $00	; must be first variable -- for zeroing
JUMPMARK:	.byte $00
ALERTS:		.byte $00
BOOMED:		.byte $00
KLAXON:		.byte $00
DROPPED:	.byte $00
EMPTY:		.byte $00
FLASHING:	.byte $00
HEARTBEATS:	.byte $00
HIT:		.byte $00
PHASER:		.byte $00
PULSE:		.byte $00
SHOOTING:	.byte $00
WARPED:		.byte $00
ACTION:		.byte $00
ACTION2:	.byte $00
ACTIONS:	.byte $00
COLLISION:	.byte $00
DIRECTION:	.byte $00
FIRE:		.byte $00
TITLE:		.byte $00
BOLT:		.res 3
BOLTDIR:	.res 3
ENERGIZER:	.byte $00
HITMARK:	.byte $00
BIGHITMARK:	.byte $00
NAVI:		.byte $00
SF2010:		.byte $00
SPIN:		.byte $00
THRUST:		.byte $00
NMETHRUST:	.res 4
INERTIA:	.byte $00
NMEDIR:		.res 4
SPEED:		.byte $00
NMESPEED:	.res 4
OMEGAHP:	.byte $00	; center Omegan objective
FURYHP:		.byte $00
NMEHP:		.res 4
MINE:		.byte $00
MINES:		.res 16
MINEX:		.res 16
MINEY:		.res 16
BOOMB4:		.byte $00
BOOMER:		.byte $00
BOOMC:		.byte $00
BOOMX:		.byte $00
BOOMY:		.byte $00
BOOMDX:		.byte $00
BOOMDY:		.byte $00
NEUTRON:	.byte $00
NEUTRONDIR:	.byte $00
NME:		.res 4
NMEPLAN:	.res 4		; 0=travel, 1=pursue, 2=evade, 3=objective
NMELEN:		.res 4		; repeat PLAN for LEN turns
NMESHOT:	.res 4
NMESHOTDIR:	.res 4
NMETURN:	.res 4
LOOPX:		.byte $00
MISSION:	.byte $00
MISSIONX:	.byte $00
OBJECTIVE:	.byte $00	; number of active objectives to destroy
SHIPS:		.byte $00
SCORE:		.res 3

;*********************************************************************
; VIC Custom Graphic Characters
;
; If < 64 will be used for the software sprite stack, the remaining
; unused characters can be used for other custom graphics, beginning
; at $1C00 where "@", "A", "B", ... characters can be redefined.
;
; Do not use this as an initialized segment if you plan on linking
; this source as a future game cartridge later.  You must COPY any
; read-only data into this address space.
;
; If your data was saved from some tool in binary format, you can
; include that binary file here as:
;		.incbin "graphics.dat"
;
; else, just enter each 8x8 values here, such as:
;		.byte	$FF,$81,$81,$81,$81,$81,$81,$FF
; or:
;		.byte	%11111111	; square
;		.byte	%10000001
;		.byte	%10000001
;		.byte	%10000001
;		.byte	%10000001
;		.byte	%10000001
;		.byte	%10000001
;		.byte	%11111111
;
		.segment "MYCHAR"
CUSTOM:	;.res $0400
@FURY:
	; 0,0
		.byte	%00001111
		.byte	%00001111
		.byte	%00001111
		.byte	%00001111
		.byte	%00001110
		.byte	%00001110
		.byte	%00111001
		.byte	%00111001
	; 1,0
		.byte	%00101001
		.byte	%00111001
		.byte	%00111001
		.byte	%00111110
		.byte	%00111110
		.byte	%00111111
		.byte	%11111111
		.byte	%11111111
	; 2,0
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 3,0
		.byte	%00001111
		.byte	%00000000
		.byte	%00000010
		.byte	%00001010
		.byte	%00000110
		.byte	%00001010
		.byte	%00000110
		.byte	%00000110
	; 4,0
		.byte	%00001010
		.byte	%00000110
		.byte	%00001010
		.byte	%00000010
		.byte	%00000000
		.byte	%00000010
		.byte	%00001010
		.byte	%00101010
	; 5,0
		.byte	%00011010
		.byte	%00101010
		.byte	%00011010
		.byte	%00101010
		.byte	%00011010
		.byte	%00011010
		.byte	%00101010
		.byte	%00011010
	; 6,0
		.byte	%00101010
		.byte	%00011010
		.byte	%00101010
		.byte	%00001010
		.byte	%00000010
		.byte	%00000000
		.byte	%00000011
		.byte	%00001111
	; 7,0
		.byte	%00001011
		.byte	%00000010
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	;
	; 0,1
		.byte	%11111111
		.byte	%11111111
		.byte	%11010101
		.byte	%11010101
		.byte	%01101010
		.byte	%01101010
		.byte	%10101010
		.byte	%10101010
	; 1,1
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%01101010
		.byte	%01101010
		.byte	%10011001
		.byte	%11011001
		.byte	%01011101
	; 2,1
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 3,1
		.byte	%11111111
		.byte	%10101010
		.byte	%10101111
		.byte	%10111111
		.byte	%10111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 4,1
		.byte	%11111111
		.byte	%10111111
		.byte	%10111111
		.byte	%10101111
		.byte	%10101010
		.byte	%10101010
		.byte	%10101011
		.byte	%10101111
	; 5,1
		.byte	%10111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 6,1
		.byte	%11111111
		.byte	%10111111
		.byte	%10101111
		.byte	%10111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 7,1
		.byte	%11111111
		.byte	%11111111
		.byte	%10111111
		.byte	%00101111
		.byte	%00001011
		.byte	%00000010
		.byte	%00000000
		.byte	%00000000
	;
	; 0,2
		.byte	%11000000
		.byte	%11000000
		.byte	%11110000
		.byte	%11110000
		.byte	%01101100
		.byte	%01101100
		.byte	%10011011
		.byte	%10011011
	; 1,2
		.byte	%10011010
		.byte	%10011011
		.byte	%10011011
		.byte	%01101111
		.byte	%01101111
		.byte	%10111111
		.byte	%11111111
		.byte	%01111111
	; 2,2
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11101110
		.byte	%10111110
	; 3,2
		.byte	%10111110
		.byte	%10111110
		.byte	%10111110
		.byte	%10111110
		.byte	%10111110
		.byte	%11101110
		.byte	%11111111
		.byte	%11111111
	; 4,2
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%10101111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 5,2
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 6,2
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 7,2
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%10111111
		.byte	%00101010
	;
	; 0,3
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 1,3
		.byte	%11000000
		.byte	%11000000
		.byte	%11110000
		.byte	%11110000
		.byte	%11111100
		.byte	%11111100
		.byte	%11111111
		.byte	%11111111
	; 2,3
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111110
		.byte	%10111011
		.byte	%11111011
		.byte	%11111011
	; 3,3
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%11111011
		.byte	%10111011
		.byte	%11111110
	; 4,3
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 5,3
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 6,3
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 7,3
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%10101010
	;
	; 0,4
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 1,4
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 2,4
		.byte	%11000000
		.byte	%11000000
		.byte	%11110000
		.byte	%11111111
		.byte	%11111110
		.byte	%11111010
		.byte	%11111010
		.byte	%11111010
	; 3,4
		.byte	%11111010
		.byte	%11111010
		.byte	%11111010
		.byte	%11111010
		.byte	%11111010
		.byte	%11111010
		.byte	%11111110
		.byte	%11111111
	; 4,4
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 5,4
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 6,4
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111110
	; 7,4
		.byte	%11111110
		.byte	%11111110
		.byte	%11111000
		.byte	%11111000
		.byte	%11111000
		.byte	%11100000
		.byte	%11100000
		.byte	%10100000
	;
	; 0,5
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 1,5
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 2,5
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%11111010
		.byte	%10111010
		.byte	%10111010
		.byte	%10111010
		.byte	%10111010
	; 3,5
		.byte	%10111010
		.byte	%10111010
		.byte	%10111010
		.byte	%10111010
		.byte	%10111010
		.byte	%10111010
		.byte	%10111010
		.byte	%10111010
	; 4,5
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 5,5
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111110
		.byte	%11111110
		.byte	%11111110
		.byte	%11111000
		.byte	%11111000
	; 6,5
		.byte	%11111000
		.byte	%11100000
		.byte	%11100000
		.byte	%11100000
		.byte	%10000000
		.byte	%10000000
		.byte	%10000000
		.byte	%00000000
	; 7,5
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	;
	; 0,6
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 1,6
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 2,6
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%10000000
		.byte	%10100000
		.byte	%10101000
		.byte	%10101010
	; 3,6
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101011
		.byte	%10111111
		.byte	%11111111
	; 4,6
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 5,6
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%10101010
		.byte	%10101010
		.byte	%10101000
		.byte	%00000000
	; 6,6
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 7,6
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	;
	; 0,7
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 1,7
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 2,7
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 3,7
		.byte	%10000000
		.byte	%10100000
		.byte	%10101000
		.byte	%10101010
		.byte	%10111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 4,7
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 5,7
		.byte	%11111111
		.byte	%11111111
		.byte	%11111000
		.byte	%11100000
		.byte	%10100100
		.byte	%10100100
		.byte	%00100000
		.byte	%00001000
	; 6,7
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 7,7
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	;
	; 0,8
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 1,8
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 2,8
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 3,8
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%10100000
		.byte	%11111010
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 4,8
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
	; 5,8
		.byte	%11110000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 6,8
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 7,8
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	;
	; 0,9
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 1,9
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 2,9
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 3,9
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%10100000
		.byte	%11111010
		.byte	%11111111
	; 4,9
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11110000
		.byte	%00000000
	; 5,9
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 6,9
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 7,9
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	;
	; 0,10
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 1,10
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 2,10
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 3,10
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%10100000
	; 4,10
		.byte	%11111010
		.byte	%11111111
		.byte	%11111111
		.byte	%11111100
		.byte	%11110000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 5,10
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 6,10
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 7,10
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
@Ckey:	;
	; 0,0
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
	; 1,0
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
	; 2,0
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 0,1
		.byte	%00000000
		.byte	%00000000
		.byte	%11111111
		.byte	%11111111
		.byte	%11111010
		.byte	%11101010
		.byte	%11101010
		.byte	%10101011
	; 1,1
		.byte	%10101111
		.byte	%10101111
		.byte	%10101111
		.byte	%10101111
		.byte	%10101111
		.byte	%10101111
		.byte	%10101111
		.byte	%10101011
	; 2,1
		.byte	%11101010
		.byte	%11101010
		.byte	%11111010
		.byte	%11111111
		.byte	%11111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 0,2
		.byte	%00000000
		.byte	%00000000
		.byte	%11111111
		.byte	%11111111
		.byte	%10111111
		.byte	%10111111
		.byte	%10111111
		.byte	%11101010
	; 1,2
		.byte	%11101010
		.byte	%11101011
		.byte	%11101011
		.byte	%11111111
		.byte	%11010111
		.byte	%11010111
		.byte	%11010101
		.byte	%11010101
	; 2,2
		.byte	%10111111
		.byte	%10111111
		.byte	%10111111
		.byte	%11111111
		.byte	%11111111
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
	; 0,3
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
	; 1,3
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
	; 2,3
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%11000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
@C:		.byte	%00111100
		.byte	%01000010
		.byte	%10011101
		.byte	%10100001
		.byte	%10100001
		.byte	%10011101
		.byte	%01000010
		.byte	%00111100

;*********************************************************************
; Your main program code starts here
;
		.segment "CODE"

RESTART:
		.global RESTART
		.include "omega-fury.s"

