; Omega Fury for Commodore VIC 20
; written by Robert Hurst <robert@hurst-ri.us>
; updated version: 30-Oct-2011
;
; see compile.bat for compile, link, and go instructions
;
; to run the binary using viceteam.org project:
;	xvic -memory 16k -autostart omega-fury.prg
; to run the binary using mess.org project:
;	mess vic20 -ramsize 24k -quik omega-fury.prg
;
; these globals are not required -- but it makes nice to the VICE monitor
; for debugging ...
;
.global	ACTION			; player's action this frame
.global	ACTION2			; player's action last frame
.global	ACTIONS			; frames this action was repeated
.global	ALERTS			; 0-3: sound KLAXON event sequence
.global	BIGHITMARK		; 0-3: animated large explosion sequence
.global	BOLT			; 0-3: Omega Fury light bolt type
.global	BOLTDIR			; 0-15: Omega Fury light bolt direction
.global BOOMB4
.global BOOMC			; graphic character code for sonic boomer pulse
.global BOOMDX			; direction of sonic boomer pulse
.global BOOMDY			; direction of sonic boomer pulse
.global BOOMED			; sound event when hit by a sonic boomer pulse
.global BOOMER			; flag for active sonic boomer pulse
.global	BOOMX			; current sonic boomer pulse col
.global BOOMY			; current sonic boomer pulse row
.global	COLLISION		; immunity countdown from an Omega Fury collision
.global	DIRECTION		; 0-15: Omega Fury directional pointer
.global	DROPPED			; 0-4: sound dropped a mine event sequence
.global	EMPTY			; 0-8: sound empty energizer event sequence
.global	ENERGIZER		; 0-255: Omega Fury fuel/energy status
.global	FIRE			; $80: firing
.global	FLASHING		; countdown to toggle alternate VIC color
.global	FRAME			; 0-255: current gameloop counter
.global	FURYHP			; 0-64: Omega Fury current hull plating status
.global	HEARTBEATS		; 0-?: number of active heart-stoppers (sound event)
.global	HIT
.global	HITMARK			; 0-3: animated explosion sequence
.global	INERTIA			; 0-15: current direction travelling
.global	JUMPMARK		; 0-5: animated warp sequence
.global	KLAXON			; sound value for voice #2
.global	LOOPX			; short loop counter used throughout game
.global	MINE			; 0-16: number of mines
.global	MINES			; 1-4: mine type
.global	MINEX			; 0-20: X coordinate
.global	MINEY			; 0-23: Y coordinate
.global	MISSION			; 0-??: current mission, 15 is repeated
.global	MISSIONX		; 0-??: current mission in decimal mode
.global	NAVI
.global	NEUTRON
.global NEUTRONDIR		; 0-15: direction
.global	NME				; 1-4: enemy ship type (0=empty)
.global	NMEDIR			; 0-15: enemy ship direction
.global	NMEHP			; 0-??: enemy ship hull points
.global	NMELEN			; 0-??: enemy ship duration of plan
.global	NMEPLAN			; 0-??: 8-bit flags for enemy ship plan of attack
.global	NMESHOT			; 1-4: enemy phaser type
.global	NMESHOTDIR		; 0-15: enemy phaser direction
.global	NMESPEED		; 0-10: enemy speed
.global	NMETHRUST		; enemy ship movement counter for timed events
.global	NMETURN			; enemy ship is making a U-turn
.global	OBJECTIVE		; mission objectives to destroy (0=completed)
.global	OMEGAHP			; center Omegan objective
.global	PULSE			; sound HEART-STOPPER event sequence
.global	SCORE
.global	SF2010
.global	SHIPS			; number of active Thargoid ships
.global	SHOOTING
.global	SPEED			; level [FAST] 16-2[SLOW], or 1/0=none
.global	SPIN
.global	THRUST			; 0-8: which frame to move
.global	TITLE			; 0-5: player's current title
.global	WARPED			; 0-3: Thargoid ship that just warped into battle

;*********************************************************************
; Game restarts here ...
;
		; my VIC chipset init
		LDA #$80+$16	; set for videoram @ $1E00 with 22-columns
		STA VIC+$02		; video matrix address + columns
		LDA #$AE		; $AE = 10101110 = 23-rows + 8x8 height
		STA VIC+$03		; rows / character height
		LDA #$0E		; black / blue
		STA VIC+$0F		; background / border color
		; my Software Sprite Stack 4 init
		JSR SSSINIT
		;
		; my interrupt vector init
		SEI
		LDX #<BACKGROUND
		LDY #>BACKGROUND
		STX $0314
		STY $0315
		CLI
		LDA #$E0
		STA HIT
		;
		LDX #$11
		LDY #$00
		STX $01
		STY $02
		LDX #$00
@logo:	LDA LOGO+0*(8*17),X
		STA $1C00+0*(8*17),Y
		LDA LOGO+1*(8*17),X
		STA $1C00+1*(8*17),Y
		LDA LOGO+2*(8*17),X
		STA $1C00+2*(8*17),Y
		LDA LOGO+3*(8*17),X
		STA $1C00+3*(8*17),Y
		LDA LOGO+4*(8*17),X
		STA $1C00+4*(8*17),Y
		LDA LOGO+5*(8*17),X
		STA $1C00+5*(8*17),Y
		DEC $01
		BNE @not17
		LDA #$11
		STA $01
		INC $02
		LDY $02
		BNE @got17
@not17:	TYA
		CLC
		ADC #$08
		TAY
@got17:	INX
		CPX #(8*17)
		BNE @logo
		LDY #$00		; gratuitous
		JSR SSSFLIP
		;
		; title top half
		LDX #$02		; red
		STX COLORCODE
		LDX #$03
		LDY #$08
		JSR SSSPLOT
		LDX #$11
		LDY #0
@row1:	TYA
		JSR SSSPRINT
		INY
		DEX
		BNE @row1
		;
		LDX #$03
		LDY #$09
		JSR SSSPLOT
		LDX #$11
		LDY #17
@row2:	TYA
		JSR SSSPRINT
		INY
		DEX
		BNE @row2
		;
		LDX #$03
		LDY #$0A
		JSR SSSPLOT
		LDX #$11
		LDY #34
@row3:	TYA
		JSR SSSPRINT
		INY
		DEX
		BNE @row3
		; title bottom half
		LDX #$07		; yellow
		STX COLORCODE
		LDX #$03
		LDY #$0B
		JSR SSSPLOT
		LDX #$11
		LDY #51
@row4:	TYA
		JSR SSSPRINT
		INY
		DEX
		BNE @row4
		;
		LDX #$03
		LDY #$0C
		JSR SSSPLOT
		LDX #$11
		LDY #68
@row5:	TYA
		JSR SSSPRINT
		INY
		DEX
		BNE @row5
		;
		LDX #$03
		LDY #$0D
		JSR SSSPLOT
		LDX #$04
		LDY #85
@row6:	TYA
		JSR SSSPRINT
		INY
		DEX
		BNE @row6
		LDY #$00		; gratuitous
		JSR SSSFLIP
		LDY #$A0
		JSR SSSFLIP
		; me!
		LDX #$01		; white
		STX COLORCODE
		LDX #$07
		LDY #$0D
		JSR SSSPLOT
		LDX #$0D
		LDY #89
@author:
		TYA
		JSR SSSPRINT
		INY
		DEX
		BNE @author
		LDA #$04
		STA DROPPED
		LDY #$00		; gratuitous
		JSR SSSFLIP
		LDY #$50
		JSR SSSFLIP
		;
		LDA #$00
		STA $C6			; empty keyboard buffer
		TAY
@zero:	STA FRAME,Y		; init my variable space
		INY
		CPY #$90		; thru SCORE
		BNE @zero
		;
		LDA #%10001011	; float horizontal, 16x16
		LDY #$10
		JSR SSSCREATE
		LDA #%00001001	; multi-color with white
		LDX #<OMEGABIG
		LDY #>OMEGABIG
		JSR SSSANIM
		LDX #$FE
		STX VIC+$0D
		LDY #$80
		JSR SSSMOVEXY
		;
@flyby:
		LDX #$00
		JSR SSSUSE
		LDX SPRITEX
		TXA
		LSR
		LSR
		LSR
		LSR
		EOR #$0F
		ORA #$20
@vol:	STA VIC+$0E
		DEX
		DEX
		LDY SPRITEY
		JSR SSSMOVEXY
		LDY #$01
		JSR SSSFLIP
		LDA SPRITEX
		BNE @flyby
		;
@bye:	LDA #$00
		STA VIC+$0D
		LDA #$8F
		STA VIC+$0E
		LDA #SSSNULL
		JSR SSSCLEAR
		LDY #$00		; render immediate
		JSR SSSFLIP
		;
		LDX #$48
		LDY #$00
@copy:	LDA GRENADE1,Y
		STA $1C00,X
		INY
		INX
		CPX #$A8
		BNE @copy
		;
		LDA $028D		; look familiar?
		;CMP #$07		; :P -- emulators usually map CTRL as FIRE
		BNE @egg		; any combo counts,
		LDA $C6			; or a premature keypress
		BNE @egg
		JMP @intros
		;
@egg:	LDX #$00
		STX $C6
		STX FRAME
		JSR SSSUSE
		LDA #$01		; WHITE
		LDX #<FURYC
		LDY #>FURYC
		JSR SSSANIM
		LDX SSSCLIPX
		LDY #23*8
		JSR SSSMOVEXY
		LDA #<CREDITS
		STA $8E
		LDA #>CREDITS
		STA $8F
		LDY #$FF
		STY R0
		INY
		STY R1
		STY R2
@credits:
		LDY R1
		LDA ($8E),Y
		BEQ @xcredits
		CMP #$10
		BCS @crprint
		CMP #$0D
		BNE @crclr
		LDX #$00
		LDY CRSRROW
		INY
		JSR SSSPLOT
		JMP @crnext
@crclr:	STA COLORCODE
		BNE @crnext
@crprint:
		JSR SSSPRINT
		LDY #$08
		JSR @wait
@crnext:
		INC R1
		LDY R1
		BNE @credits
		INC $8F
		BNE @credits
@xcredits:
		LDY #$B0
		JSR @wait
		;
@intros:
		LDY #$00
		STY R0
@loop:
		LDA #SSSNULL
		JSR SSSCLEAR
		LDY #$00		; render immediate
		JSR SSSFLIP
		;
		; PRESS F1 TO CONTINUE
		LDY #$00
		STY HEARTBEATS
		LDX #$00
@copy1:	LDA F1,Y		; make F1
		STA $1F30,X
		INY
		INX
		CPX #$10
		BNE @copy1
		LDX #$03		; cyan
		STX COLORCODE
		LDX #$01
		LDY PLAYROWS
		DEY
		JSR SSSPLOT
		LDX #$00
@f1cont:
		LDA CONT,X
		JSR SSSPRINT
		INX
		CPX #$14
		BNE @f1cont
		;
		LDX #$07
		LDY PLAYROWS
		DEY
		JSR SSSPLOT
		LDY CRSRCOL
		LDA (COLORLINE),Y
		AND #$E0
		ORA #$09
		STA (COLORLINE),Y
		INY
		STA (COLORLINE),Y
		;
		LDX #$00
		JSR SSSUSE
		LDY #$00
		STY FRAME
		STY R1
		STY R2
		JSR SSSPLOT
		LDX R0
		LDA WALKERS+1,X
		TAY
		LDA WALKERS,X		; init vector to next walker
		TAX
		LDA #$01			; WHITE
		JSR SSSANIM
		LDX SSSCLIPX
		LDY #21*8
		JSR SSSMOVEXY
		LDX R0
		LDA INTROS,X		; init vector to next passage
		STA $8E
		LDA INTROS+1,X
		STA $8F
		BEQ @whoami
@while:
		LDY R1
		LDA ($8E),Y
		BEQ @nomore
		CMP #$80
		BCS @print
		CMP #$0D
		BNE @color
		LDX #$00
		LDY CRSRROW
		INY
		JSR SSSPLOT
		JMP @next
@color:	STA COLORCODE
		BNE @next
@print:
		JSR SSSPRINT
		LDY #$05
		JSR @wait
@next:
		JSR @start
		INC R1
		LDY R1
		BNE @while
		INC $8F
		BNE @while
@nomore:
		LDY #$A0
		JSR @wait
		INC R0
		INC R0			; next word
		JMP @loop
@whoami:
		JMP RESTART		; cycle intro again

@fini:	PLA				;--
		STA CRSRROW
		TAY
		PLA				;--
		STA CRSRCOL
		TAX
		JSR SSSPLOT
		PLA				;--
		STA COLORCODE
		RTS
@wait:
		STY R2
		LDA COLORCODE
		PHA				;++
		LDA CRSRCOL
		PHA				;++
		LDA CRSRROW
		PHA				;++
		LDY #$00
		STY MINE
		STY MINES
		LDA #%10001000	; float horizontal, 8x8
		LDY #$08
		STA SPRITEDEF
		STY SPRITEH
@waitloop:
		DEC R2
		BEQ @fini
		LDA FRAME
		AND #$01
		BNE @walk
		LDX #$00
		JSR SSSUSE
		DEC SPRITEX
		LDX SPRITEX
		LDY SPRITEY
		JSR SSSMOVEXY
		LDA SPRITEX
		SEC
		SBC #$08
		LSR
		LSR
		LSR
		TAX
		LDA SPRITEY
		SEC
		SBC #$10
		LSR
		LSR
		LSR
		TAY
		LDA R0
@w0:	BNE @w2
		CPX #9
		BNE @s1
		JSR DROPGRENADE
@s1:	JSR SCOUT
		JMP @walk
@w2:	CMP #$02
		BNE @w4
		CPX #10
		BNE @s2
		JSR DROPHULLBLASTER
@s2:	LDY #$0C		; left
		JSR CRUISER
		JMP @walk
@w4:	CMP #$04
		BNE @w6
		CPX #11
		BNE @s3
		JSR DROPSONICBOOMER
@s3:	LDY #$0C		; left
		JSR DESTROYER
		JMP @walk
@w6:	CMP #$06
		BNE @walk
		LDA #%10001011	; float horizontal, 16x16
		STA SPRITEDEF
		LDA #$10
		STA SPRITEH
		LDA FRAME
		PHA
		CPX #12
		BNE @s4
		JSR DROPHEARTSTOPPER
@s4:	PLA
		STA FRAME
		JSR CARRIER
@walk:	LDY #$01
		JSR SSSFLIP
		INC FRAME
		JMP @waitloop
@start:
		LDA #$FF
		STA $9122
		LDA $9111
		AND #$20		; FIRE
		BEQ INITNEW
		JSR GETIN		; get keyboard
		CMP #$85		; got F1 ?
		BEQ INITNEW		; go start a new game
		CMP #$86		; got F3 ?
		BEQ INITNEW3	; go start a new game
		CMP #$87		; got F5 ?
		BEQ INITNEW5	; go start a new game
		CMP #$88		; got F7 ?
		BEQ INITNEW7	; go start a new game
		RTS

INITNEW3:
		LDA #$02		; start at last SCOUT mission
		STA MISSION
		STA MISSIONX
		BNE INITNEW

INITNEW5:
		LDA #$05		; start at last DEFENDER mission
		STA MISSION
		STA MISSIONX
		BNE INITNEW

INITNEW7:
		LDA #$09		; start at last WARRIOR mission
		STA MISSION
		STA MISSIONX

;*********************************************************************
; INITIALIZE NEW GAME
;
INITNEW:
		PLA				; dump JSR @start return PC
		PLA
		JSR RANDOMIZE
		LDA #$80+$15	; 21-columns
		STA VIC+$02		; video matrix address + columns
		LDA #$B0		; $B0 = 10110000 = 24-rows + 8x8 height
		STA VIC+$03		; rows / character height
		LDA #$0A		; black with red
		STA VIC+$0F		; screen and border
		; reset sound channels
		LDA #$00
		TAY
@snd:	STA VIC+$0A,Y
		INY
		CPY #$04
		BNE @snd
		LDA #$FF		; light yellow & highest
		STA VIC+$0E		; auxiliary color & volume
		JSR SSSINIT
@missiles:				; #0-6: 3 for player, 4 for enemies
		LDA #%00000000	; 8x8 only
		LDY #$03		; 3-pixels high
		JSR SSSCREATE
		CPX #$06
		BNE @missiles
@bombs:					; #7 is for the neutron bomb
		LDA #%00001100	; 8x8 float
		LDY #$08
		JSR SSSCREATE
@ships:					; #8-11: up to 4 enemies
		LDA #%00001100
		LDY #$08
		JSR SSSCREATE
		CPX #$0A
		BNE @ships
@carrier:				; but for #11, reserve size for a space carrier
		LDA #%00001111
		LDY #$10
		JSR SSSCREATE
@sf2010:				; #12 pilot
		LDA #%00001100
		LDY #$08
		JSR SSSCREATE
@hitmarks:				; #13 is small hit, #14 is large hit
		LDA #%00001100
		LDY #$07
		JSR SSSCREATE
		LDA #%00001111
		LDY #$10
		JSR SSSCREATE	; total of 98 (49*2) custom characters for sprites

;*********************************************************************
; START NEXT LEVEL
;
NEXTLEVEL:
		LDX #$00
@bolts:	LDA #$00
		STA BOLT,X
		LDA SPRITEDEF,X
		AND #$7F		; disable sprite
		STA SPRITEDEF,X
		INX
		CPX #$03
		BNE @bolts
		LDX #$00
@phasers:
		LDA #$00
		STA NMESHOT,X
		LDA SPRITEDEF+3,X
		AND #$7F		; disable sprite
		STA SPRITEDEF+3,X
		INX
		CPX #$04
		BNE @phasers
		LDX #$00
		TXA
@nmes:	STA NME,X
		STA NMETURN,X
		INX
		CPX #$04
		BNE @nmes
		TAX
@mines:	STA MINES,X
		INX
		CPX #$10
		BNE @mines
		STA BOOMB4
		STA BOOMER
		;
		LDA #SSSNULL
		JSR SSSCLEAR
		JSR DRAWOBJECTIVE
		JSR DRAWRANK
		LDY #$00		; render immediate
		JSR SSSFLIP
		;
		LDX MISSION
		BNE @mission
		LDA #$01		; WHITE
		STA COLORCODE
		LDX #$07
		LDY #$05
		JSR DRAWCELLS
		.byte	$81,$83,$81,$84,$85,$8D,$99,$00
		LDY #$00		; render immediate
		JSR SSSFLIP
		;
		LDA #$07		; YELLOW
		STA COLORCODE
		LDX #$07
		LDY #$05
		JSR DRAWCELLS
		.byte	$81,$83,$81,$84,$85,$8D,$99,$00
		LDY #$00		; render immediate
		JSR SSSFLIP
		JMP @missionx
		;
@mission:
		LDA #$01		; WHITE
		STA COLORCODE
		LDX #$05
		LDY #$05
		JSR DRAWCELLS
		.byte	$8D,$89,$93,$93,$89,$8F,$8E,$BA,$00
		LDY #$00		; render immediate
		JSR SSSFLIP
		;
		LDA #$03		; CYAN
		STA COLORCODE
		LDX #$05
		LDY #$05
		JSR DRAWCELLS
		.byte	$8D,$89,$93,$93,$89,$8F,$8E,$BA,$00
		LDY #$00		; render immediate
		JSR SSSFLIP
		;
		LDA #$07
		STA COLORCODE
		LDX #$0E
		LDY #$05
		JSR SSSPLOT
		LDA MISSIONX
		LSR
		LSR
		LSR
		LSR
		ORA #$B0
		JSR SSSPRINT
		LDA MISSIONX
		AND #$0F
		ORA #$B0
		JSR SSSPRINT
		;
@missionx:
		LDX #$0C		; player
		JSR SSSUSE
		LDA SPRITEDEF,X
		ORA #$80		; enable
		STA SPRITEDEF,X
		LDA #$01		; WHITE
		LDX #<FURY0
		LDY #>FURY0
		JSR SSSANIM
		LDX #$60
		LDY #$B0
		JSR SSSMOVEXY
		LDY #$00
		STY NAVI
		STY DIRECTION
		STY SPEED
		;
		LDA #$D4		; 212 hull points
		STA OMEGAHP
		LDA #$40		; 64 hull points
		STA FURYHP
		JSR HULLSTATUS
		JSR SCORESTATUS
		LDA #$F0
		STA ENERGIZER
		LDA #$04
		STA ALERTS
		LDY MISSION
		BEQ @na
		LSR ALERTS
@na:	JSR WEAPONSTATUS
		LDY #2
		JSR SSSFLIP
		LDA ALERTS
		BNE @na
		;
		LDA #$E0
		STA HIT
		LDX #$00		; fill buffer with RND values (0-99)
		STX LOOPX
@rloop:	JSR $E094		; rnd: perform BASIC RND
		JSR $DAE2		; mul10: Multiply FAC#1 by 10
		JSR $DAE2		; mul10: Multiply FAC#1 by 10
		JSR $D1AA		; facinx: FAC#1 to Integer in (AC/YR)
		TYA				; A is always zero, make A = 0-99
		INC LOOPX
		LDX LOOPX
		STA RANDOM,X	; store result in cassette buffer
		CPX #$BF		; until it is filled
		BNE @rloop
		;
		LDA #SSSNULL
		JSR SSSCLEAR
		JSR DRAWOBJECTIVE
		LDY #$00		; render immediate
		JSR SSSFLIP
		JSR HULLSTATUS
		JSR WEAPONSTATUS
		JSR SCORESTATUS
		;
		LDY #$00
		STY OBJECTIVE
		STY SHIPS
		LDA MISSION
		ASL
		ASL
		ASL
		TAX
@ships:	LDA MISSIONS,X
		BEQ @skip
		INC SHIPS
@skip:	INX
		INY
		CPY #$04
		BNE @ships
		;
		STX R2
		LDY #$00
		STY R3
@seed:	LDA MISSIONS,X
		BEQ @next
		JSR NEWMINE
@next:	INC R2
		LDX R2
		INC R3
		LDY R3
		CPY #$04
		BNE @seed
		;
		LDA #$01
		STA FRAME
		JSR DRAWSHIELDS

;*********************************************************************
; MAIN GAME PLAYING LOOP
;
GAMELOOP:
		LDA FRAME
		AND #$7F
		BNE OMEGAFURY
		;
		DEC SHIPS
		LDA MISSION
		LDX FURYHP
		BNE @cont
		LDA #$0F		; complement attack now that it is defenseless
@cont:	ASL
		ASL
		ASL
		CLC
		ADC SHIPS
		TAX
		LDA MISSIONS,X
		JSR NEWTHARGOID

;*********************************************************************
; Omega Fury navigational and firing controls
;
OMEGAFURY:
		LDX #$0C		; player
		STX sssNUM
		LDA SPRITEDEF,X
		AND #$80
		BNE @alive
		STA SPEED		; ship is dead
		STA INERTIA		; keep it off the playfield
		STA SPRITEX,X
		STA SPRITEY,X
@alive:	LDA NAVI
		STA SF2010
		BEQ @navi		; is ship under pilot's control?
		LDY #$00
		LDA FRAME
		AND #$03
		BNE @skip
		DEC NAVI
		LDY SPIN
@skip:	STY ACTION
		JMP @nonavi
@navi:
		LDY #$00
		STY ACTION
		STY $9113
		LDA #$7F
		STA $9122
		;
		LDA $9120
		AND #$80
		BNE @joy0
		LDA #$01		; RIGHT
		STA ACTION
@joy0:	LDA #$FF
		STA $9122
		LDY $9111
		;
		LDX #$00		; clear events
		TYA
		AND #$20		; FIRE
		BNE @xfire
		LDX #$80		; record DOWN event
		LDA FIRE
		BNE @xfire
		STY YCOPY
		JSR NEWBOLT
		LDY YCOPY
		LDX #$80		; record DOWN event
@xfire:	STX FIRE
		;
		TYA
		AND #$08
		BNE @joy1
		LDA #$02		; DOWN
		ORA ACTION
		STA ACTION
		;
@joy1:	TYA
		AND #$10
		BNE @joy2
		LDA #$04		; LEFT
		ORA ACTION
		STA ACTION
		;
@joy2:	TYA
		AND #$04
		BNE @joy3
		LDA #$08		; UP
		ORA ACTION
		STA ACTION
@joy3:
		LDA ACTION
		CMP ACTION2
		BEQ @oldact
		STA ACTION2
		LDA #00
		STA ACTIONS
		STA VIC+$0D
@oldact:
		LDA ACTIONS
		AND #$03		; repeat every 4-frames
		BEQ @ckup
		JMP @nochange
		;
@ckup:
		INC SF2010
		LDA ACTION
		AND #$08		; UP
		BEQ @nonavi
		LDA INERTIA
		CMP DIRECTION
		BEQ @accel		; same direction, accelerate
		LDA SPEED
		CMP #$03
		BCC @accel		; at this snail pace, just allow new direction
		CMP #$0B
		BCC @turn
		JMP @braking	; slowdown first, before turning
@turn:	LDA INERTIA
		CMP DIRECTION
		BCS @delta
		ADC #$10
		SEC
@delta:	SBC DIRECTION
		CMP #$08
		BCC @driftleft
		BNE @driftright
		JMP @braking
@driftright:
		LDA #$FE
		STA VIC+$0D
		INC INERTIA
		LDA INERTIA
		CMP #$10
		BCC @nonavi
		LDA #$00
		STA INERTIA
		BEQ @nonavi
@driftleft:
		LDA #$FE
		STA VIC+$0D
		DEC INERTIA
		LDA INERTIA
		CMP #$FF
		BNE @nonavi
		LDA #$0F
		STA INERTIA
		BNE @nonavi
@accel:	LDA SPEED
		CMP #$10
		BCS @newdir
		INC SPEED
@newdir:
		LDA ENERGIZER
		BEQ @nonavi
		DEC ENERGIZER
		LDA #$FE
		STA VIC+$0D
		LDA DIRECTION
		STA INERTIA
		;
@nonavi:
		LDA ACTIONS
		AND #$07		; repeat every 8-frames
		BEQ @ckright
		JMP @nochange
@ckright:
		LDA ACTION
		AND #$01		; RIGHT
		BEQ @ckleft
@goright:
		INC DIRECTION
		LDA DIRECTION
		CMP #$10
		BCC @ckleft
		LDA #$00
		STA DIRECTION
		;
@ckleft:
		LDA ACTION
		AND #$04		; LEFT
		BEQ @ckdown
@goleft:
		DEC DIRECTION
		LDA DIRECTION
		CMP #$FF
		BCC @ckdown
		LDA #$0F
		STA DIRECTION
		;
@ckdown:
		LDA ACTION
		AND #$02		; DOWN
		BEQ @nochange
@braking:
		LDA SPEED
		BEQ @nochange
		LDA ENERGIZER
		BEQ @nochange
		DEC ENERGIZER
		DEC SPEED
		LDA #$FE
		STA VIC+$0D
		;
@nochange:
		INC ACTIONS
		LDY #>FURY0
		LDA DIRECTION
		ASL
		ASL
		ASL
		CLC
		ADC #<FURY0
		BCC @nocc
		INY
@nocc:	TAX
		LDA #$01		; white
		JSR SSSANIM		; update ship's direction

OMEGAMOVE:
		LDA SPEED
		LSR
		BEQ TACTICAL
		TAY
		LDA FRAME
		AND #$07
		TAX
		LDA MASK,X
		AND VELOCITY-1,Y
		BEQ TACTICAL
		;
		INC SF2010
		INC THRUST		; ship will move this frame
		LDX INERTIA
		LDY #$00
		JSR TRAVELING
		STX INERTIA
		LDX R1
		LDY R2
		JSR SSSMOVEXY

TACTICAL:
		; replenish fuel and update energizer gauge
		LDA FURYHP
		BEQ @cc
		LDA OMEGAHP
		BEQ @mt			; no Omega energy field for fuel!!
		LDA FRAME
		AND #$02
		BNE ENEMY
		LDA @unit+1
		EOR #$03
		STA @unit+1
		LDA ENERGIZER
		CMP #$FF
		BEQ ENEMY
		CLC
@unit:	ADC #$02
		BCC @cc
		LDA #$FF
@cc:	STA ENERGIZER
@mt:	JSR WEAPONSTATUS

;*********************************************************************
; ENEMY CRAFT
;
ENEMY:
		LDX #$00
		STX LOOPX
@loop:
		LDA NME,X
		BNE @ok
@not:	JMP @next
@ok:	LDA NME,X		; active?
		BEQ @not
		LDA NMESPEED,X
		LSR
		BEQ @x			; stationary?
		TAY
		LDA FRAME
		AND #$07
		TAX
		LDA MASK,X
		AND VELOCITY-1,Y
		BNE @go
		JMP @next
@go:
		LDX LOOPX
		LDA NMEDIR,X
		TAX				; X = direction
		LDY LOOPX
		JSR TRAVELING2	; what's the next correct move?
		TXA
		LDX LOOPX
		STA NMEDIR,X
		TXA
		CLC
		ADC #$08
		STA sssNUM
		LDX R1
		LDY R2
		JSR SSSMOVEXY
@x:
		LDX LOOPX
		LDA SPRITEX+8,X
		AND #$07
		BEQ @y
		JMP @anim		; in-between cells
@y:		LDA SPRITEY+8,X
		AND #$07
		BEQ @l
		JMP @anim		; in-between cells
@l:		DEC NMELEN,X
		INC NMETHRUST,X	; account for this action
		LDA NMESPEED,X
		BNE @moving
		LDA NMEPLAN,X
		BEQ @warpout	; operation spoil-sport
		LDA NMETHRUST,X
		AND #$1F
		BNE @moving
@firing:
		LDA NMEDIR,X
		TAY
		LDA NME,X
		JSR NEWPHASER
@moving:
		LDX LOOPX
		LDA NMETURN,X
		BEQ @noturn
		CLC
		ADC NMEDIR,X	; ship is still completing a U-turn
		AND #$0F
		STA NMEDIR,X
		LDA NMELEN,X
		BNE @anim
		STA NMETURN,X	; ship completed turn
		INC NMELEN,X	; travel another space
		BNE @anim
@noturn:
		LDA NMELEN,X
		BNE @anim
		STA NMETURN,X
		JSR PLANNING	; re-examine for any change in agenda
		JMP @anim
@warpout:
		LDA NMELEN,X
		CMP #$01
		BNE @anim
		LDA #$00
		STA NME,X
		LDA NMEDIR,X
		STA NEUTRONDIR
		DEC OBJECTIVE
		LDA SPRITEDEF+8,X
		AND #$7F
		STA SPRITEDEF+8,X
		LDA SPRITEY+8,X
		PHA				;++
		TAY
		LDA #$00
		STA SPRITEY+8,X	; baseship leaves
		LDA SPRITEX+8,X
		PHA				;++
		TAX
		JSR NEWJUMPMARK
		PLA				;--
		CLC
		ADC #$04
		TAX
		PLA				;--
		CLC
		ADC #$04
		TAY
		JSR NEWNEUTRONBOMB
		JMP @next
@anim:
		LDX LOOPX
		TXA
		CLC
		ADC #$08
		STA sssNUM
		LDA NMEDIR,X
		TAY
		LDA NME,X
		BEQ @next
@s1:	CMP #$01
		BNE @s2
		JSR SCOUT
		JMP @next
@s2:	CMP #$02
		BNE @s3
		JSR CRUISER
		JMP @next
@s3:	CMP #$03
		BNE @s4
		JSR DESTROYER
		JMP @next
@s4:	JSR CARRIER
@next:
		INC LOOPX
		LDX LOOPX
		CPX #$04
		BEQ @fini
		JMP @loop
@fini:

;*********************************************************************
; NPC MOVES
;
		LDA NEUTRON
		BEQ PLAYERSHOTS
		LDA FRAME
		AND #$07		; move agonizingly slow
		BNE PLAYERSHOTS
		JSR OLDNEUTRONBOMB

PLAYERSHOTS:
		LDX #$00
		STX LOOPX
@hero:	LDA BOLT,X
		BEQ @next
		JSR OLDBOLT
@next:	INC LOOPX
		LDX LOOPX
		CPX #$03
		BNE @hero
		;
NMESHOTS:
		LDX #$00
		STX LOOPX
@nme:	LDA NMESHOT,X
		BEQ @next
		JSR OLDPHASER
@next:	INC LOOPX
		LDX LOOPX
		CPX #$04
		BNE @nme
		;
		LDA HITMARK
		BEQ @nohit
		JSR OLDHITMARK
@nohit:	LDA BIGHITMARK
		BEQ @nobig
		JSR OLDBIGHITMARK
@nobig:	LDA JUMPMARK
		BEQ @nojump
		JSR OLDJUMPMARK
@nojump:
		;
MINEPROXIMITY:
		LDX #$00
		STX R4
@loop:	LDA MINES,X
		BNE @q1
		JMP @next
		;
@q1:	CMP #$01		; grenade
		BEQ @m1
		JMP @q2
@m1:	LDA MINEX,X
		ASL
		ASL
		ASL
		CLC
		ADC #$10
		SEC
		SBC SPRITEX+12
		BCS @nov1
		EOR #$FF
		CLC
		ADC #$01
@nov1:	CMP #$08
		BCC @x1
		JMP @next
@x1:	LDA MINEY,X
		ASL
		ASL
		ASL
		CLC
		ADC #$10
		SEC
		SBC SPRITEY+12
		BCS @nov2
		EOR #$FF
		CLC
		ADC #$01
@nov2:	CMP #$08
		BCC @y1
		JMP @next
@y1:	JSR XMINE
		LDA #$18
		JSR FURYHIT
		LSR SPEED
		JSR RNDNEXT
		AND #$0F
		JSR FURYNAVI
		JMP @next
		;
@q2:	CMP #$02		; hull blaster
		BEQ @m2
		JMP @q3
@m2:	LDA MINEX,X
		ASL
		ASL
		ASL
		CLC
		ADC #$10
		SEC
		SBC SPRITEX+12
		BCS @nov3
		EOR #$FF
		CLC
		ADC #$01
@nov3:	CMP #$10
		BCC @x2
		JMP @next
@x2:	LDA MINEY,X
		ASL
		ASL
		ASL
		CLC
		ADC #$10
		SEC
		SBC SPRITEY+12
		BCS @nov4
		EOR #$FF
		CLC
		ADC #$01
@nov4:	CMP #$10
		BCC @y2
		JMP @next
@y2:	JSR XMINE
		LDA #$10
		JSR FURYHIT
		JSR RNDNEXT
		AND #$1F
		JSR FURYNAVI
		JMP @next
		;
@q3:	CMP #$03			; sonic boomer
		BEQ @m3
		JMP @q4
@m3:	LDA BOOMER
		BNE @m3a
		LDA BOOMB4
		CMP R4
		BEQ @m3new
		BCC @m3new
		JMP @next
@m3new:	TXA
		TAY
		INY
		STY BOOMER			; start next boomer
		LDA MINEX,X
		ASL
		ASL
		ASL
		CLC
		ADC #$10
		STA BOOMX
		LDA MINEY,X
		ASL
		ASL
		ASL
		CLC
		ADC #$10
		STA BOOMY
		JSR RNDNEXT
		AND #$03
		TAY
		LDA BOOMITC,Y
		STA BOOMC
		LDA BOOMITX,Y
		STA BOOMDX
		LDA BOOMITY,Y
		STA BOOMDY
		LDX R4
@m3a:	LDA MINEX,X
		ASL
		ASL
		ASL
		CLC
		ADC #$10
		SEC
		SBC SPRITEX+12
		BCS @nov5
		EOR #$FF
		CLC
		ADC #$01
@nov5:	CMP #$08
		BCC @x3
		JMP @next
@x3:	LDA MINEY,X
		ASL
		ASL
		ASL
		CLC
		ADC #$10
		SEC
		SBC SPRITEY+12
		BCS @nov6
		EOR #$FF
		CLC
		ADC #$01
@nov6:	CMP #$08
		BCC @y3
		JMP @next
@y3:	JSR XMINE
		LDA #$02
		JSR FURYHIT
		LDA FRAME
		AND #$07
		JSR FURYNAVI
		JMP @next
		;
@q4:	CMP #$04			; heart-stopper
		BNE @next
@m4:	LDA MINEX,X
		ASL
		ASL
		ASL
		CLC
		ADC #$10
		SEC
		SBC SPRITEX+12
		BCS @nov7
		EOR #$FF
		CLC
		ADC #$01
@nov7:	CMP #$20
		BCC @x4
		JMP @next
@x4:	LDA MINEY,X
		ASL
		ASL
		ASL
		CLC
		ADC #$10
		SEC
		SBC SPRITEY+12
		BCS @nov8
		EOR #$FF
		CLC
		ADC #$01
@nov8:	CMP #$20
		BCC @y4
		JMP @next
@y4:	JSR XMINE
		LDA #$AA		; pink / red
		STA VIC+$0F
		DEC HEARTBEATS
		LDA #$00
		STA ENERGIZER
		LDA #$32
		JSR FURYHIT
		JSR RNDNEXT
		AND #$3F
		JSR FURYNAVI
@next:	INC R4
		LDX R4
		CPX #$10
		BEQ @xmines
		JMP @loop
@xmines:
		LDA FRAME
		AND #$03
		BNE @noboom
		LDA BOOMER
		BNE @boom
		JMP @xboom
@noboom:
		JMP NMEPROXIMITY
@boom:					; do sonic boomer probing ...
		LDX BOOMX
		LDY BOOMY
		JSR SSSPEEKXY
		CMP BOOMC
		BEQ @erase
		CMP #SSSNULL
		BEQ @boom2
		CMP #$20
		BCC @boom2		; allow passthru of other mines
		BCS @ng
@erase:	LDA #SSSNULL
		JSR SSSPOKE
@boom2:	LDA BOOMX
		CLC
		ADC BOOMDX
		STA BOOMX
		CMP #$10
		BCS @x
@ng:	JMP @xboom		; disrupt wave
		CMP SSSCLIPX
		BCS @ng
@x:		TAX
		LDA BOOMY
		CLC
		ADC BOOMDY
		STA BOOMY
		CMP #$10
		BCC @ng
		CMP #200		; SSSCLIPY - 8
		BCS @ng
@y:		TAY
		JSR SSSPEEKXY
		CMP #SSSNULL
		BNE NMEPROXIMITY
		LDX CRSRCOL
		LDY CRSRROW
		JSR SSSPEEKS
		CMP #SSSNULL
		BNE NMEPROXIMITY
		LDX CRSRCOL
		LDY CRSRROW
		JSR SSSPLOT
		LDA #$04		; MAGENTA
		STA COLORCODE
		LDA BOOMC
		JSR SSSPOKE
		LDA BOOMX
		SEC
		SBC SPRITEX+12
		BCS @bx
		EOR #$FF
		CLC
		ADC #$01
@bx:	CMP #$08
		BCC @bhx
		JMP NMEPROXIMITY
@bhx:	LDA BOOMY
		SEC
		SBC SPRITEY+12
		BCS @by
		EOR #$FF
		CLC
		ADC #$01
@by:	CMP #$08
		BCC @bhy
		JMP NMEPROXIMITY
@bhy:	LDA #$01
		STA BOOMED
		JSR FURYHIT
		LDA #$32		; NAVICOM requires a re-boot
		JSR FURYNAVI
		LDA #$00		; shutoff any noise
		STA HIT
		STA VIC+$0D
		JMP NMEPROXIMITY
@xboom:	LDA BOOMER
		STA BOOMB4
		LDA #$00
		STA BOOMER
		;
NMEPROXIMITY:
		LDX COLLISION
		BEQ @cont
		DEC COLLISION
		JMP @xnme
@cont:	LDX #$00
		STX LOOPX
@loop:	LDA NME,X
		BNE @ck
		JMP @next
@ck:	LDA #$01		; adjustment for EOR operation
		STA ACOPY
		LDA #$08
		CMP SPRITEH+8,X
		BEQ @nme
		LDA #$F9		; adjust for carrier collision (-7)
		STA ACOPY
@nme:	LDA SPRITEX+8,X
		SEC
		SBC SPRITEX+12
		BCS @nov1
		EOR #$FF
		CLC
		ADC ACOPY
@nov1:	CMP #$08
		BCS @next
		LDA SPRITEY+8,X
		SEC
		SBC SPRITEY+12
		BCS @nov2
		EOR #$FF
		CLC
		ADC ACOPY
@nov2:	CMP #$08
		BCS @next
		LDX SPRITEX+12
		LDY SPRITEY+12
		JSR NEWHITMARK	; player hit
		LDA #$E0
		STA HIT
		LDX LOOPX
		DEC NMEHP,X		; scratch the enemy's hull paint
		BNE @paint
		; killed enemy by ramming it -- no score
		LDA #$00
		STA NME,X
		LDA SPRITEDEF+8,X
		AND #$7F		; disable sprite
		STA SPRITEDEF+8,X
		DEC OBJECTIVE
@paint:	LDA SPRITEH+8,X	; bigger the ship, the bigger the hit
		CLC
		ADC SPEED		; and the faster you were going
		LSR				; half
		JSR FURYHIT
		LDA FURYHP
		BEQ @xnme
		LDA #$08
		STA SPEED
		LDA #$38		; 56-frames of immunity
		STA COLLISION
		LDA INERTIA
		EOR #$08		; reverse direction
		STA INERTIA
		LDA #100
		JSR RND
		AND #$0F
		JSR FURYNAVI
		JMP @xnme		; go!
@next:	INC LOOPX
		LDX LOOPX
		CPX #$04
		BEQ @xnme
		JMP @loop
@xnme:
		LDA SHIPS
		BEQ @nomore		; more ships for this mission?
		INC FRAME		; anim counter
		LDY #$00
		JSR SSSFFLIP	; render, but skip a frame if necessary
		JMP GAMELOOP
@nomore:
		LDA OBJECTIVE
		BNE @notdone	; completed this mission?
		JMP FINILEVEL
@notdone:
		LDA FURYHP
		BNE @alive
		LDA MISSION
		BEQ @fini		; dead at the acamedy?  You suck.
		LDA OMEGAHP
		BNE @alive
		LDA NAVI
		BEQ @fini
		JSR KILLOBJECTIVE
		LDY #$00
		STY $C6			; empty keyboard buffer
@alive:
		INC FRAME		; anim counter
		LDY #$00
		JSR SSSFFLIP	; render, but skip a frame if necessary
		JMP OMEGAFURY	; complete the game loop until over
		;
@fini:
		LDA #$08
		STA VIC+$0F
		LDA FRAME
		AND #$01
		TAX
		LDA SPRITEDEF+8,X
		EOR #$80		; flicker Thargoid sprite #0/1
		STA SPRITEDEF+8,X
		LDA SPRITEDEF+10,X
		EOR #$80		; flicker Thargoid sprite #2/3
		STA SPRITEDEF+10,X
		JSR SSSREFRESH
		JSR GAMEOVER
		JSR DRAWRANK
		INC FRAME		; anim counter
		LDY #$00
		JSR SSSFLIP
@scan:	JSR GETIN		; get keyboard
		CMP #$85		; got F1 ?
		BEQ GOODBYE		; try again ...
		LDA #$FF
		STA $9122
		LDA $9111
		AND #$20		; FIRE
		BEQ GOODBYE
		JMP ENEMY

;*********************************************************************
; FAREWELL TO THIS GAMING SESSION
;
GOODBYE:
		LDA #SSSNULL
		JSR SSSCLEAR
		LDY #$00		; render immediate
		JSR SSSFLIP
		JMP RESTART

;*********************************************************************
; COMPLETED THIS LEVEL'S OBJECTIVE
;
FINILEVEL:
		LDA #$C0
		STA FRAME		; allow for current sequencing to complete
@loop:
		LDX #$00
		STX LOOPX
@hero:	LDA BOLT,X
		BEQ @next
		JSR OLDBOLT
@next:	INC LOOPX
		LDX LOOPX
		CPX #$03
		BNE @hero
@hits:
		LDA HITMARK
		BEQ @nohit
		JSR OLDHITMARK
@nohit:	LDA BIGHITMARK
		BEQ @nobig
		JSR OLDBIGHITMARK
@nobig:	LDA JUMPMARK
		BEQ @nojump
		JSR OLDJUMPMARK
@nojump:
		LDY #$01
		JSR SSSFLIP
		INC FRAME
		BNE @loop
		;
		LDX #$00
		STX HEARTBEATS
@mt:	LDA SPRITEDEF,X
		AND #$7F		; disable sprite
		STA SPRITEDEF,X
		INX
		CPX SPRITES
		BNE @mt
		LDY #$00
		JSR SSSFLIP
		;
		INC MISSION
		LDA MISSION
		CMP #$10
		BCC @ok
		LDA #$0F
		STA MISSION
@ok:	LDA MISSIONX
		CLC
		SED
		ADC #$01
		CLD
		STA MISSIONX
		;
		LDX #$00
		STX ALERTS
		STX BOOMED
		STX VIC+$0B
		STX VIC+$0D
		STX LOOPX
		LDA MISSIONX
		CMP #$01		; SCOUT
		BEQ @tune
		CMP #$03		; DEFENDER
		BEQ @tune
		CMP #$06		; WARRIOR
		BEQ @tune
		CMP #$10		; CENTURION
		BEQ @tune
		CMP #$15		; PROTECTOR
		BEQ @tune
		JMP NEXTLEVEL
		;
@tune:
		LDY OBJECTIVE
		BNE @chorus
		LSR
		ASL
		ASL
		ASL
		ASL
		CLC
		SED
		ADC SCORE+1		; 1000s of bonus
		BCC @nocc
		INC SCORE
@nocc:	STA SCORE+1
		CLD
		JSR SCORESTATUS
@chorus:
		LDA #221		; B
		STA VIC+$0C
		LDA #0
		CPX #$01
		BCC @s1
		LDA #230		; e
@s1:	STA VIC+$0A
		LDY #$07
		JSR SSSFLIP
		LDA #217		; A
		STA VIC+$0C
		LDA #0
		STA VIC+$0A
		LDY #$07
		JSR SSSFLIP
		LDA #221		; B
		STA VIC+$0C
		CPX #$01
		BCC @s2
		LDA #221		; b
@s2:	STA VIC+$0A
		LDY #$07
		JSR SSSFLIP
		LDA #223		; C
		STA VIC+$0C
		LDA #0
		STA VIC+$0A
		LDY #$07
		JSR SSSFLIP
		LDA #226		; D
		STA VIC+$0C
		LDA #217		; a high
		STA VIC+$0A
		LDY #$1C
		JSR SSSFLIP
		LDA #213		; G
		STA VIC+$0C
		LDA #179		; a low
		STA VIC+$0A
		LDY #$0E
		JSR SSSFLIP
		INC LOOPX
		LDX LOOPX
		CPX #$03
		BNE @chorus
		;
		LDA #191		; c
		STA VIC+$0A
		LDA #221		; B
		STA VIC+$0C
		LDY #$07
		JSR SSSFLIP
		LDA #217		; A
		STA VIC+$0C
		LDY #$07
		JSR SSSFLIP
		LDA #221		; B
		STA VIC+$0C
		LDY #$07
		JSR SSSFLIP
		LDA #223		; C
		STA VIC+$0C
		LDY #$07
		JSR SSSFLIP
		LDA #217		; A
		STA VIC+$0C
		LDY #$1C
		JSR SSSFLIP
		;
		LDA #0
		STA VIC+$0C
		STA VIC+$0A
		JMP NEXTLEVEL

;*********************************************************************
; Fury attempts to fire a new energy bolt
;
NEWBOLT:
		LDA ENERGIZER
		CMP BOLTE+2
		BCS @ok
@mt:	LDA #$08
		STA EMPTY
		RTS
		;
@ok:	LDX #$00
@find:	LDA BOLT,X
		BEQ @ready
		INX
		CPX #$03
		BNE @find
		BEQ @mt
		;
@ready:	STX sssNUM
		LDY #$00
		LDA ENERGIZER
@bolte:	CMP BOLTE,Y
		BCS @fire
		INY
		CPY #$03
		BNE @bolte
@fire:	SEC
		SBC BOLTE,Y
		STA ENERGIZER
		LDA BOLTC,Y
		STA SPRITECOL,X
		INY
		TYA
		STA BOLT,X
		;
		LDA DIRECTION
		STA BOLTDIR,X
		TAY
		LDA SPRITEDEF,X
		ORA #$80		; enable new sprite
		STA SPRITEDEF,X
		LDA BOLTX,Y		; position new shot
		CLC
		ADC SPRITEX+12
		AND #$FE
		TAX
		LDA BOLTY,Y
		CLC
		ADC SPRITEY+12
		AND #$FE
		TAY
		JSR SSSMOVEXY
		LDA #$FD		; new FIRE event
		STA SHOOTING
		RTS

;*********************************************************************
; Process Fury fired shot
; pass X with sprite #
;
OLDBOLT:
		STX sssNUM
		STA R0
		TAY
		DEY
		LDA BOLTC,Y

		LDX #<BOLTIMG0
		LDY #>BOLTIMG0
		JSR SSSANIM
		LDX sssNUM
		LDA FRAME
		AND #$08
		BEQ @even
		;
		LDA R0
		ASL
		CLC
		ADC R0
		CLC
		ADC SPRITEIMGL,X
		BCC @nocc
		INC SPRITEIMGH,X
@nocc:	STA SPRITEIMGL,X
@even:
		LDA BOLTDIR,X
		TAY
		LDA DDX2,Y
		BEQ @ddy2
		CMP #$01
		BEQ @ddx
		LDA FRAME
		AND #$01
		BEQ @ddy2
@ddx:	LDA NDDX,Y
		CLC
		ADC SPRITEX,X
		STA SPRITEX,X
		CMP #$10
		BCS @okleft
		JMP @xout
@okleft:
		CLC
		ADC #$02
		CMP SSSCLIPX
		BCC @okright
		JMP @xout
@okright:
@ddy2:	LDA DDY2,Y
		BEQ @fini
		CMP #$01
		BEQ @ddy
		LDA FRAME
		AND #$01
		BEQ @fini
@ddy:	LDA NDDY,Y
		CLC
		ADC SPRITEY,X
		STA SPRITEY,X
		CMP #$10
		BCS @oktop
		JMP @xout
@oktop:	CLC
		ADC #$0A
		CMP SSSCLIPY
		BCC @okbottom
		JMP @xout
@okbottom:
@fini:
		LDA SPRITEY,X
		SEC
		SBC #$0F		; hcenter of bolt counts ($10 - 1)
		LSR
		LSR
		LSR
		TAY
		LDA SPRITEX,X
		SEC
		SBC #$0F		; vcenter of bolt counts ($10 - 1)
		LSR
		LSR
		LSR
		TAX
		JSR SSSPEEKS
		CMP #$80
		BCC @object
		CMP #$C0
		BCS @xout
		LDX sssNUM
		JSR NMEHIT
		BCS @xout
		RTS
@object:
		LDX CRSRCOL
		LDY CRSRROW
		JSR FINDMINE
		CPX #$10
		BCS @xout
		LDA MINES,X
		CMP #$04		; is it a heart-stopper?
		BCC @mine
		LDA R0
		CMP #$01
		BNE @xout		; was it hit by a red bolt?
		DEC HEARTBEATS
@mine:	LDA #$00		; eradicate
		STA MINES,X
		DEC OBJECTIVE
		LDA CRSRCHAR	; retrieve last peeked cell
		PHA				;++
		LDX CRSRCOL
		LDY CRSRROW
		JSR SSSPLOT
		LDA #SSSNULL
		JSR SSSPOKE
		LDA #$FD
		STA HIT
		PLA				;--
		TAY
		JSR SCOREUPDATE
@xout:
		LDX sssNUM		; restore sprite #
		LDA #$00
		STA BOLT,X
		LDA SPRITEDEF,X
		AND #$7F		; disable sprite
		STA SPRITEDEF,X
		LDA SPRITEY,X
		TAY
		DEY
		DEY
		LDA SPRITEX,X
		TAX
		DEX
		DEX
		JSR NEWHITMARK
		RTS

;*********************************************************************
; check if bolt hit an enemy ship
;
NMEHIT:
		TXA
		PHA				;++
		LDY #$00
		STY R4			; current enemy ship #0-3
@loop:	LDA NME,Y
		BNE @nme
		JMP @next
@nme:	LDA SPRITEX,X
		CLC
		ADC #$02		; adjust for bolt width
		CMP SPRITEX+8,Y
		BCC @next
		SBC SPRITEX+8,Y
		CMP SPRITEH+8,Y
		BCC @y
		SBC #$02		; adjust for bolt width
		CMP SPRITEH+8,Y
		BCS @next
@y:		LDA SPRITEY,X
		CLC
		ADC #$02		; adjust for bolt height
		CMP SPRITEY+8,Y
		BCC @next
		SBC SPRITEY+8,Y
		CMP SPRITEH+8,Y
		BCC @hit
		SBC #$02		; adjust for bolt height
		CMP SPRITEH+8,Y
		BCS @next
@hit:	LDA #$F0
		STA HIT
		PLA				;--
		TAX
		STX XCOPY
		LDA BOLT,X
		TAY
		LDX R4
		LDA NMEHP,X
		SEC
		SBC HULLV-1,Y
		BCS @alive
		;
@score:	LDA NME,X
		TAY
		LDA VALUE-1,Y
		CLC
		SED
		ADC SCORE+1
		BCC @nocc
		INC SCORE
@nocc:	STA SCORE+1
		CLD
		JSR SCORESTATUS
		LDX R4
		LDA #$00
@alive:	STA NMEHP,X
		BNE @done
		STA NME,X
		LDA SPRITEDEF+8,X
		AND #$7F		; disable sprite
		STA SPRITEDEF+8,X
		DEC OBJECTIVE
@done:
		LDX XCOPY
		SEC
		RTS
		;
@next:	INC R4
		LDY R4
		CPY #$04
		BEQ @xnme
		JMP @loop
@xnme:
		PLA
		TAX
		CLC
		RTS

;*********************************************************************
; plot a new hit mark
; pass X/Y
;
NEWHITMARK:
		CPX #$10
		BCS @ok1
		LDX #$0D
@ok1:	CPX #$B4
		BCC @ok2
		LDX #$B4
@ok2:	CPY #$10
		BCS @ok3
		LDY #$0D
@ok3:	LDA #$0D
		STA sssNUM
		JSR SSSMOVEXY
		LDA #%10001100
		STA SPRITEDEF+13
		LDA #$02		; red
		LDX #<HITMARK0
		LDY #>HITMARK0
		JSR SSSANIM
		LDA #$03
		STA HITMARK
		RTS

;*********************************************************************
; dissipate old hit mark
;
OLDHITMARK:
		LDA FRAME
		AND #$03
		BNE @fini
		LDX #$0D
		STX sssNUM
		DEC HITMARK
		BNE @cont
		AND #$7F
		STA SPRITEDEF,X
		BNE @fini
@cont:
		LDA HITMARK
		AND #$01
		BNE @h0
@h1:	LDX #<HITMARK1
		LDY #>HITMARK1
		LDA #$07		; yellow
		BNE @anim
@h0:	LDX #<HITMARK0
		LDY #>HITMARK0
		LDA #$02		; red
@anim:	JSR SSSANIM
@fini:	RTS

;*********************************************************************
; plot a new BIG hit mark
; pass X/Y
;
NEWBIGHITMARK:
		CPX #$10
		BCS @ok1
		LDX #$0D
@ok1:	CPX #$B4
		BCC @ok2
		LDX #$B4
@ok2:	CPY #$10
		BCS @ok3
		LDY #$0D
@ok3:	LDA #$0E
		STA sssNUM
		JSR SSSMOVEXY
		LDA #%10001111
		STA SPRITEDEF,X
		LDA #$02		; red
		LDX #<BIGHITMARK0
		LDY #>BIGHITMARK0
		JSR SSSANIM
		LDA #$03
		STA BIGHITMARK
		RTS

;*********************************************************************
; dissipate old BIG hit mark
;
OLDBIGHITMARK:
		LDA FRAME
		AND #$03
		BNE @fini
		LDX #$0E
		STX sssNUM
		DEC BIGHITMARK
		BNE @cont
		AND #$7F		; disable sprite
		STA SPRITEDEF,X
		LDA #$0A		; black with red
		STA VIC+$0F		; screen and border
		BNE @fini
@cont:
		LDA BIGHITMARK
		AND #$01
		BNE @h0
@h1:	LDX #<BIGHITMARK1
		LDY #>BIGHITMARK1
		LDA #$01		; white
		BNE @anim
@h0:	LDX #<BIGHITMARK0
		LDY #>BIGHITMARK0
		LDA #$02		; red
@anim:	JSR SSSANIM
@fini:	RTS

;*********************************************************************
; plot a new JUMP mark
; pass X/Y
;
NEWJUMPMARK:
		LDA #$0E		; re-use big hitmark sprite
		STA sssNUM
		LDA #%10001111
		STA SPRITEDEF+14
		JSR SSSMOVEXY
		LDA #$01		; white
		LDX #<BIGHITMARK1
		LDY #>BIGHITMARK1
		JSR SSSANIM
		LDA #$05
		STA JUMPMARK
		RTS

;*********************************************************************
; dissipate old JUMP mark
;
OLDJUMPMARK:
		LDA FRAME
		AND #$07
		BNE @fini
		LDX #$0E
		STX sssNUM
		DEC JUMPMARK
		BNE @cont
		LDA SPRITEDEF,X
		AND #$7F
		STA SPRITEDEF,X
		LDX WARPED
		LDA NME,X
		BEQ @fini		; warped out
		LDA SPRITEDEF+8,X
		ORA #$80		; warping in
		STA SPRITEDEF+8,X
		BNE @fini
@cont:
		LDX WARPED
		LDA SPRITECOL+8,X
		STA @anim+1		; use Thargoid hull color
		LDA JUMPMARK
		AND #$01
		BNE @h0
@h1:	LDX #<BIGHITMARK1
		LDY #>BIGHITMARK1
		BNE @anim
@h0:	LDX #<BIGHITMARK0
		LDY #>BIGHITMARK0
@anim:	LDA #$01		; white
		JSR SSSANIM
@fini:	RTS

;*********************************************************************
; Thargoid ship attempts to fire a phaser shot
;
; Pass A with phaser type (1-4)
; Pass Y with direction (0-15)
; Pass LOOPX with Thargoid ship (0-3)
;
NEWPHASER:
		PHA				;++
		TYA
		PHA				;++
		LDX #$00
@find:	LDA NMESHOT,X
		BEQ @fire
		INX
		CPX #$04
		BNE @find
		PLA				;--
		TAY
		PLA				;--
		RTS
@fire:
		PLA				;--
		STA NMESHOTDIR,X
		PLA				;--
		STA NMESHOT,X
		TXA
		CLC
		ADC #$03
		STA sssNUM		; sprite #3-6
		TAY
		LDA SPRITEDEF,Y
		ORA #$80		; enable new sprite
		STA SPRITEDEF,Y
		STX XCOPY
		LDA NMESHOT,X
		TAX				; load phaser type for sprite
		ASL
		ASL
		ASL				; x8
		STA PHASER		; sound event
		LDA #<PHASER0
		LDY #>PHASER0
		DEX
		BEQ @type
@loop:	CLC
		ADC #$03
		BCC @nocc
		INY
@nocc:	DEX
		BNE @loop
@type:	TAX
		LDA #$01		; WHITE
		JSR SSSANIM
		LDX XCOPY
		LDA NMESHOTDIR,X
		LDX LOOPX
		TAY
		LDA BOLTX,Y		; position new shot
		CLC
		ADC SPRITEX+8,X
		STA XCOPY
		LDA BOLTY,Y
		CLC
		ADC SPRITEY+8,X
		TAY
		LDX XCOPY
		JSR SSSMOVEXY
		RTS

;*********************************************************************
; Process Thargoid phaser shot
;
; pass LOOPX with shot #
;
OLDPHASER:
		LDA NMESHOTDIR,X
		TAY
		TXA
		CLC
		ADC #$03
		TAX
		STX sssNUM
		LDA DDX2,Y
		BEQ @ddy2
		CMP #$01
		BEQ @ddx
		LDA FRAME
		AND #$01
		BEQ @ddy2
@ddx:	LDA NDDX,Y
		CLC
		ADC SPRITEX,X
		STA SPRITEX,X
		CMP #$10
		BCS @okleft
		JMP @xout
@okleft:
		CLC
		ADC #$02
		CMP SSSCLIPX
		BCC @okright
		JMP @xout
@okright:
@ddy2:	LDA DDY2,Y
		BEQ @fini
		CMP #$01
		BEQ @ddy
		LDA FRAME
		AND #$01
		BEQ @fini
@ddy:	LDA NDDY,Y
		CLC
		ADC SPRITEY,X
		STA SPRITEY,X
		CMP #$10
		BCS @oktop
		JMP @xout
@oktop:	CLC
		ADC #$0A
		CMP SSSCLIPY
		BCC @okbottom
		JMP @xout
@okbottom:
@fini:
		LDA SPRITEY,X
		TAY
		LDA SPRITEX,X
		TAX
		JSR SSSMOVEXY
		LDX sssNUM
		LDA SPRITEY,X
		SEC
		SBC #$10
		LSR
		LSR
		LSR
		TAY
		LDA SPRITEX,X
		SEC
		SBC #$10
		LSR
		LSR
		LSR
		TAX
		JSR SSSPEEKS
		TAY
		CPY #$C0
		BCS @shields
		JSR PLAYERHIT
		BCS @hit
		RTS
@shields:
		LDA #$FD
		STA HIT
		LDX LOOPX
		LDA NMESHOT,X
		PHA
		JSR @xout
		PLA
		CLC
		ADC #$01
		ASL
		ASL				; x4 damage
		STA ACOPY
		LDA OMEGAHP
		CMP ACOPY
		BNE @more
		INC ACOPY
@more:	SEC
		SBC ACOPY
		BCS @nocc
		JSR KILLOBJECTIVE
		LDA #$00		; no more fuel for the Fury ...
@nocc:	STA OMEGAHP		; the end is near!
		JSR DRAWSHIELDS	; if any
		RTS
@hit:	; shot Omega Fury
		LDA #$F8
		STA HIT
		LDX LOOPX
		LDA NMESHOT,X
		PHA
		JSR @xout
		PLA
		ASL
		ASL
		ASL				; x8 damage
		SEC
		SBC #$02
		JSR FURYHIT
		LDA FRAME
		AND #$07
		JSR FURYNAVI
		RTS
@xout:
		LDX LOOPX
		LDA #$00
		STA NMESHOT,X
		LDX sssNUM		; restore sprite #
		LDA SPRITEDEF,X
		AND #$7F		; disable sprite
		STA SPRITEDEF,X
		LDA SPRITEY,X
		TAY
		DEY
		DEY
		LDA SPRITEX,X
		TAX
		DEX
		DEX
		JSR NEWHITMARK
		RTS

;*********************************************************************
; Thargoid launches a deadly neutron bomb at Omegan objective
;
; Pass X/Y with starting coordinates
;
NEWNEUTRONBOMB:
		STX SPRITEX+7
		STY SPRITEY+7
		LDA #$07
		STA NEUTRON
		STA sssNUM
		LDA #%10001100
		STA SPRITEDEF+7
		LDA #$04		; MAGENTA
		LDX #<NEUTRONBOMB
		LDY #>NEUTRONBOMB
		JSR SSSANIM
		RTS

;*********************************************************************
; Process launched neutron bomb
;
OLDNEUTRONBOMB:
		LDX #$07
		STX sssNUM
		;
		LDA NEUTRONDIR
		TAY
		LDA DDX2,Y
		BEQ @ddy2
		CMP #$01
		BEQ @ddx
		LDA FRAME
		AND #$01
		BEQ @ddy2
@ddx:	LDA DDX,Y
		CLC
		ADC SPRITEX+7
		STA SPRITEX+7
		CMP #$10
		BCS @okleft
		JMP @xout
@okleft:
		CLC
		ADC #$02
		CMP SSSCLIPX
		BCC @okright
		JMP @xout
@okright:
@ddy2:	LDA DDY2,Y
		BEQ @fini
		CMP #$01
		BEQ @ddy
		LDA FRAME
		AND #$01
		BEQ @fini
@ddy:	LDA DDY,Y
		CLC
		ADC SPRITEY+7
		STA SPRITEY+7
		CMP #$10
		BCS @oktop
		JMP @xout
@oktop:	CLC
		ADC #$0A
		CMP SSSCLIPY
		BCC @okbottom
		JMP @xout
@okbottom:
@fini:
		LDX SPRITEX+7
		LDY SPRITEY+7
		JSR SSSMOVEXY
		LDX SPRITEX+7
		LDY SPRITEY+7
		JSR SSSPEEKXY
		CMP #$C0
		BCS @shields
		RTS
@shields:
		LDA #$FD
		STA HIT
		JSR @xout
		LDA OMEGAHP
		CMP #$80
		BNE @more
		DEC OMEGAHP
@more:	SEC
		SBC #$80
		BCS @nocc
		JSR KILLOBJECTIVE
		LDA #$00		; no more fuel for the Fury ...
@nocc:	STA OMEGAHP		; the end is near!
		JSR DRAWSHIELDS	; if any
		RTS
@xout:
		LDX sssNUM		; restore sprite #
		LDA SPRITEDEF,X
		AND #$7F		; disable sprite
		STA SPRITEDEF,X
		LDA SPRITEY,X
		TAY
		DEY
		DEY
		LDA SPRITEX,X
		TAX
		DEX
		DEX
		JSR NEWBIGHITMARK
		LDA #$00
		STA NEUTRON
		RTS

;*********************************************************************
; check if phaser hit the Fury
;
PLAYERHIT:
		LDX LOOPX
		LDA SPRITEX+3,X
		CLC
		ADC #$02		; adjust for phaser width
		CMP SPRITEX+12
		BCC @xplayer
		SBC SPRITEX+12
		CMP #$08
		BCC @y
		SBC #$02		; adjust for phaser width
		CMP #$08
		BCS @xplayer
@y:		LDA SPRITEY+3,X
		CLC
		ADC #$02		; adjust for phaser height
		CMP SPRITEY+12
		BCC @xplayer
		SBC SPRITEY+12
		CMP #$08
		BCC @hit
		SBC #$02		; adjust for bolt height
		CMP #$08
		BCS @xplayer
@hit:	SEC				; a hit!
		RTS
@xplayer:
		CLC				; ok
		RTS

;*********************************************************************
; Omega Fury takes a hull hit!
; Pass A for damage points
;
FURYHIT:
		STA ACOPY
		LDA FURYHP
		SEC
		SBC ACOPY
		BCC @dead
		STA FURYHP
		BEQ @dead
		JSR HULLSTATUS
		RTS
@dead:	; RIP
		LDA #$00
		STA VIC+$0D
		JSR GAMEOVER
		LDA #$00
		STA FURYHP
		STA INERTIA
		STA SPEED
		JSR HULLSTATUS
		LDX SPRITEX+12
		DEX
		DEX
		DEX
		LDY SPRITEY+12
		DEY
		DEY
		DEY
		JSR NEWBIGHITMARK
		LDA SSSCLIPY
		LSR
		STA SPRITEX+12	; move out of the way
		STA SPRITEY+12	; so game over can complete
		LDA SPRITEDEF+12
		AND #$7F		; disable sprite
		STA SPRITEDEF+12
		LDA #$F0
		STA NAVI		; spin cycle
		LDA MISSION
		BEQ @fini		; academy training center is immune
		LDA OMEGAHP
		BEQ @fini		; already destroyed
		LDA #$04
		STA SHIPS		; fill with missing ships
		STA FRAME		; allow fury to explode
@fini:	RTS

;*********************************************************************
; Omega Fury navigations go offline
; Pass A for duration
;
FURYNAVI:
		CMP NAVI
		BCS @cont
		ADC NAVI
		BCC @cont
		LDA #$F0
@cont:	STA NAVI
		BNE @good
		INC NAVI
@good:	LDA #$00
		STA ACTIONS
		INC INERTIA
		LDA JIFFYL
		AND #$01
		BNE @right
		DEC INERTIA
		DEC INERTIA
		LDA #$04
@right:	STA SPIN
		LDA SPEED
		CMP #$02
		BCS @moving
		LDA FRAME
		STA INERTIA
		LDA #$02
		STA SPEED
@moving:
		LDA INERTIA
		AND #$0F
		STA INERTIA
		RTS

;*********************************************************************
; Update tactical with Omega Fury's hull plating status
;
HULLSTATUS:
		LDY #$00
		LDA FURYHP
@dash1:	CMP HULLV,Y
		BCS @hull
		INY
		CPY #$03
		BNE @dash1
@hull:
		LDA HULLC,Y
		STA COLORCODE
@hi:
		CPY #$00
		BNE @mid
		LDA FURYHP
		CMP #$40
		BCS @full
		INY
@full:	STY ALERTS
		LDX #$00
		LDY #$17
		JSR DRAWCELLS
		.byte	$E6,$E6,$E6,$E6,$E6,$E6,$E6,$E6,$00
		LDY #$00		; render immediate
		JSR SSSFLIP
		LDA HULLC
		STA COLORCODE
		LDX #$00
		LDY #$17
		JSR DRAWCELLS	; OK
		.byte	$E6,$E6,$E6,$8F,$8B,$E6,$E6,$E6,$00
		JMP @fini
@mid:
		CPY #$01
		BNE @low
		INY
		STY ALERTS
		LDX #$00
		LDY #$17
		JSR DRAWCELLS
		.byte	$E6,$E8,$E8,$E8,$E8,$E8,$E8,$E6,$00
		LDY #$00		; render immediate
		JSR SSSFLIP
		LDA HULLC+1
		STA COLORCODE
		LDX #$00
		LDY #$17
		JSR DRAWCELLS	; DAMAGE
		.byte	$E6,$84,$81,$8D,$81,$87,$85,$E6,$00
		JMP @fini
@low:
		CPY #$02
		BNE @mt
@ng:	INY
		STY ALERTS
		LDX #$00
		LDY #$17
		JSR DRAWCELLS
		.byte	$A0,$E8,$E8,$E8,$E8,$E8,$E8,$A0,$00
		LDY #$00		; render immediate
		JSR SSSFLIP
		LDA HULLC+2
		STA COLORCODE
		LDX #$00
		LDY #$17
		JSR DRAWCELLS	; CRITICAL
		.byte	$83,$92,$89,$94,$89,$83,$81,$8C,$00
		JMP @fini
@mt:
		LDA FURYHP
		BNE @ng
@dead:
		LDA #$01
		STA COLORCODE
		LDX #$00
		LDY #$17
		JSR DRAWCELLS	; EJECT!
		.byte	$A0,$85,$8A,$85,$83,$94,$A1,$A0,$00
		LDY #$00		; render immediate
		JSR SSSFLIP
		LDA #$07
		STA COLORCODE
		LDX #$00
		LDY #$17
		JSR DRAWCELLS	; EJECT!
		.byte	$A0,$85,$8A,$85,$83,$94,$A1,$A0,$00
@fini:	LDY #$00		; render immediate
		JSR SSSFLIP
		RTS

;*********************************************************************
; Update tactical with Omega Fury's weapon status
;
WEAPONSTATUS:
		LDY #$00
		LDA ENERGIZER
@dash1:	CMP BOLTE,Y
		BCS @energy
		INY
		CPY #$03
		BNE @dash1
@energy:
		LDA BOLTC,Y
		STA COLORCODE
@hi:
		CPY #$00
		BNE @mid
		LDX #$08
		LDY #$17
		JSR DRAWCELLS
		.byte	$E6,$E6,$E6,$E6,$E6,$00
		RTS
@mid:
		CPY #$01
		BNE @low
		LDX #$08
		LDY #$17
		JSR DRAWCELLS
		.byte	$E8,$E8,$E8,$E8,$E8,$00
		RTS
@low:
		CPY #$02
		BNE @mt
		LDX #$08
		LDY #$17
		JSR DRAWCELLS
		.byte	$A0,$E8,$E8,$E8,$A0,$00
		RTS
@mt:
		LDX #$08
		LDY #$17
		JSR DRAWCELLS
		.byte	$85,$8D,$90,$94,$99,$00	; EMPTY
		RTS

;*********************************************************************
; Show player's score
;
SCORESTATUS:
		LDA #$01
		STA COLORCODE
		LDX #$0E
		LDY #$17
		JSR SSSPLOT
		LDX #$00
@loop:	LDA SCORE,X
		LSR
		LSR
		LSR
		LSR
		ORA #$B0
		JSR SSSPRINT
		LDA SCORE,X
		AND #$0F
		ORA #$B0
		JSR SSSPRINT
		INX
		CPX #$03
		BNE @loop
		RTS

;*********************************************************************
; Update player's score
; Send Y as index
;
SCOREUPDATE:
		CPY #$04
		BCS @g1
		LDA VALUE,Y
		BNE @score
@g1:	CPY #$0B		; grenade
		BCS @g2
		LDA #$10
		BNE @score
@g2:	CPY #$0D		; hull blaster
		BCS @g3
		LDA #$20
		BNE @score
@g3:	CPY #$0F		; sonic boomer
		BCS @g4
		LDA #$40
		BNE @score
@g4:	CPY #$11		; heart stopper
		BCS @x
		LDA #$80
		BNE @score
@x:		RTS
@score:
		CLC
		SED
		ADC SCORE+2
		STA SCORE+2
		BCC @nocc
		LDA SCORE+1
		CLC
		ADC #$01
		STA SCORE+1
		BCC @nocc
		LDA SCORE
		CLC
		ADC #$01
		STA SCORE
@nocc:	CLD
		JSR SCORESTATUS
		RTS

;*********************************************************************
; Print STATIC CELL data from PC stored on stack
;
DRAWCELLS:
		PLA
		STA VECTORFG
		PLA
		STA VECTORFG+1
		JSR SSSPLOTS
		LDY #$01
		STY R0
@loop:	LDA (VECTORFG),Y
		BEQ @fini
		LDX CRSRCOL
		LDY CRSRROW
		JSR SSSCELL
		INC CRSRCOL
		INC R0
		LDY R0
		BNE @loop
		;
@fini:	TYA
		CLC
		ADC VECTORFG
		BCC @nocc
		INC VECTORFG+1
@nocc:	STA VECTORFG
		LDA VECTORFG+1
		PHA
		LDA VECTORFG
		PHA
		RTS

;*********************************************************************
; Render Omegan Objective at the center of the screen
; Pass A for objective
;
DRAWOBJECTIVE:
		LDA MISSION
@mission0:
		CMP #$01
		BCS @mission1
		LDA #$00
		STA TITLE
		LDA #$FF		; light yellow & highest
		STA VIC+$0E		; auxiliary color & volume
		LDA #$0E
		STA COLORCODE
		LDX #<ACADEMY
		LDY #>ACADEMY
		STX $01
		STY $02
		JMP @copy
@mission1:
		CMP #$03
		BCS @mission2
		LDA #$01
		STA TITLE
		LDA #$EF		; light blue & highest
		STA VIC+$0E		; auxiliary color & volume
		LDA #$0E
		STA COLORCODE
		LDX #<STARGATE
		LDY #>STARGATE
		STX $01
		STY $02
		JMP @copy
@mission2:
		CMP #$06
		BCS @mission3
		LDA #$02
		STA TITLE
		LDA #$FF		; light yellow & highest
		STA VIC+$0E		; auxiliary color & volume
		LDA #$0D		; green
		STA COLORCODE
		LDX #<CARGOSHIP
		LDY #>CARGOSHIP
		STX $01
		STY $02
		JMP @copy
@mission3:
		CMP #$0A
		BCS @mission4
		LDA #$03
		STA TITLE
		LDA #$AF		; light red & highest
		STA VIC+$0E		; auxiliary color & volume
		LDA #$09
		STA COLORCODE
		LDX #<MEDICALSHIP
		LDY #>MEDICALSHIP
		STX $01
		STY $02
		JMP @copy
@mission4:
		CMP #$0F
		BCS @mission5
		LDA #$04
		STA TITLE
		LDA #$EF		; light blue & highest
		STA VIC+$0E		; auxiliary color & volume
		LDA #$0F
		STA COLORCODE
		LDX #<WEAPONSHIP
		LDY #>WEAPONSHIP
		STX $01
		STY $02
		JMP @copy
@mission5:
		LDA #$05
		STA TITLE
		LDA #$EF		; light blue & highest
		STA VIC+$0E		; auxiliary color & volume
		LDA #$0F
		STA COLORCODE
		LDX #<COLONISTS
		LDY #>COLONISTS
		STX $01
		STY $02
		JMP @copy
@copy:
		LDY #$00
@img:	LDA ($01),Y
		STA $1C00,Y
		INY
		CPY #$48		; 3x3 matrix
		BNE @img
		;
		LDX #$09
		STX R0
		LDA #$00		; start with this character code
		STA R2
@y:		LDY #$0A
		STY R1
@draw:	JSR SSSPLOT
		LDA R2
		JSR SSSPOKE
		INC R2
		INC R1
		LDY R1
		LDX R0
		CPY #$0D
		BNE @draw
		INX
		STX R0
		CPX #$0C
		BNE @y
		RTS

;*********************************************************************
; Draw Omegan Objective's shields
;
DRAWSHIELDS:
		LDA OMEGAHP
		BNE @shields
		RTS
@shields:
		LDX #$01		; WHITE
		CMP #$D0
		BCS @color
		LDX #$03		; CYAN
		CMP #$90
		BCS @color
		LDX #$07		; YELLOW
		CMP #$50
		BCS @color
		LDX #$02		; RED
		CMP #$10
		BCS @color
		LDX #$00		; BLACK
@color:	STX COLORCODE
		LDX #$08
		LDY #$09
		JSR SSSPLOT
		LDA #$D5
		JSR SSSPRINT
		LDX #$02
		STX R0
@top:	LDA #$C0
		JSR SSSPRINT
		DEC R0
		LDX R0
		BPL @top
		LDA #$C9
		JSR SSSPRINT
		LDY CRSRROW
		INY
		STY R0
		;
@mid:	LDX #$08
		JSR SSSPLOT
		LDA #$DD
		JSR SSSPRINT
		LDX #$0C
		LDY R0
		JSR SSSPLOT
		LDA #$DD
		JSR SSSPRINT
		INC R0
		LDY R0
		CPY #$0D
		BNE @mid
		;
		LDX #$08
		JSR SSSPLOT
		LDA #$CA
		JSR SSSPRINT
		LDX #$02
@bot:	LDA #$C0
		JSR SSSPRINT
		DEX
		BPL @bot
		LDA #$CB
		JSR SSSPRINT
		;
		RTS

;*********************************************************************
; Destroy Omegan Objective at the center of the screen
;
KILLOBJECTIVE:
		LDA MISSION
		BEQ @fini		; let's not destroy the academy over one bad cadet
		LDA OMEGAHP
		BEQ @fini		; sanity check -- already destroyed
		; light show first
		LDA #$1A		; white / red
		STA VIC+$0F
		LDA #$F8
		STA HIT
		LDX #$02
		STX LOOPX
		; explode
@loop:	LDX #(11*8)+5
		LDY #(12*8)+4
		JSR NEWBIGHITMARK
		LDY #$02
		JSR SSSFLIP
		INC FRAME
		; dissipate
@fade:	JSR OLDBIGHITMARK
		LDY #$02
		JSR SSSFLIP
		INC FRAME
		LDA BIGHITMARK
		BNE @fade
		DEC LOOPX
		BNE @loop
		; bite the dust
		LDA #$00
		STA OMEGAHP
		LDX #$08
		STX R0
@y:		LDY #$09
		STY R1
@draw:	JSR SSSPLOT
		LDA #SSSNULL
		JSR SSSPOKE
		INC R1
		LDY R1
		LDX R0
		CPY #$0E
		BNE @draw
		INX
		STX R0
		CPX #$0D
		BNE @y
		; incinerate
		LDA #$E0
		STA HIT
		LDX #(11*8)+5
		LDY #(12*8)+4
		JSR NEWBIGHITMARK
		LDA #$1F
		STA NAVI		; fury momentarily loses control
@fini:	RTS

;*********************************************************************
; Display player's RANK
;
DRAWRANK:
		LDA #$04
		STA COLORCODE
		LDA TITLE
		ASL
		TAX
		LDA TITLES,X
		STA $8E
		LDA TITLES+1,X
		STA $8F
		LDY #$00
		LDA ($8E),Y
		TAX
		LDY #$11
		JSR SSSPLOT
		LDY #$01
@title:	LDA ($8E),Y
		BEQ @xtitle
		JSR SSSPRINT
		INY
		BNE @title
@xtitle:
		RTS

;*********************************************************************
; Find a mine in the array
; Pass X/Y coordinate to check for,
; returning result in X
;
FINDMINE:
		STX XCOPY
		STY YCOPY
		LDX #$00
@loopx:	LDA MINES,X
		BEQ @next
		LDA MINEX,X
		CMP XCOPY
		BNE @next
		LDA MINEY,X
		CMP YCOPY
		BEQ @fini
@next:	INX
		CPX #$10
		BNE @loopx
@fini:	RTS

;*********************************************************************
; Plot a grenade mine
; Pass X/Y
;
DROPGRENADE:
		TXA
		PHA
		TYA
		PHA
		JSR SSSPEEKS
		CMP #SSSNULL
		BEQ @mt
		PLA
		PLA
		RTS
@mt:
		LDX #$00
@loop:	LDA MINES,X
		BEQ @slot
		INX
		CPX #$10
		BNE @loop
		PLA
		PLA
		RTS
@slot:
		TXA
		PHA
		LDA #$05
		STA COLORCODE
		LDA #$09
		LDX CRSRCOL
		LDY CRSRROW
		JSR SSSCELL
		LDY #$00		; render immediate
		JSR SSSFLIP
		PLA
		TAX
		LDA #$01
		STA MINES,X
		PLA
		STA MINEY,X
		TAY
		PLA
		STA MINEX,X
		TAX
		LDA #$05
		STA COLORCODE
		LDA #$0A
		JSR SSSCELL
		LDA #$04
		STA DROPPED
		INC OBJECTIVE
		RTS

;*********************************************************************
; Plot a hull blaster mine
; Pass X/Y
;
DROPHULLBLASTER:
		TXA
		PHA
		TYA
		PHA
		JSR SSSPEEKS
		CMP #$A0
		BEQ @mt
		PLA
		PLA
		RTS
@mt:
		LDX #$00
@loop:	LDA MINES,X
		BEQ @slot
		INX
		CPX #$10
		BNE @loop
		PLA
		PLA
		RTS
@slot:
		TXA
		PHA
		LDA #$01
		STA COLORCODE
		LDA #$0B
		LDX CRSRCOL
		LDY CRSRROW
		JSR SSSCELL
		LDY #$00		; render immediate
		JSR SSSFLIP
		PLA
		TAX
		LDA #$02
		STA MINES,X
		PLA
		STA MINEY,X
		TAY
		PLA
		STA MINEX,X
		TAX
		LDA #$04
		STA COLORCODE
		LDA #$0C
		JSR SSSCELL
		LDA #$04
		STA DROPPED
		INC OBJECTIVE
		RTS

;*********************************************************************
; Plot a sonic boomer mine
; Pass X/Y
;
DROPSONICBOOMER:
		TXA
		PHA
		TYA
		PHA
		JSR SSSPEEKS
		CMP #SSSNULL
		BEQ @mt
		PLA
		PLA
		RTS
@mt:
		LDX #$00
@loop:	LDA MINES,X
		BEQ @slot
		INX
		CPX #$10
		BNE @loop
		PLA
		PLA
		RTS
@slot:
		TXA
		PHA
		LDA #$06
		STA COLORCODE
		LDA #$0D
		LDX CRSRCOL
		LDY CRSRROW
		JSR SSSCELL
		LDY #$00		; render immediate
		JSR SSSFLIP
		PLA
		TAX
		LDA #$03
		STA MINES,X
		PLA
		STA MINEY,X
		TAY
		PLA
		STA MINEX,X
		TAX
		LDA #$03
		STA COLORCODE
		LDA #$0E
		JSR SSSCELL
		LDA #$04
		STA DROPPED
		INC OBJECTIVE
		RTS

;*********************************************************************
; Plot a heart-stopper mine
; Pass X/Y
;
DROPHEARTSTOPPER:
		TXA
		PHA
		TYA
		PHA
		JSR SSSPEEKS
		CMP #SSSNULL
		BEQ @mt
		PLA
		PLA
		RTS
@mt:
		LDX #$00
@loop:	LDA MINES,X
		BEQ @slot
		INX
		CPX #$10
		BNE @loop
		PLA
		PLA
		RTS
@slot:
		TXA
		PHA
		LDA #$02
		STA COLORCODE
		LDA #$0F
		LDX CRSRCOL
		LDY CRSRROW
		JSR SSSCELL
		LDY #$00		; render immediate
		JSR SSSFLIP
		PLA
		TAX
		LDA #$04
		STA MINES,X
		PLA
		STA MINEY,X
		TAY
		PLA
		STA MINEX,X
		TAX
		LDA #$02
		STA COLORCODE
		LDA #$10
		JSR SSSCELL
		LDA #$04
		STA DROPPED
		STA PULSE
		INC OBJECTIVE
		INC HEARTBEATS
		RTS

;*********************************************************************
; Dispatch a new Thargoid mine
; send A = number of mines (1-16)
; send Y = mine type (1-4)
;
NEWMINE:
		STY R0
		STA R1
@loop:
		JSR RNDXY
@g1:	LDA R0
		CMP #$01
		BCS @g2
		JSR DROPGRENADE
		JMP @next
@g2:
		CMP #$02
		BCS @g3
		JSR DROPHULLBLASTER
		JMP @next
@g3:
		CMP #$03
		BCS @g4
		JSR DROPSONICBOOMER
		JMP @next
@g4:
		JSR DROPHEARTSTOPPER
@next:
		DEC R1
		BNE @loop
		RTS

;*********************************************************************
; Detonate a mine
; Pass X into MINES array
;
XMINE:
		LDA #$00
		STA MINES,X
		LDA MINEY,X
		PHA
		TAY
		LDA MINEX,X
		PHA
		TAX
		JSR SSSPLOT
		LDA #SSSNULL
		JSR SSSPOKE
		PLA
		ASL
		ASL
		ASL
		CLC
		ADC #$0C
		TAX
		PLA
		ASL
		ASL
		ASL
		CLC
		ADC #$0C
		TAY
		JSR NEWBIGHITMARK
		DEC OBJECTIVE
		LDA #$FD
		STA HIT
		RTS

;*********************************************************************
; Dispatch a new Thargoid ship
; send A = to type of ship
;
NEWTHARGOID:
		STA ACOPY
		LDX #$00
@find:	LDA NME,X
		BEQ @mt
		INX
		CPX #$04
		BNE @find
		RTS
@mt:
		STX WARPED
		LDA ACOPY
		STA NME,X
		TAY
		LDA #$08
		STA SPRITEH+8,X
		LDA #%10001100	; enable new sprite
		STA SPRITEDEF+8,X
		LDA ACOPY		; duration
		STA NMELEN,X
		LDA #$01		; travel
		STA NMEPLAN,X
		LDA #$02		; slow: 1 update per 8 frames
		STA NMESPEED,X
		LDA #$00		; straight
		STA NMETURN,X
		;
		LDA INNER,Y
		PHA				;++
		LDA OUTER,Y
		TAY
		LDA OTY,Y
		STA SPRITEY+8,X
		PLA				;--
		TAY
		LDA OLX,Y
		STA SPRITEX+8,X
		LDA ORX,Y
		SEC
		SBC SPRITEX+8,X
		LSR
		LSR
		LSR
		JSR RND
		LDX WARPED
		ASL
		ASL
		ASL
		CLC
		ADC SPRITEX+8,X
		STA SPRITEX+8,X
		LDA SPRITEY+8,X
		SEC
		SBC #$04
		TAY
		LDA SPRITEX+8,X
		SEC
		SBC #$04
		TAX
		JSR NEWJUMPMARK
		LDX WARPED
		LDA SPRITEX+8,X
		LDY #$04
		CMP #$60
		BCS @dir
		LDY #$0C
@dir:	TYA
		STA NMEDIR,X
		TXA
		ASL
		TAY
		LDA ACOPY
@s1:
		CMP #$01
		BNE @s2
		LDA #<SCOUT0
		STA SPRITEIMGL+8,X
		LDA #>SCOUT0
		STA SPRITEIMGH+8,X
		LDA #$5B		; 91 hull points
		STA NMEHP,X
		LDY MISSION
		BNE @real		; real mission?
		ASL	NMEHP,X		; training -- double thick hull
@real:	JMP @fini
@s2:
		CMP #$02
		BNE @s3
		LDA #$6A		; 106 hull points
		STA NMEHP,X
		JMP @fini
@s3:
		CMP #$03
		BNE @s4
		LDA #$79		; 121 hull points
		STA NMEHP,X
		JMP @fini
@s4:
		LDA #$10
		STA SPRITEH+8,X
		LDA #%10001111
		STA SPRITEDEF+8,X
		LDA #<CARRIER0
		STA SPRITEIMGL+8,X
		LDA #>CARRIER0
		STA SPRITEIMGH+8,X
		LDA #$C9		; 201 hull points
		STA NMEHP,X
@fini:
		LDA #$01		; start off as WHITE
		STA SPRITECOL+8,X
		INC OBJECTIVE
		LDA #$0A		; black with red
		STA VIC+$0F		; screen and border
		RTS

;*********************************************************************
; Thargoid SCOUT animation
; pass sssNUM with SPRITE #
;
SCOUT:
		LDX sssNUM
		LDA SPRITEX,X
		ORA SPRITEY,X
		AND #$01
		BNE @fini
		LDA SPRITEIMGL,X
		CLC
		ADC #$08
		BCC @nocc
		INC SPRITEIMGH,X
@nocc:	STA SPRITEIMGL,X
		CMP #<CRUISER0
		BNE @anim
		LDA #<SCOUT0
		STA SPRITEIMGL,X
		LDA #>SCOUT0
		STA SPRITEIMGH,X
@anim:	LDA SPRITEIMGH,X
		TAY
		LDA SPRITEIMGL,X
		TAX
		LDA #$05		; GREEN
		JSR SSSANIM
@fini:	RTS

;*********************************************************************
; Thargoid CRUISER move
; pass sssNUM with SPRITE #
;
CRUISER:
		TYA
		LSR				; 8-directions
		TAX
		LDA #<CRUISER0
		LDY #>CRUISER0
		CPX #$00
		BEQ @anim
@loop:	CLC
		ADC #$08
		BCC @nocc
		INY
@nocc:	DEX
		BNE @loop
@anim:	TAX
		LDA #$03		; CYAN
		JSR SSSANIM
		RTS

;*********************************************************************
; Thargoid DESTROYER move
; pass sssNUM with SPRITE #
;
DESTROYER:
		TYA
		LSR				; 8-directions
		TAX
		LDA #<DESTROYER0
		LDY #>DESTROYER0
		CPX #$00
		BEQ @anim
@loop:	CLC
		ADC #$08
		BCC @nocc
		INY
@nocc:	DEX
		BNE @loop
@anim:	TAX
		LDA #$07		; YELLOW
		JSR SSSANIM
		RTS

;*********************************************************************
; Thargoid CARRIER move
; pass sssNUM with SPRITE #
;
CARRIER:
		LDX sssNUM
		LDA SPRITEX,X
		ORA SPRITEY,X
		AND #$07
		BNE @fini
		LDA SPRITEIMGL,X
		CLC
		ADC #$20
		BCC @nocc
		INC SPRITEIMGH,X
@nocc:	STA SPRITEIMGL,X
		CMP #<FURY0
		BNE @anim
		LDA #<CARRIER0
		STA SPRITEIMGL,X
		LDA #>CARRIER0
		STA SPRITEIMGH,X
@anim:	LDA SPRITEIMGH,X
		TAY
		LDA SPRITEIMGL,X
		TAX
		LDA NMEPLAN+3
		BNE @w
		LDA FURYHP
		BEQ @w
		LDA #$02		; RED
		BNE @x
@w:		LDA #$01		; WHITE
@x:		JSR SSSANIM
@fini:	RTS

;*********************************************************************
; Seed the random number generator
; NOTE: floating point work here will corrupt DIRTYLINE2 in FAC#1
;
RANDOM = $033C
RSEED = $B9
RANDOMIZE:
		LDA #$00
		STA RSEED
		LDA $9124
		LDY $9125
		JSR $D391		; givayf: Convert Integer in (AC/YR) to Flpt
		LDX #$8B
		LDY #$00
		JSR $DBD7		; mov2f: Store FAC#1 in Memory
		RTS

;*********************************************************************
; Retrieve a random int
; Pass A with a CEILING value, i.e., 1 - 100
; returns A (0 thru CEILING-1)
;
RND:
		STA $BA
@retry:	JSR RNDNEXT
		CMP $BA
		BCS @retry
		RTS

;*********************************************************************
; Retrieve next random int
; returns A (0-99)
;
RNDNEXT:
		INC RSEED
		LDX RSEED
		CPX #$C0		; 192 buffered values
		BCC @cont
		LDX #$00
		STX RSEED
@cont:	LDA RANDOM,X
		RTS

;*********************************************************************
; Generate a random entry point into the play field
; returns X,Y
;
RNDXY:
		LDY #$01
		JSR RNDNEXT
		AND #$03
		STA ACOPY
		CMP #$02
		BCS @top
		LDY #$08
@top:	STY $01
		JSR RNDNEXT
		AND #$07
		CLC
		ADC $01
		TAY
		;
		LDX #$00
		LDA ACOPY
		AND #$01
		BNE @left
		LDX #$0D
@left:	STX $01
		JSR RNDNEXT
		AND #$07
		CLC
		ADC $01
		TAX
		RTS

;*********************************************************************
; Traveling somewhere?  Enforce the rules of our field of play.
; pass/return X as direction #0-15
; pass Y as ship #0 (FURY)
;
TRAVELING:
		STX XCOPY
		STY YCOPY
		TYA
		LDX SPRITEX+12	; save Fury's position
		LDY SPRITEY+12
		STA R0			; save ship type
		STX R1			; save X coord
		STY R2			; save Y coord
		;
@ddx2:	LDX XCOPY
		LDY YCOPY
		LDA DDX2,X
		BEQ @ddy2
		CMP #$01
		BEQ @ddx
		LDA THRUST
		AND #$01
		BEQ @ddy2
@ddx:	LDA DDX,X
		CLC
		ADC R1
		STA R1
		CMP #$10
		BCC @tooleft
		CMP #$75		; right edge of objective?
		BNE @okleft
		LDA R2
		CMP #$52
		BCC @okleft
		CMP #$7F
		BCS @okleft
		LDA #$76
		BNE @tooleft2
@tooleft:
		LDA #$10
@tooleft2:
		STA R1
		LDA SPEED
		BNE @bouncex
@okleft:
		LDA R1
		CLC
		ADC #$07
		CMP SSSCLIPX
		BCS @tooright
		CMP #$54		; left edge of objective?
		BNE @okright
		LDA R2
		CMP #$52
		BCC @okright
		CMP #$7F
		BCS @okright
		LDA #$53
		BNE @tooright2
@tooright:
		LDA SSSCLIPX
@tooright2:
		SEC
		SBC #$07
		STA R1
@bouncex:
		LDA SPEED
		CMP #$03
		BCC @boingx
		STA SPEED
		DEC SPEED
@boingx:
		LDA VBOUNCE,X
		STA XCOPY
		TAX
@okright:
		;
@ddy2:	LDA DDY2,X
		BEQ @okbottom
		CMP #$01
		BEQ @ddy
		LDA THRUST
		AND #$01
		BEQ @okbottom
@ddy:	LDA DDY,X
		CLC
		ADC R2
		STA R2
		CMP #$10
		BCC @toohi
		CMP #$7E		; bottom edge of objective?
		BNE @oktop
		LDA R1
		CMP #$54-7
		BCC @oktop
		CMP #$76
		BCS @oktop
		LDA #$7F
		BNE @toohi2
@toohi:
		LDA #$10
@toohi2:
		STA R2
		LDA SPEED
		BNE @bouncey
@oktop:
		LDA R2
		CLC
		ADC #$10
		CMP SSSCLIPY
		BCS @toolow
		CMP #$63		; top edge of objective?
		BNE @okbottom
		LDA R1
		CMP #$54-7
		BCC @okbottom
		CMP #$76
		BCS @okbottom
		LDA #$62
		BNE @toolow2
@toolow:
		LDA SSSCLIPY
@toolow2:
		SEC
		SBC #$10
		STA R2
@bouncey:
		LDA SPEED
		CMP #$03
		BCC @boingy
		STA SPEED
		DEC SPEED
@boingy:
		LDA HBOUNCE,X
		STA XCOPY
@okbottom:
		LDX XCOPY
		RTS
		;
		; essentially the same playfield for Thargoids, but are not limited to
		; VIC's borders and should not go crashing into the shields either.
		; pass Y as ship #0-3 (Thargoids)
TRAVELING2:
		STX XCOPY
		STY YCOPY
		LDA NME,Y
		PHA				;++
		LDA SPRITEX+8,Y	; save enemy ship's position
		TAX
		LDA SPRITEY+8,Y
		TAY
		PLA				;--
		STA R0			; save ship type
		STX R1			; save X coord
		STY R2			; save Y coord
		;
@ddx2:	LDX XCOPY
		LDY YCOPY
		LDA DDX2,X
		BEQ @ddy2
@ddx:	LDA DDX,X
		CLC
		ADC R1
		STA R1
		CMP #$08
		BCC @tooleft	; left edge of outer space?
		CMP #$6F		; right edge of objective?
		BNE @okleft
		LDA R2
		CMP #$58
		BCC @okleft
		CMP #$80
		BCS @okleft
		LDA #$70
		BNE @tooleft2
@tooleft:
		LDA #$08
@tooleft2:
		STA R1
		LDA SPEED,Y
		BNE @bouncex
@okleft:
		LDA R1
		SEC
		SBC #$08
		CMP SSSCLIPX
		BCS @tooright	; right edge of outer space?
		CMP #$51		; left edge of objective?
		BNE @okright
		LDA R2
		CMP #$58
		BCC @okright
		CMP #$80
		BCS @okright
		LDA #$50
@tooright:
		LDA SSSCLIPX
		BNE @skip8
@tooright2:
		SEC
		SBC #$08
@skip8:
		STA R1
@bouncex:
		LDX XCOPY
		LDA VBOUNCE,X
		AND #$0C
		STA XCOPY
		TAX
		LDA #$02		; just for 2-moves
		STA NMELEN,Y
		LDA #$00		; and stop turning
		STA NMETURN,Y
@okright:
		;
@ddy2:	LDA DDY2,X
		BEQ @okbottom
@ddy:	LDA DDY,X
		CLC
		ADC R2
		STA R2
		CMP #$08
		BCC @toohi		; top edge of outer space?
		CMP #$7F		; bottom edge of objective?
		BNE @oktop
		LDA R1
		CMP #$50
		BCC @oktop
		CMP #$71
		BCS @oktop
		LDA #$80
		BNE @toohi2
@toohi:
		LDA #$08
@toohi2:
		STA R2
		LDA SPEED,Y
		BNE @bouncey
@oktop:
		LDA R2
		CLC
		ADC #$08
		CMP SSSCLIPY
		BCS @toolow		; bottom edge of outer space?
		CMP #$51		; top edge of objective?
		BNE @okbottom
		LDA R1
		CMP #$50
		BCC @okbottom
		CMP #$71
		BCS @okbottom
		LDA #$58
		BNE @toolow2
@toolow:
		LDA SSSCLIPY
@toolow2:
		SEC
		SBC #$08
		STA R2
@bouncey:
		LDX XCOPY
		LDA HBOUNCE,X
		AND #$0C
		STA XCOPY
		LDA #$01		; just for 2-moves
		STA NMELEN,Y
		LDA #$00		; and stop turning
		STA NMETURN,Y
@okbottom:
		LDX XCOPY
		RTS

;*********************************************************************
; Planning something?  Give our Thargoid "friends" some personality
; pass LOOPX as ship #0-3
; 
; NMEPLAN
; bit 0: cruisin'
; bit 1: turning left
; bit 2: turning right
; bit 3: mining
; bit 4: firing
; bit 5: center attack
; bit 6: spread attack
; bit 7: deadly attack
;
; FIRING
; Scouts and Carriers can fire any of the 16-directions at anytime
; Cruisers can fire only in the facing direction (+/- 1)
; Destroyers can fire only in the facing direction (+/- 1)
; 
; CENTER ATTACK
; Thargoid ship becomes stationary, facing center objective, firing at a constant rate
; and only in N,S,E,W areas:
; Scouts: when HP < 16
; Cruisers: when HP < 30
; Destroyers: all of the time
; Carriers: all of the time
;
; SPREAD ATTACK
; Only Destroyers and Carriers can fire up to three consecutive shots
;
; DEADLY ATTACK
; Only Carriers can launch a Neutron Bomb at the center objective, speed = 1,
; and only 8% chance in N,S,E,W areas, before warping out
;
PLANNING:
		LDA FURYHP
		BNE @cont
		LDA #$01		; fury's dead
		LDY OMEGAHP
		BNE @kill
		LDA #$FF		; objective destroyed
@kill:	STA NMEHP,X		; make this ship aggressive
@cont:	LDA NMESPEED,X
		BNE @moving
		LDA #$02
		STA NMESPEED,X
		LDA #%00000001	; resume flight
		JMP PLANNED
@moving:
		LDA MISSION
		BNE @real
		LDA #$03
		STA NMESPEED,X
		LDA #%00000001	; remote-controlled android
		JMP PLANNED
@real:	LDA NME,X
		CMP #$01		; SCOUT ($5B)
		BNE @s2
		LDA #$04
		STA NMESPEED,X
		LDA NMEHP,X
		CMP #$5B
		BCC @s1o1
		LDA #%00000111
		JMP PLANNED
@s1o1:	CMP #$10
		BCC @s1o2
		LDA #%00011111	; you hit me?
		JMP PLANNED
@s1o2:	LDA #$06
		STA NMESPEED,X
		LDA #%00111111
		JMP PLANNED
@s2:	CMP #$02		; CRUISER ($6A)
		BNE @s3
		LDA #$06
		STA NMESPEED,X
		LDA NMEHP,X
		CMP #$6A
		BCC @s2o1
		LDA #%00001111
		JMP PLANNED
@s2o1:	CMP #$1E
		BCC @s2o2
		LDA #%00011111
		JMP PLANNED
@s2o2:	LDA #$08
		STA NMESPEED,X
		LDA #%00111111
		JMP PLANNED
@s3:	CMP #$03		; DESTROYER ($79)
		BNE @s4
		LDA #$08
		STA NMESPEED,X
		LDA NMEHP,X
		CMP #$79
		BCC @s3o1
		LDA #%00111111
		JMP PLANNED
@s3o1:	LDA #$0A
		STA NMESPEED,X
		LDA #%01111111
		JMP PLANNED
@s4:					; CARRIER ($C8)
		LDA #$03
		LDY FURYHP
		BNE @s4o1
		ASL				; x2
@s4o1:	STA NMESPEED,X
		LDA NMEHP,X
		LDA #%11110001
;
PLANNED:
		STA NMEPLAN,X
		LDA NMEHP,X
		CMP #$FF
		BCC @game
		;ASL NMESPEED,X
		LDA #%00000111	; attract mode
		STA NMEPLAN,X
@game:	JSR @cruisin	; always determine what's the next move
		LDY #$08
		STY R1
@nextplan:
		DEC R1
		BEQ @xcruz		; nothing else, just move
		LDX LOOPX
		LDA NMEPLAN,X
		LDY R1
		AND MASK,Y
		BEQ @nextplan
@y7:	CPY #$07
		BNE @y6
		JMP @deadly
@y6:	CPY #$06
		BNE @y5
		JMP @spread
@y5:	CPY #$05
		BNE @y4
		JMP @center
@y4:	CPY #$04
		BNE @y3
		JMP @firing
@y3:	CPY #$03
		BNE @y2
		JMP @mining
@y2:	CPY #$02
		BNE @y1
		JMP @rturn
@y1:	JMP @lturn
		;
@cruisin:
		LDA SPRITEY+8,X
		TAY
		LDA SPRITEX+8,X
		TAX
		JSR @area
		LDX LOOPX
		LDA NMEDIR,X
		LSR
		LSR
		CLC
		ADC R0
		TAY
		;
		LDA AREADIR,Y	; fetch new direction
		CMP NMEDIR,X	; same direction?
		BNE @maketurn
		JSR RNDNEXT
		LDX LOOPX
		AND #$07
		STA NMELEN,X
		CLC
		ADC NME,X
		BNE @newlen
@tooslow:
		LDA #$01
@newlen:
		STA NMELEN,X
		LDA AREADIR,Y	; fetch new direction
		STA NMEDIR,X
@xcruz:	RTS
@maketurn:
		LDA NMESPEED,X
		CMP #$03
		BCC @tooslow
		LDA NMEDIR,X
		BNE @noup1		; was old direction UP?
		LDA AREADIR,Y
		CMP #$0C		; is new direction LEFT?
		BEQ @mkleft		; yes, go counter-clockwise
		BNE @mkright	; no, go clockwise
@noup1:	LDA AREADIR,Y
		BNE @noup2		; is new direction UP?
		LDA NMEDIR,X
		CMP #$0C		; was old direction LEFT?
		BEQ @mkright	; yes, go clockwise
		BNE @mkleft		; no, go counter-clockwise
@noup2:	CMP NMEDIR,X	; is new direction greater than old?
		BCS @mkright	; yes, go clockwise
@mkleft:				; else, go counter-clockwise
		LDX LOOPX
		DEC NMEDIR,X
		DEC NMEDIR,X
		LDA NMEDIR,X
		AND #$0F
		STA NMEDIR,X
		LDA #$01		; two moves to make the turn
		STA NMELEN,X
		LDA #$FE
		STA NMETURN,X
		RTS
@mkright:
		LDX LOOPX
		INC NMEDIR,X
		INC NMEDIR,X
		LDA NMEDIR,X
		AND #$0F
		STA NMEDIR,X
		LDA #$01		; two moves to complete a turn
		STA NMELEN,X
		LDA #$02
		STA NMETURN,X
		RTS
		;
@lturn:
		LDA R0
		CMP #$04		; 1
		BEQ @ltcont
		CMP #$0C		; 3
		BEQ @ltcont
		CMP #$14		; 5
		BEQ @ltcont
		CMP #$1C		; 7
		BEQ @ltcont
		JMP @nextplan
@ltcont:
		LDA NMEDIR,X
		CMP #$00		; up
		BEQ @ltx1
		CMP #$08		; down
		BEQ @ltx2
		CMP #$04		; right
		BEQ @lty1
		CMP #$0C		; left
		BEQ @lty2
@nolt:	JMP @nextplan
@ltx1:	LDA SPRITEX+8,X
		CMP #$20
		BCC @nolt
		CMP #$50
		BCC @ltok
		CMP #$90
		BCS @ltok
		RTS
@ltx2:	LDA SPRITEX+8,X
		CMP #$30
		BCC @ltok
		CMP #$78
		BCC @nolt
		CMP #$A8
		BCC @ltok
		RTS
@lty1:	LDA SPRITEY+8,X
		CMP #$20
		BCC @nolt
		CMP #$50
		BCC @ltok
		CMP #$98
		BCS @ltok
		RTS
@lty2:	LDA SPRITEY+8,X
		CMP #$38
		BCC @ltok
		CMP #$80
		BCC @nolt
		CMP #$A8
		BCC @ltok
		RTS
@ltok:	JSR RNDNEXT
		CMP #$14		; 20%
		BCC @lt2
		RTS
@lt2:	LDX LOOPX
		DEC NMEDIR,X
		DEC NMEDIR,X
		LDA NMEDIR,X
		AND #$0F
		STA NMEDIR,X
		LDA #$03		; four moves to complete a turn
		STA NMELEN,X
		LDA #$FE
		STA NMETURN,X
		RTS
		;
@rturn:
		LDA R0
		CMP #$04		; 1
		BEQ @rtcont
		CMP #$0C		; 3
		BEQ @rtcont
		CMP #$14		; 5
		BEQ @rtcont
		CMP #$1C		; 7
		BEQ @rtcont
		JMP @nextplan
@rtcont:
		LDA NMEDIR,X
		CMP #$00		; up
		BEQ @rtx1
		CMP #$08		; down
		BEQ @rtx2
		CMP #$04		; right
		BEQ @rty1
		CMP #$0C		; left
		BEQ @rty2
@nort:	JMP @nextplan
@rtx1:	LDA SPRITEX+8,X
		CMP #$30
		BCC @rtok
		CMP #$78
		BCC @nort
		CMP #$A8
		BCC @rtok
		JMP @nextplan
@rtx2:	LDA SPRITEX+8,X
		CMP #$20
		BCC @nort
		CMP #$50
		BCC @rtok
		CMP #$90
		BCS @rtok
		JMP @nextplan
@rty1:	LDA SPRITEY+8,X
		CMP #$38
		BCC @rtok
		CMP #$80
		BCC @nort
		CMP #$A8
		BCC @rtok
		JMP @nextplan
@rty2:	LDA SPRITEY+8,X
		CMP #$20
		BCC @nort
		CMP #$50
		BCC @rtok
		CMP #$98
		BCS @rtok
		JMP @nextplan
@rtok:	JSR RNDNEXT
		CMP #$14		; 20%
		BCC @rt2
		JMP @nextplan
@rt2:	LDX LOOPX
		INC NMEDIR,X
		INC NMEDIR,X
		LDA NMEDIR,X
		AND #$0F
		STA NMEDIR,X
		LDA #$03		; four moves to complete a turn
		STA NMELEN,X
		LDA #$02
		STA NMETURN,X
		RTS
		;
@mxx:	JMP @nextplan
@mining:
		LDA MINES
		CMP #$10
		BCS @mxx
		JSR RNDNEXT
		CMP #91			; 10% drop a mine
		BCC @mxx
		LDA XCOPY
		CMP #$10
		BCC @mxx
		CMP SSSCLIPX
		BCS @mxx
		SEC
		SBC #$10
		LSR
		LSR
		LSR
		STA XCOPY
		LDA YCOPY
		CMP #$10
		BCC @mxx
		CMP #200		; SSSCLIPY - 8
		BCS @mxx
		SEC
		SBC #$10
		LSR
		LSR
		LSR
		STA YCOPY
		JSR RNDNEXT
		STA ACOPY
		LDX LOOPX
		LDA NME,X
		TAY
		LDA ACOPY
		CPY #$03
		BCC @mine3
		AND #$0F
		BNE @mine3		; 16:1
@mine4:
		LDX XCOPY
		LDY YCOPY
		JSR DROPHEARTSTOPPER
		JMP @nextplan
@mine3:
		CPY #$02
		BCC @mine2
		AND #$07
		BNE @mine2		; 8:1
		LDX XCOPY
		LDY YCOPY
		JSR DROPSONICBOOMER
		JMP @nextplan
@mine2:
		AND #$03
		BNE @mine1
		LDX XCOPY
		LDY YCOPY
		JSR DROPHULLBLASTER
		JMP @nextplan
@mine1:
		LDX XCOPY
		LDY YCOPY
		JSR DROPGRENADE
		JMP @nextplan
		;
@firing:
		LDA SPRITEX+8,X
		CMP #$10
		BCS @fx1
		JMP @nextplan
@fx1:	CLC
		ADC #$07
		CMP SSSCLIPX
		BCC @fx2
		JMP @nextplan
@fx2:	LDA SPRITEY+8,X
		CMP #$10
		BCS @fy1
		JMP @nextplan
@fy1:	CLC
		ADC #$0F
		CMP SSSCLIPY
		BCC @fy2
		JMP @nextplan
@fy2:	LDA NME,X
		ASL
		ASL
		ASL
		ASL
		STA ACOPY
		JSR RNDNEXT
		CMP ACOPY
		BCC @fire
		JMP @nextplan
@fire:	LDX LOOPX
		LDA NME,X
		CMP #$01		; SCOUT
		BEQ @fireback
		CMP #$04		; CARRIER
		BEQ @fireback
		LDA NMEDIR,X
		STA ACOPY
		JSR RNDNEXT
		AND #$03
		BNE @f3
		LDA FRAME
		AND #$01
		BNE @f2
@f1:	INC ACOPY
		JMP @f3
@f2:	DEC ACOPY
@f3:	LDA ACOPY
		JMP @fired
@fireback:				; a 'smarter' firing solution
		LDX LOOPX
		LDA SPRITEX+8,X
		SEC
		SBC SPRITEX+12
		BCS @fxcs
		EOR #$FF		; abs(dX)
@fxcs:	LSR
		LSR
		LSR
		LSR				; divide abs delta X by 16
		STA R3
		LDA SPRITEY+8,X
		SEC
		SBC SPRITEY+12
		BCS @fycs
		EOR #$FF		; abs(dY)
@fycs:	LSR
		LSR
		LSR
		LSR				; divide abs delta Y by 16
		STA R4
		LDX #$02		; start by aiming diagonal
		CMP R3
		BEQ	@fsckx		; uh-oh
		BCS	@fshigher	; Y >= X?
		LDX #$03		; aim 3 o'clock
		LSR R4			; divide abs delta Y by 32
		BNE	@fsckx		; still enough Y to consider?
		LDX #$04		; aim horizontal
		BNE @fsckx
@fshigher:
		LDX #$01		; aim 1 o'clock
		LSR R3			; divide abs delta X by 32
		BNE @fsckx		; still enough X to consider?
		DEX				; aim vertical
		STX R4
		BEQ @fscky
@fsckx:
		STX R4			; aim direction is 1 - 4
		LDX LOOPX
		LDA SPRITEX+12
		CMP SPRITEX+8,X
		BCS @fscky		; fury is to the right of carrier?
		LDA #$10
		SEC
		SBC R4
		STA R4
@fscky:
		LDA SPRITEY+12
		CMP SPRITEY+8,X
		BCC @fsfini		; direction is above
		LDA #$08		; direction is below
		SEC
		SBC R4
		STA R4
		CMP #$08
		BEQ @fsfini		; direction is straight down
		BCC @fsfini		; direction is to the right
		CLC
		ADC #$10		; direction is to the left
		STA R4
@fsfini:
		LDA R4
@fired:
		AND #$0F
		TAY
		LDX LOOPX
		LDA NME,X		; phasers ready
		JSR NEWPHASER
		JMP @nextplan
		;
@center:
		LDA OMEGAHP
		BEQ @xcenter
		LDA NEUTRON
		BNE @xcenter
		LDA FURYHP
		BEQ @ccheck
		JSR RNDNEXT
		CMP #$07
		BCC @ccheck
@xcenter:
		JMP @nextplan
@ccheck:
		LDA R0
		CMP #$04		; 1
		BEQ @objcont1
		CMP #$0C		; 3
		BEQ @objcont2
		CMP #$14		; 5
		BEQ @objcont3
		CMP #$1C		; 7
		BEQ @objcont4
		JMP @nextplan
@objcont1:
		LDA #$08
		BNE @objcont
@objcont2:
		LDA #$04
		BNE @objcont
@objcont3:
		LDA #$0C
		BNE @objcont
@objcont4:
		LDA #$00
@objcont:
		LDX LOOPX
		STA NMEDIR,X
		LDA #$00		; keep ship stationary
		STA NMESPEED,X
		STA NMETURN,X
		LDA FRAME
		AND #$3F
		CLC
		ADC #$20
		STA NMELEN,X
		RTS
		;
@spread:
		JSR RNDNEXT
		CMP #$55
		BCS @cspread
		JMP @nextplan
@cspread:
		LDX LOOPX
		LDA NMEDIR,X
		SEC
		SBC #$01
		AND #$0F
		TAY
		LDA NME,X
		JSR NEWPHASER
		LDX LOOPX
		LDA NMEDIR,X
		TAY
		LDA NME,X
		JSR NEWPHASER
		LDX LOOPX
		LDA NMEDIR,X
		CLC
		ADC #$01
		AND #$0F
		TAY
		LDA NME,X
		JSR NEWPHASER
		RTS
		;
@deadly:
		LDA OMEGAHP
		BEQ @xdeadly
		LDA FURYHP
		BEQ @cdeadly
		JSR RNDNEXT
		CMP #92			; 8% chance
		BCS @cdeadly
@xdeadly:
		JMP @nextplan
@cdeadly:
		LDA R0
		CMP #$04		; 1
		BEQ @nbcont1
		CMP #$0C		; 3
		BEQ @nbcont2
		CMP #$14		; 5
		BEQ @nbcont3
		CMP #$1C		; 7
		BEQ @nbcont4
		JMP @nextplan
@nbcont1:
		LDA #$08
		BNE @nbcont
@nbcont2:
		LDA #$04
		BNE @nbcont
@nbcont3:
		LDA #$0C
		BNE @nbcont
@nbcont4:
		LDA #$00
@nbcont:
		LDX LOOPX
		STA NMEDIR,X
		LDA #$00		; sit and spin ...
		STA NMESPEED,X
		STA NMEPLAN,X
		LDA #$10		; short wait
		LDY FURYHP
		BEQ @nblen
		LDA #$05
		STA ALERTS
		LDA #$FF		; long wait
@nblen:	STA NMELEN,X
		RTS
		;
		; which area (0-8)
@area:	STX XCOPY
		STY YCOPY
		LDY #$00
@aloop:	LDA XCOPY
		CMP AREA,Y
		BCC @next
		CMP AREA+2,Y
		BCS @next
		LDA YCOPY
		CMP AREA+1,Y
		BCC @next
		CMP AREA+3,Y
		BCS @next
		JMP @afini
@next:	INY
		INY
		INY
		INY
		CPY #$40
		BNE @aloop
		brk				; should not occur
@afini:	STY R0
		RTS

;*********************************************************************
GAMEOVER:
		LDX #$06
		LDY #$05
		JSR SSSPLOT
		JSR SSSPRINTS	; GAME OVER
		.byte	$F2,$87,$81,$8D,$85,$A0,$8F,$96,$85,$92,$00
		RTS

;*********************************************************************
; Background software interrupt routine
;
BACKGROUND:
		CLD
		LDA FLASHING
		BNE @nofl
		LDA VIC+$0E
		EOR #$80
		STA VIC+$0E
		LDA #$20
		STA FLASHING
@nofl:	DEC FLASHING
		;
		LDA PHASER
		BEQ @n0
		LDX #$00
		DEC PHASER
		BEQ @snd0
		LDX #$FB
		LDA PHASER
		AND #$01
		BNE @snd0
		LDX #$FA
@snd0:	STX VIC+$0B		; voice #2
@n0:
		LDA SHOOTING
		BEQ @n1
		DEC SHOOTING
		LDA SHOOTING
		CMP #$F0
		BNE @snd1
		LDA #$00
		STA SHOOTING
@snd1:	STA VIC+$0C		; voice #3
@n1:
		LDA EMPTY
		BEQ @n2
		LDY #$FA
		DEC EMPTY
		LDA EMPTY
		AND #$01
		BNE @snd2
		LDY #$00
@snd2:	STY VIC+$0A		; voice #1
@n2:
		LDA DROPPED
		BEQ @n3
		LDY #$DC
		DEC DROPPED
		LDA DROPPED
		AND #$01
		BNE @snd3
		LDY #$00
@snd3:	STY VIC+$0B		; voice #2
@n3:
		LDA HIT
		BEQ @n4
		DEC HIT
		LDA VIC+$0D
		CMP #$FE
		BEQ @n4
		LDA HIT
		CMP #$B0
		BNE @snd4
		LDA #$00
		STA HIT
@snd4:	STA VIC+$0D		; noise
@n4:
		LDA PULSE
		BEQ @n5
		LDA HEARTBEATS
		BEQ @dead
		DEC PULSE
		BNE @beat
		DEC PULSE
		BNE @n5
@beat:	LDY PULSE
		TYA
		AND #$01
		BEQ @beat1
		TYA
		AND #$7F
		CMP #$20
		BCC @beat2
@beat1:	LDA #$00
		STA VIC+$0A		; voice #1
		BEQ @n5
@beat2:	CMP #$08
		BCC @beat3
		LDA #$D0
		STA VIC+$0A		; voice #1
		BNE @n5
@beat3:	LDA #$A0
		STA VIC+$0A		; voice #1
		BNE @n5
@dead:	LDA #$00
		STA PULSE
		STA VIC+$0A		; voice #1
@n5:
		LDA BOOMED
		BEQ @n6
		LDA KLAXON
		SEC
		SBC #$04
		CMP #$80
		BCS @boomed
		LDA #$00
		STA BOOMED
@boomed:
		STA KLAXON
		STA VIC+$0B
		JMP @n7
@n6:
		LDA ALERTS
		BEQ @n7
		INC KLAXON
		LDA KLAXON
		CMP #$28
		BCC @n7			; moment of silence
		CMP #$F4
		BCS @quiet		; reached top note
		CMP #$D0
		BCS @alert		; continue the alert
		LDA #$D0		; sound an alert
		STA KLAXON
@alert:	STA VIC+$0B		; voice #2
		BNE @n7
@quiet:	DEC ALERTS
		LDA #$00
		STA KLAXON
		STA VIC+$0B		; voice #2
@n7:
		JMP SSSIRQ

		.segment "RODATA"

;*********************************************************************
; INTRODUCTION
;
		;		PRESS F1 TO CONTINUE!
CONT:	.byte	$90, $92, $85, $93, $93, $A0, $66,$67, $A0, $94, $8F, $A0, $83, $8F, $8E, $94, $89, $8E, $95, $85
		;
CREDITS:
		; ++ ACKNOWLEDGEMENTS ++
		;
		; OMEGA FURY THEME TUNE
		;   -=>  CARLSSON  <=-
		;
		; TECHNICAL ADVICES
		;   --=:)  MIKE  (:=--
		;
		; AND THE MEGA-CART TEAM
		;
		; WITH SPECIAL THANKS TO
		; 
		;   ^ MY LOVING WIFE ^
		;   LOIS MARIE D'AMBRA
		;
		; COMMODORE =OMEGA RACE=
		;  GAME AND INSPIRATION
		; ( (  ANDY FINKEL  ) )
		;
		.byte $07,$AB,$AB,$A0,$01,$81,$83,$8B,$8E,$8F,$97,$8C,$85,$84,$87,$85,$8D,$85,$8E,$94,$93,$A0,$07,$AB,$AB,$0D
		.byte $05,$8F,$8D,$85,$87,$81,$A0,$86,$95,$92,$99,$A0,$94,$88,$85,$8D,$85,$A0,$94,$95,$8E,$85,$0D
		.byte $07,$A0,$A0,$AD,$BD,$BE,$A0,$A0,$01,$83,$81,$92,$8C,$93,$93,$8F,$8E,$A0,$A0,$07,$BC,$BD,$AD,$0D
		.byte $0D
		.byte $05,$94,$85,$83,$88,$8E,$89,$83,$81,$8C,$A0,$81,$84,$96,$89,$83,$85,$93,$0D
		.byte $07,$A0,$A0,$AD,$AD,$BD,$BA,$A9,$A0,$A0,$01,$8D,$89,$8B,$85,$A0,$A0,$07,$A8,$BA,$BD,$AD,$AD,$0D
		.byte $0D
		.byte $03,$81,$8E,$84,$A0,$94,$88,$85,$A0,$8D,$85,$87,$81,$AD,$83,$81,$92,$94,$A0,$94,$85,$81,$8D,$0D
		.byte $0D
		.byte $04,$97,$89,$94,$88,$A0,$93,$90,$85,$83,$89,$81,$8C,$A0,$94,$88,$81,$8E,$8B,$93,$A0,$94,$8F,$0D
		.byte $0D
		.byte $02,$A0,$A0,$D3,$A0,$05,$8D,$99,$A0,$8C,$8F,$96,$89,$8E,$87,$A0,$97,$89,$86,$85,$A0,$02,$D3,$0D
		.byte $01,$A0,$A0,$8C,$8F,$89,$93,$A0,$8D,$81,$92,$89,$85,$A0,$84,$A7,$81,$8D,$82,$92,$81,$0D
		.byte $0D
		.byte $05,$83,$8F,$8D,$8D,$8F,$84,$8F,$92,$85,$A0,$02,$BD,$07,$8F,$8D,$85,$87,$81,$A0,$92,$81,$83,$85,$02,$BD
		.byte $05,$A0,$87,$81,$8D,$85,$A0,$81,$8E,$84,$A0,$89,$8E,$93,$90,$89,$92,$81,$94,$89,$8F,$8E,$0D
		.byte $04,$12,$A0,$12,$01,$A0,$A0,$81,$8E,$84,$99,$A0,$86,$89,$8E,$8B,$85,$8C,$A0,$A0,$04,$14,$A0,$14
		.byte $00
		;
INTROS:	.word	@i0,@i1,@i2,@i3,$0000
				; IN THE YEAR 2010, THE
				; OMEGAN SYSTEM EXPANDED
				; ITS STAR COLONIES TOO
				; FAR.  THEY ENCROACHED
				; UPON THE BORDERS OF
				; THE THARGOID TERRITORY
				; -- A CIVILIZATION WITH
				; ADVANCED DEFENSIVE
				; PROWESS AND RELENTLESS
				; FIRE POWER.
				;
				; AND THE THARGOIDS HAVE
				; THEIR OWN EXPANSION
				; PLANS, WHICH DOES NOT
				; INCLUDE ANY OMEGANS.
@i0:	.byte	$03,$89,$8E,$A0,$94,$88,$85,$A0,$99,$85,$81,$92,$A0,$01,$B2,$B0,$B1,$B0,$03,$AC,$A0,$94,$88,$85,$A0
		.byte	$04,$8F,$8D,$85,$87,$81,$8E,$A0,$93,$99,$93,$94,$85,$8D,$03,$A0,$85,$98,$90,$81,$8E,$84,$85,$84
		.byte	$89,$94,$93,$A0,$93,$94,$81,$92,$A0,$83,$8F,$8C,$8F,$8E,$89,$85,$93,$A0,$94,$8F,$8F,$A0
		.byte	$86,$81,$92,$AE,$A0,$A0,$94,$88,$85,$99,$A0,$85,$8E,$83,$92,$8F,$81,$83,$88,$85,$84,$A0
		.byte	$95,$90,$8F,$8E,$A0,$94,$88,$85,$A0,$82,$8F,$92,$84,$85,$92,$93,$A0,$8F,$86,$A0,$A0,$A0
		.byte	$94,$88,$85,$A0,$05,$94,$88,$81,$92,$87,$8F,$89,$84,$A0,$94,$85,$92,$92,$89,$94,$8F,$92,$99,$03
		.byte	$AD,$AD,$A0,$81,$A0,$83,$89,$96,$89,$8C,$89,$9A,$81,$94,$89,$8F,$8E,$A0,$97,$89,$94,$88
		.byte	$07,$81,$84,$96,$81,$8E,$83,$85,$84,$A0,$84,$85,$86,$85,$8E,$93,$89,$96,$85,$A0,$A0,$A0,$A0
		.byte	$90,$92,$8F,$97,$85,$93,$93,$03,$A0,$81,$8E,$84,$A0,$07,$92,$85,$8C,$85,$8E,$94,$8C,$85,$93,$93
		.byte	$86,$89,$92,$85,$A0,$90,$8F,$97,$85,$92,$03,$AE,$0D,$0D
		.byte	$81,$8E,$84,$A0,$94,$88,$85,$A0,$05,$94,$88,$81,$92,$87,$8F,$89,$84,$93,$03,$A0,$88,$81,$96,$85
		.byte	$94,$88,$85,$89,$92,$A0,$8F,$97,$8E,$A0,$85,$98,$90,$81,$8E,$93,$89,$8F,$8E,$A0,$A0,$A0
		.byte	$90,$8C,$81,$8E,$93,$AC,$A0,$97,$88,$89,$83,$88,$A0,$84,$8F,$85,$93,$A0,$8E,$8F,$94,$A0
		.byte	$89,$8E,$83,$8C,$95,$84,$85,$A0,$81,$8E,$99,$A0,$04,$8F,$8D,$85,$87,$81,$8E,$93,$03,$AE
		.byte	$00
				; THAT CONCLUSION WAS
				; CONFIRMED BY THE LAST
				; SCOUTING REPORTS.  ITS
				; TACTICAL DATA REVEALED
				; THE THARGOID THREAT ON
				; THE OMEGAN OUTREACH IS
				; NOT GOING TO END THERE
				; -- AN INVASION FORCE
				; IS MASSING TO STRIKE
				; AS DEEP AS THEIR HULL
				; PLATING AND MIGHT CAN
				; ENDURE . . . . .
@i1:	.byte	$03,$94,$88,$81,$94,$A0,$83,$8F,$8E,$83,$8C,$95,$93,$89,$8F,$8E,$A0,$97,$81,$93,$A0,$A0,$A0
		.byte	$83,$8F,$8E,$86,$89,$92,$8D,$85,$84,$A0,$82,$99,$A0,$94,$88,$85,$A0,$8C,$81,$93,$94,$A0
		.byte	$93,$83,$8F,$95,$94,$89,$8E,$87,$A0,$92,$85,$90,$8F,$92,$94,$93,$AE,$A0,$A0,$89,$94,$93
		.byte	$07,$94,$81,$83,$94,$89,$83,$81,$8C,$A0,$84,$81,$94,$81,$03,$A0,$92,$85,$96,$85,$81,$8C,$85,$84
		.byte	$94,$88,$85,$A0,$05,$94,$88,$81,$92,$87,$8F,$89,$84,$03,$A0,$94,$88,$92,$85,$81,$94,$A0,$8F,$8E
		.byte	$94,$88,$85,$A0,$04,$8F,$8D,$85,$87,$81,$8E,$03,$A0,$8F,$95,$94,$92,$85,$81,$83,$88,$A0,$89,$93
		.byte	$8E,$8F,$94,$A0,$87,$8F,$89,$8E,$87,$A0,$94,$8F,$A0,$85,$8E,$84,$A0,$94,$88,$85,$92,$85
		.byte	$AD,$AD,$A0,$81,$8E,$A0,$01,$89,$8E,$96,$81,$93,$89,$8F,$8E,$A0,$86,$8F,$92,$83,$85,$03,$A0,$A0
		.byte	$89,$93,$A0,$8D,$81,$93,$93,$89,$8E,$87,$A0,$94,$8F,$A0,$93,$94,$92,$89,$8B,$85,$A0,$A0
		.byte	$81,$93,$A0,$84,$85,$85,$90,$A0,$81,$93,$A0,$94,$88,$85,$89,$92,$A0,$88,$95,$8C,$8C,$A0
		.byte	$90,$8C,$81,$94,$89,$8E,$87,$A0,$81,$8E,$84,$A0,$8D,$89,$87,$88,$94,$A0,$83,$81,$8E,$A0
		.byte	$85,$8E,$84,$95,$92,$85,$A0,$AE,$A0,$AE,$A0,$AE,$A0,$AE,$A0,$AE,$A0
		.byte	$00
				; THE TWIN CITY RAMOK
				; HAS DEVELOPED A NEW
				; STAR FIGHTER TO COMBAT
				; THE THARGOIDS.  PILOT
				; TRAINING AND MISSIONS
				; HAVE BEEN ORDERED TO
				; PROVE THE LIGHTER SHIP
				; IN BATTLE CONDITIONS.
				;
				; ITS SUPERIOR SPEED &
				; AGILITY COMES AT THE
				; EXPENSE OF A SMALLER
				; WEAPON ENERGIZER --
				; ENERGY BOLTS DISPENSED
				; BEFORE FULLY CHARGED
				; HAVE A LESSER EFFECT,
				; PLACING A PREMIUM ON
				; ACCURATE, FIRST SHOTS.
@i2:	.byte	$03,$94,$88,$85,$A0,$94,$97,$89,$8E,$A0,$83,$89,$94,$99,$A0,$04,$92,$81,$8D,$8F,$8B,$03,$A0,$A0,$A0
		.byte	$88,$81,$93,$A0,$84,$85,$96,$85,$8C,$8F,$90,$85,$84,$A0,$81,$A0,$8E,$85,$97,$A0,$A0,$A0
		.byte	$07,$93,$94,$81,$92,$A0,$86,$89,$87,$88,$94,$85,$92,$03,$A0,$94,$8F,$A0,$83,$8F,$8D,$82,$81,$94
		.byte	$94,$88,$85,$A0,$05,$94,$88,$81,$92,$87,$8F,$89,$84,$93,$03,$AE,$A0,$A0,$90,$89,$8C,$8F,$94,$A0
		.byte	$01,$94,$92,$81,$89,$8E,$89,$8E,$87,$A0,$81,$8E,$84,$A0,$8D,$89,$93,$93,$89,$8F,$8E,$93,$03,$A0
		.byte	$88,$81,$96,$85,$A0,$82,$85,$85,$8E,$A0,$8F,$92,$84,$85,$92,$85,$84,$A0,$94,$8F,$A0,$A0
		.byte	$90,$92,$8F,$96,$85,$A0,$94,$88,$85,$A0,$8C,$89,$87,$88,$94,$85,$92,$A0,$93,$88,$89,$90
		.byte	$89,$8E,$A0,$82,$81,$94,$94,$8C,$85,$A0,$83,$8F,$8E,$84,$89,$94,$89,$8F,$8E,$93,$AE,$A0,$0D
		.byte	$89,$94,$93,$A0,$01,$93,$95,$90,$85,$92,$89,$8F,$92,$A0,$93,$90,$85,$85,$84,$03,$A0,$A6,$A0,$A0
		.byte	$01,$81,$87,$89,$8C,$89,$94,$99,$03,$A0,$83,$8F,$8D,$85,$93,$A0,$81,$94,$A0,$94,$88,$85,$A0,$A0
		.byte	$85,$98,$90,$85,$8E,$93,$85,$A0,$8F,$86,$A0,$81,$A0,$93,$8D,$81,$8C,$8C,$85,$92,$A0,$A0
		.byte	$97,$85,$81,$90,$8F,$8E,$A0,$85,$8E,$85,$92,$87,$89,$9A,$85,$92,$A0,$AD,$AD,$A0,$A0,$A0
		.byte	$02,$85,$8E,$85,$92,$87,$99,$A0,$82,$8F,$8C,$94,$93,$03,$A0,$84,$89,$93,$90,$85,$8E,$93,$85,$84
		.byte	$82,$85,$86,$8F,$92,$85,$A0,$86,$95,$8C,$8C,$99,$A0,$83,$88,$81,$92,$87,$85,$84,$A0,$A0
		.byte	$88,$81,$96,$85,$A0,$81,$A0,$8C,$85,$93,$93,$85,$92,$A0,$85,$86,$86,$85,$83,$94,$AC,$A0
		.byte	$90,$8C,$81,$83,$89,$8E,$87,$A0,$81,$A0,$90,$92,$85,$8D,$89,$95,$8D,$A0,$8F,$8E,$A0,$A0
		.byte	$01,$81,$83,$83,$95,$92,$81,$94,$85,$03,$AC,$A0,$86,$89,$92,$93,$94,$A0,$93,$88,$8F,$94,$93,$AE
		.byte	$00
				; SEVEN YEARS OF COMMAND
				;  AND RESPECT FROM ALL
				;     THROUGHOUT THE
				; GALAXIES IS CHALLENGED
				;      ONCE AGAIN.
				;
				; THE WARRIORS TASKED TO
				; PROTECT THIS HONOR ARE
				; CALLING THEIR NEW STAR
				; FIGHTER . . . . .
@i3:	.byte	$03,$93,$85,$96,$85,$8E,$A0,$99,$85,$81,$92,$93,$A0,$8F,$86,$A0,$07,$83,$8F,$8D,$8D,$81,$8E,$84,$03
		.byte	$A0,$81,$8E,$84,$A0,$07,$92,$85,$93,$90,$85,$83,$94,$03,$A0,$86,$92,$8F,$8D,$A0,$81,$8C,$8C,$A0
		.byte	$A0,$A0,$A0,$A0,$94,$88,$92,$8F,$95,$87,$88,$8F,$95,$94,$A0,$94,$88,$85,$A0,$A0,$A0,$A0
		.byte	$87,$81,$8C,$81,$98,$89,$85,$93,$A0,$89,$93,$A0,$83,$88,$81,$8C,$8C,$85,$8E,$87,$85,$84
		.byte	$A0,$A0,$A0,$A0,$A0,$8F,$8E,$83,$85,$A0,$81,$87,$81,$89,$8E,$AE,$A0,$A0,$A0,$A0,$A0,$A0,$0D
		.byte	$94,$88,$85,$A0,$97,$81,$92,$92,$89,$8F,$92,$93,$A0,$94,$81,$93,$8B,$85,$84,$A0,$94,$8F
		.byte	$01,$90,$92,$8F,$94,$85,$83,$94,$A0,$94,$88,$89,$93,$A0,$88,$8F,$8E,$8F,$92,$03,$A0,$81,$92,$85
		.byte	$83,$81,$8C,$8C,$89,$8E,$87,$A0,$94,$88,$85,$89,$92,$A0,$07,$8E,$85,$97,$A0,$93,$94,$81,$92
		.byte	$86,$89,$87,$88,$94,$85,$92,$03,$A0,$AE,$A0,$AE,$A0,$AE,$A0,$AE,$A0,$AE,$A0
		.byte	$00
;
WALKERS:
		.word	SCOUT0,CRUISER0,DESTROYER0,CARRIER0,$0000
;
; spaceship fly-by
OMEGABIG:
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00001010
		.byte	%10101010

		.byte	%00101010
		.byte	%00001010
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000011

		.byte	%00000010
		.byte	%00000010
		.byte	%00001010
		.byte	%00001010
		.byte	%00010110
		.byte	%01010110
		.byte	%10101010
		.byte	%10101000

		.byte	%10101011
		.byte	%10101011
		.byte	%10101011
		.byte	%10101000
		.byte	%10100000
		.byte	%10100000
		.byte	%10000000
		.byte	%10101000
;
HBOUNCE:
		.byte	$08,$07,$06,$05,$04,$03,$02,$01,$00,$0F,$0E,$0D,$0C,$0B,$0A,$09
VBOUNCE:
		.byte	$00,$0F,$0E,$0D,$0C,$0B,$0A,$09,$08,$07,$06,$05,$04,$03,$02,$01
;
HULLC:	.byte	$05,$07,$02,$02
HULLV:	.byte	$2D,$1E,$0F	; 45-30-15 damage points
BOLTC:	.byte	$02,$03,$07,$01
BOLTE:	.byte	$90,$48,$20
BOLTX:	.byte	$02,$04,$06,$06,$06,$06,$06,$04,$02,$00,$FE,$FE,$FE,$FE,$FE,$00
BOLTY:	.byte	$FD,$FD,$FE,$00,$02,$04,$06,$07,$07,$07,$06,$04,$02,$00,$FE,$FD
DDX:	.byte	$00,$01,$01,$01,$01,$01,$01,$01,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DDX2:	.byte	$00,$02,$01,$01,$01,$01,$01,$02,$00,$02,$01,$01,$01,$01,$01,$02
DDY:	.byte	$FF,$FF,$FF,$FF,$00,$01,$01,$01,$01,$01,$01,$01,$00,$FF,$FF,$FF
DDY2:	.byte	$01,$01,$01,$02,$00,$02,$01,$01,$01,$01,$01,$02,$00,$02,$01,$01
NDDX:	.byte	$00,$02,$02,$02,$02,$02,$02,$02,$00,$FE,$FE,$FE,$FE,$FE,$FE,$FE
NDDY:	.byte	$FE,$FE,$FE,$FE,$00,$02,$02,$02,$02,$02,$02,$02,$00,$FE,$FE,$FE
		; travel lane data
		; X outer left - right
		; X inner left - right
		; Y outer top - bottom
		; Y inner top - bottom
OUTER:	.byte	0,2,1,0,0	; Omega Fury and Thargoid destroyer travel between
INNER:	.byte	2,2,1,2,0	; all lanes: OUTER=0 thru INNER=2
OLX:	.byte	$10,$20,$38	; 0,2,5
ORX:	.byte	$A8,$98,$88	; 19,17,15
OTY:	.byte	$10,$20,$40	; 0,2,6
		; top-left, bottom-right
AREA:	.byte	 0*8,  0*8, 11*8-1, 12*8-1
		.byte	11*8,  0*8, 14*8-1, 12*8-1
		.byte	14*8,  0*8, 25*8-1, 12*8-1
		.byte	 0*8, 12*8, 11*8-1, 15*8-1
		.byte	11*8, 12*8, 14*8-1, 15*8-1
		.byte	14*8, 12*8, 25*8-1, 15*8-1
		.byte	 0*8, 15*8, 11*8-1, 27*8-1
		.byte	11*8, 15*8, 14*8-1, 27*8-1
		.byte	14*8, 15*8, 25*8-1, 27*8-1
		; up, right, down, left
AREADIR:
		.byte	 4, 4, 8, 8		; TL: R,R,D,D
		.byte	12, 4, 4,12		; TM: L,R,R,L
		.byte	12, 8, 8,12		; TR: L,D,D,L
		.byte	 0, 8, 8, 0		; ML: U,D,D,U
		.byte	 0, 4, 8,12		; MM: U,R,D,L
		.byte	 0, 0, 8, 8		; MR: U,U,D,D
		.byte	 0, 4, 4, 0		; BL: U,R,R,U
		.byte	12, 4, 4,12		; BM: L,U,U,L
		.byte	 0, 0,12,12		; BR: U,U,L,L
		;
BOOMITC:
		.byte	$11, $12, $13, $14	; Q R S T
BOOMITX:
		.byte	$00, $08, $00, $F8
BOOMITY:
		.byte	$F8, $00, $08, $00
		;
VALUE:	.byte	$03, $10, $25, $50	; pts awarded per ship type destroyed
		;
VELOCITY:
		.byte	%10000000
		.byte	%10001000
		.byte	%10010010
		.byte	%01010101
		.byte	%01101101
		.byte	%01110111
		.byte	%01111111
		.byte	%11111111
;
MISSIONS:		; TRAINEE LEVEL
		.byte	$01,$00,$00,$00	; 00:	1 Scout (half-speed drone)
		.byte	$00,$00,$00,$00	;
				; SCOUT LEVEL
		.byte	$01,$00,$00,$00	; 01:	1 Scout
		.byte	$01,$01,$00,$00	;		1 grenade, 1 hull blaster
		.byte	$01,$01,$00,$00	; 02:	2 Scouts
		.byte	$02,$01,$00,$00	;		2 grenades, 1 hull blaster
				; DEFENDER LEVEL
		.byte	$01,$01,$01,$00	; 03:	3 Scouts
		.byte	$02,$02,$00,$00	;		2 grenades, 2 hull blasters
		.byte	$01,$01,$01,$01	; 04:	4 Scouts
		.byte	$04,$02,$00,$00	;		4 grenades, 2 hull blasters
		.byte	$02,$01,$01,$00	; 05:	1 Cruiser, 2 Scouts
		.byte	$02,$04,$00,$00	;		2 grenades, 4 hull blasters
				; WARRIOR LEVEL
		.byte	$02,$01,$01,$01	; 06:	1 Cruiser, 3 Scouts
		.byte	$06,$02,$00,$00	;		6 grenades, 2 hull blasters
		.byte	$02,$02,$01,$00	; 07:	2 Cruisers, 1 Scout
		.byte	$04,$02,$01,$00	;		4 grenades, 2 hull blasters, 1 sonic boomer
		.byte	$02,$02,$01,$01	; 08:	2 Cruisers, 2 Scouts
		.byte	$04,$01,$03,$00	;		4 grenades, 1 hull blaster, 3 sonic boomers
		.byte	$03,$02,$01,$00	; 09:	1 Destroyer, 1 Cruiser, 1 Scout
		.byte	$04,$01,$01,$00	;		4 grenades, 1 hull blaster, 1 sonic boomer
				; CENTURION LEVEL
		.byte	$03,$02,$01,$01	; 10:	1 Destroyer, 1 Cruiser, 2 Scouts
		.byte	$04,$02,$01,$00	;		4 grenades, 2 hull blasters, 1 sonic boomer
		.byte	$03,$02,$02,$01	; 11:	1 Destroyer, 2 Cruisers, 1 Scout
		.byte	$04,$02,$01,$00	;		4 grenades, 2 hull blasters, 1 sonic boomer
		.byte	$03,$02,$02,$02	; 12:	1 Destroyer, 3 Cruisers
		.byte	$04,$02,$02,$00	;		4 grenades, 2 hull blasters, 2 sonic boomers
		.byte	$03,$03,$02,$02	; 13:	2 Destroyers, 2 Cruisers
		.byte	$03,$01,$01,$01	;		3 grenades, 1 each of all others
		.byte	$04,$02,$02,$01	; 14:	1 Carrier, 2 Cruisers, 1 Scout
		.byte	$05,$01,$01,$01	;		5 grenades, 1 each of all others
				; PROTECTOR LEVEL
		.byte	$04,$03,$02,$01	; 15:	1 Carrier, 1 Destroyer, 1 Cruiser, 1 Scout
		.byte	$04,$02,$02,$02	;		4 grenades, 2 each of all others
;
TITLES:	.word	@t0,@t1,@t2,@t3,@t4,@t5
@t0:	.byte	$07,$94,$92,$81,$89,$8E,$85,$85,$00			; TRAINEE
@t1:	.byte	$08,$93,$83,$8F,$95,$94,$00					; SCOUT
@t2:	.byte	$06,$84,$85,$86,$85,$8E,$84,$85,$92,$00		; DEFENDER
@t3:	.byte	$07,$97,$81,$92,$92,$89,$8F,$92,$00			; WARRIOR
@t4:	.byte	$06,$83,$85,$8E,$94,$95,$92,$89,$8F,$8E,$00	; CENTURION
@t5:	.byte	$06,$90,$92,$8F,$94,$85,$83,$94,$8F,$92,$00	; PROTECTOR

;*********************************************************************
; CUSTOM CHARACTER DATA
;
; center Omegan objectives (3x3 character matrix)
; @AB
; CDE
; FGH
; 
ACADEMY:
		.byte	%11111111
		.byte	%00000000
		.byte	%00111111
		.byte	%00000000
		.byte	%00001111
		.byte	%00000000
		.byte	%00000011
		.byte	%00000000
;
		.byte	%00000000
		.byte	%00000010
		.byte	%00001000
		.byte	%00100000
		.byte	%10000000
		.byte	%10000000
		.byte	%10101010
		.byte	%00101010
;
		.byte	%00001010
		.byte	%00001010
		.byte	%00000010
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000001
;mid
		.byte	%11111100
		.byte	%00000000
		.byte	%11111100
		.byte	%00000000
		.byte	%11111100
		.byte	%00000000
		.byte	%11111110
		.byte	%00101000
;
		.byte	%11111100
		.byte	%00000000
		.byte	%00111100
		.byte	%00000000
		.byte	%00000000
		.byte	%00101010
		.byte	%10101010
		.byte	%10101010
;
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%00001010
		.byte	%00001010
		.byte	%00001010
		.byte	%00010101
		.byte	%01010101
;right
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00101000
		.byte	%10000010
		.byte	%00000010
;
		.byte	%00000010
		.byte	%00001010
		.byte	%00001010
		.byte	%00101010
		.byte	%10101010
		.byte	%10101000
		.byte	%10101000
		.byte	%10101000
;
		.byte	%10100000
		.byte	%10100000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%01000000
		.byte	%01010100
;
STARGATE:
		.byte	%00101010
		.byte	%00100001
		.byte	%00100001
		.byte	%00100001
		.byte	%10000011
		.byte	%10001100
		.byte	%10001100
		.byte	%10001100
;
		.byte	%10001100
		.byte	%10101100
		.byte	%10101100
		.byte	%00011100
		.byte	%00011100
		.byte	%10101100
		.byte	%10101100
		.byte	%10001100
;
		.byte	%10001100
		.byte	%10001100
		.byte	%10001100
		.byte	%10000011
		.byte	%00100001
		.byte	%00100001
		.byte	%00100001
		.byte	%00101010
;mid
		.byte	%01010101
		.byte	%01010101
		.byte	%00000000
		.byte	%00010100
		.byte	%00000000
		.byte	%00111100
		.byte	%00000000
		.byte	%00000000
;
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
;
		.byte	%00000000
		.byte	%00000000
		.byte	%00111100
		.byte	%00000000
		.byte	%00010100
		.byte	%00000000
		.byte	%01010101
		.byte	%01010101
;right
		.byte	%10101000
		.byte	%01001000
		.byte	%01001000
		.byte	%01001000
		.byte	%11000010
		.byte	%00110010
		.byte	%00110010
		.byte	%00110010

;
		.byte	%00110010
		.byte	%00111010
		.byte	%00111010
		.byte	%00110100
		.byte	%00110100
		.byte	%00111010
		.byte	%00111010
		.byte	%00110010
;
		.byte	%00110010
		.byte	%00110010
		.byte	%00110010
		.byte	%11000010
		.byte	%01001000
		.byte	%01001000
		.byte	%01001000
		.byte	%10101000
;
CARGOSHIP:
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%01010000
		.byte	%10100001
		.byte	%10101010
		.byte	%10101010
;
		.byte	%10101010
		.byte	%10101010
		.byte	%10001010
		.byte	%10001000
		.byte	%10101000
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
;
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%00100000
		.byte	%00101010
		.byte	%00001000
		.byte	%00000010
		.byte	%00000000
;mid
		.byte	%00111100
		.byte	%01010101
		.byte	%00101000
		.byte	%00101000
		.byte	%00101000
		.byte	%00101000
		.byte	%01010101
		.byte	%10101010
;
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10000010
		.byte	%10000010
		.byte	%10101010
		.byte	%10101010
;
		.byte	%10101010
		.byte	%10101010
		.byte	%00000000
		.byte	%10101010
		.byte	%00000000
		.byte	%10101010
		.byte	%00000000
		.byte	%10101010
;right
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000101
		.byte	%01001010
		.byte	%10101010
		.byte	%10101010
;
		.byte	%10101010
		.byte	%10101010
		.byte	%10100010
		.byte	%00100010
		.byte	%00101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
;
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%00001000
		.byte	%10101000
		.byte	%00100000
		.byte	%10000000
		.byte	%00000000
;
MEDICALSHIP:
		.byte	%00000000
		.byte	%00000011
		.byte	%00001110
		.byte	%00001110
		.byte	%00111010
		.byte	%00111010
		.byte	%00111010
		.byte	%11101010
;
		.byte	%11101010
		.byte	%11101010
		.byte	%11101001
		.byte	%11101001
		.byte	%11101001
		.byte	%11101001
		.byte	%11101010
		.byte	%11101010
;
		.byte	%11101010
		.byte	%00111010
		.byte	%00111010
		.byte	%00111010
		.byte	%00001110
		.byte	%00001110
		.byte	%00000011
		.byte	%00000000
;mid
		.byte	%11111111
		.byte	%11111111
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10010110
		.byte	%10010110
		.byte	%10010110
;
		.byte	%10010110
		.byte	%10010110
		.byte	%01010101
		.byte	%01010101
		.byte	%01010101
		.byte	%01010101
		.byte	%10010110
		.byte	%10010110
;
		.byte	%10010110
		.byte	%10010110
		.byte	%10010110
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%11111111
		.byte	%11111111
;right
		.byte	%00000000
		.byte	%11000000
		.byte	%10110000
		.byte	%10110000
		.byte	%10101100
		.byte	%10101100
		.byte	%10101100
		.byte	%10101011
;
		.byte	%10101011
		.byte	%10101011
		.byte	%01101011
		.byte	%01101011
		.byte	%01101011
		.byte	%01101011
		.byte	%10101011
		.byte	%10101011
;
		.byte	%10101011
		.byte	%10101100
		.byte	%10101100
		.byte	%10101100
		.byte	%10110000
		.byte	%10110000
		.byte	%11000000
		.byte	%00000000
;
WEAPONSHIP:
		.byte	%00111111
		.byte	%00111111
		.byte	%00001010
		.byte	%00001010
		.byte	%00001010
		.byte	%00001010
		.byte	%00001010
		.byte	%00000101
;
		.byte	%00000101
		.byte	%00010110
		.byte	%00010110
		.byte	%01010101
		.byte	%01010101
		.byte	%00010110
		.byte	%00010110
		.byte	%00000101
;
		.byte	%00000101
		.byte	%00001010
		.byte	%00001010
		.byte	%00001010
		.byte	%00001010
		.byte	%00001010
		.byte	%00111111
		.byte	%00111111
;mid
		.byte	%11110000
		.byte	%11111100
		.byte	%10101100
		.byte	%10101011
		.byte	%10101011
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
;
		.byte	%10101010
		.byte	%10101011
		.byte	%10101011
		.byte	%01011111
		.byte	%01011111
		.byte	%10101011
		.byte	%10101011
		.byte	%10101010
;
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101011
		.byte	%10101011
		.byte	%10101100
		.byte	%11111100
		.byte	%11110000
;right
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%11000000
		.byte	%11000000
		.byte	%10110000
;
		.byte	%10110000
		.byte	%11000000
		.byte	%11000000
		.byte	%10101010
		.byte	%10101010
		.byte	%11000000
		.byte	%11000000
		.byte	%10110000
;
		.byte	%10110000
		.byte	%11000000
		.byte	%11000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
;
COLONISTS:
		.byte	%00000000
		.byte	%00000000
		.byte	%00000011
		.byte	%00000011
		.byte	%00001110
		.byte	%00001110
		.byte	%00111010
		.byte	%00111001
;
		.byte	%11101001
		.byte	%11100110
		.byte	%11100110
		.byte	%11100110
		.byte	%11100110
		.byte	%11100110
		.byte	%11100110
		.byte	%11101001
;
		.byte	%11101001
		.byte	%11101010
		.byte	%11101001
		.byte	%11101001
		.byte	%00111010
		.byte	%00111010
		.byte	%00001111
		.byte	%00001111
;mid
		.byte	%11111111
		.byte	%11111111
		.byte	%10101010
		.byte	%10101010
		.byte	%10010110
		.byte	%01010101
		.byte	%01101001
		.byte	%10101010
;
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%10101010
		.byte	%01101001
;
		.byte	%01101001
		.byte	%01101001
		.byte	%01101001
		.byte	%01101001
		.byte	%10101010
		.byte	%10101010
		.byte	%11111111
		.byte	%11111111
;right
		.byte	%00000000
		.byte	%00000000
		.byte	%11000000
		.byte	%11000000
		.byte	%10110000
		.byte	%10110000
		.byte	%10101100
		.byte	%01101100
;
		.byte	%01101011
		.byte	%10011011
		.byte	%10011011
		.byte	%10011011
		.byte	%10011011
		.byte	%10011011
		.byte	%10011011
		.byte	%01101011
;
		.byte	%01101011
		.byte	%10101011
		.byte	%01101011
		.byte	%01101011
		.byte	%10101100
		.byte	%10101100
		.byte	%11110000
		.byte	%11110000
;
; Thargoid mines $1C48-$1C87 (I-P)
GRENADE1:
		.byte	%00011000
		.byte	%00100100
		.byte	%01011010
		.byte	%10100101
		.byte	%10100101
		.byte	%01011010
		.byte	%00100100
		.byte	%00011000
GRENADE2:
		.byte	%00000000
		.byte	%00011000
		.byte	%00100100
		.byte	%01011010
		.byte	%01011010
		.byte	%00100100
		.byte	%00011000
		.byte	%00000000
HULLBLASTER1:
		.byte	%00000000
		.byte	%00011000
		.byte	%00111100
		.byte	%00111100
		.byte	%00011000
		.byte	%00111100
		.byte	%00011000
		.byte	%00000000
HULLBLASTER2:
		.byte	%10000001
		.byte	%01011010
		.byte	%00111100
		.byte	%00111100
		.byte	%00011000
		.byte	%00111100
		.byte	%01011010
		.byte	%10000001
SONICBOOMER1:
		.byte	%01000010
		.byte	%10000001
		.byte	%10011001
		.byte	%10111101
		.byte	%10111101
		.byte	%10011001
		.byte	%10000001
		.byte	%01000010
SONICBOOMER2:
		.byte	%00000000
		.byte	%00100100
		.byte	%01000010
		.byte	%01011010
		.byte	%01011010
		.byte	%01000010
		.byte	%00100100
		.byte	%00000000
HEARTSTOPPER1:
		.byte	%00100100
		.byte	%01111110
		.byte	%01111110
		.byte	%01111110
		.byte	%01111110
		.byte	%00111100
		.byte	%00011000
		.byte	%00000000
HEARTSTOPPER2:
		.byte	%01100110
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%01111110
		.byte	%00111100
		.byte	%00011000
;
; Sonic boomer waves $1C88-$1CA7 (Q-T)
SONICWAVE0:
		.byte	%00111100
		.byte	%11000011
		.byte	%00000000
		.byte	%00111100
		.byte	%01000010
		.byte	%00000000
		.byte	%00011000
		.byte	%00100100
SONICWAVE1:
		.byte	%00000010
		.byte	%00010010
		.byte	%10001001
		.byte	%01001001
		.byte	%01001001
		.byte	%10001001
		.byte	%00010010
		.byte	%00000010
SONICWAVE2:
		.byte	%00100100
		.byte	%00011000
		.byte	%00000000
		.byte	%01000010
		.byte	%00111100
		.byte	%00000000
		.byte	%11000011
		.byte	%00111100
SONICWAVE3:
		.byte	%01000000
		.byte	%01001000
		.byte	%10010001
		.byte	%10010010
		.byte	%10010010
		.byte	%10010001
		.byte	%01001000
		.byte	%01000000
;
; Function key: $1F30-$1F3F
F1:		.byte	$FF, $EA, $EF, $EF, $EB, $EF, $EF, $FF	; [F
		.byte	$FC, $EC, $EC, $EC, $EC, $EC, $EC, $FC	;  1]

;*********************************************************************
; SPRITE IMAGE DATA
;
; SPRITE #0-1-2:
; Omega Fury energy bolts
;
BOLTIMG0:
		.byte	%01000000
		.byte	%10100000
		.byte	%01000000
BOLTIMG1:
		.byte	%10100000
		.byte	%01000000
		.byte	%10100000
BOLTIMG2:
		.byte	%01000000
		.byte	%11100000
		.byte	%01000000
BOLTIMG3:
		.byte	%00000000
		.byte	%11100000
		.byte	%00000000
;
; SPRITE #3-4-5-6:
; phaser shot type for scout-cruiser-destroyer-carrier
;
PHASER0:
		.byte	%10010000
		.byte	%00000000
		.byte	%10010000
PHASER1:
		.byte	%01100000
		.byte	%10010000
		.byte	%01100000
PHASER2:
		.byte	%01100000
		.byte	%11110000
		.byte	%01100000
PHASER3:
		.byte	%11110000
		.byte	%11110000
		.byte	%11110000
;
; SPRITE #7:
; neutron bomb
;
NEUTRONBOMB:
		.byte	%11111111
		.byte	%00111100
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%11111111
		.byte	%01111110
		.byte	%00111100
;
; SPRITES #8-9-10-11:
; a mix of Thargoid scout, cruiser, destroyer, carrier
;
SCOUT0:	;
		.byte	%10000001
		.byte	%00111100
		.byte	%01111110
		.byte	%01111110
		.byte	%01111110
		.byte	%01111110
		.byte	%00111100
		.byte	%10000001
SCOUT1:	;
		.byte	%00100000
		.byte	%00011100
		.byte	%01111101
		.byte	%01111110
		.byte	%01111110
		.byte	%10111110
		.byte	%00111000
		.byte	%00000100
SCOUT2:	;
		.byte	%00010000
		.byte	%00101100
		.byte	%01111110
		.byte	%01111101
		.byte	%10111110
		.byte	%01111110
		.byte	%00110100
		.byte	%00001000
SCOUT3:	;
		.byte	%00001000
		.byte	%00110100
		.byte	%01111110
		.byte	%10111110
		.byte	%01111101
		.byte	%01111110
		.byte	%00101100
		.byte	%00010000
SCOUT4:	;
		.byte	%00000100
		.byte	%00111000
		.byte	%10111110
		.byte	%01111110
		.byte	%01111110
		.byte	%01111101
		.byte	%00011100
		.byte	%00100000
		;
CRUISER0:
		.byte	%10000001
		.byte	%10000001
		.byte	%10000001
		.byte	%10011001
		.byte	%10011001
		.byte	%10111101
		.byte	%11011011
		.byte	%10000001
CRUISER1:
		.byte	%00010000
		.byte	%00100000
		.byte	%01000000
		.byte	%11011000
		.byte	%00111001
		.byte	%00110010
		.byte	%00001100
		.byte	%00001000
CRUISER2:
		.byte	%11111111
		.byte	%01000000
		.byte	%00100000
		.byte	%01111000
		.byte	%01111000
		.byte	%00100000
		.byte	%01000000
		.byte	%11111111
CRUISER3:
		.byte	%00001000
		.byte	%00001100
		.byte	%00110010
		.byte	%00111001
		.byte	%11011000
		.byte	%01000000
		.byte	%00100000
		.byte	%00010000
CRUISER4:
		.byte	%10000001
		.byte	%11011011
		.byte	%10111101
		.byte	%10011001
		.byte	%10011001
		.byte	%10000001
		.byte	%10000001
		.byte	%10000001
CRUISER5:
		.byte	%00010000
		.byte	%00110000
		.byte	%01001100
		.byte	%10011100
		.byte	%00011011
		.byte	%00000010
		.byte	%00000100
		.byte	%00001000
CRUISER6:
		.byte	%11111111
		.byte	%00000010
		.byte	%00000100
		.byte	%00011110
		.byte	%00011110
		.byte	%00000100
		.byte	%00000010
		.byte	%11111111
CRUISER7:
		.byte	%00001000
		.byte	%00000100
		.byte	%00000010
		.byte	%00011011
		.byte	%10011100
		.byte	%01001100
		.byte	%00110000
		.byte	%00010000
DESTROYER0:
		.byte	%00011000
		.byte	%00011000
		.byte	%00111100
		.byte	%00111100
		.byte	%01011010
		.byte	%10000001
		.byte	%10000001
		.byte	%10000001
DESTROYER1:
		.byte	%00000011
		.byte	%01111111
		.byte	%10001110
		.byte	%00011110
		.byte	%00011010
		.byte	%00000010
		.byte	%00000010
		.byte	%00000100
DESTROYER2:
		.byte	%11100000
		.byte	%00010000
		.byte	%00001100
		.byte	%00011111
		.byte	%00011111
		.byte	%00001100
		.byte	%00010000
		.byte	%11100000
DESTROYER3:
		.byte	%00000100
		.byte	%00000010
		.byte	%00000010
		.byte	%00011010
		.byte	%00011110
		.byte	%10001110
		.byte	%01111111
		.byte	%00000011
DESTROYER4:
		.byte	%10000001
		.byte	%10000001
		.byte	%10000001
		.byte	%01011010
		.byte	%00111100
		.byte	%00111100
		.byte	%00011000
		.byte	%00011000
DESTROYER5:
		.byte	%00100000
		.byte	%01000000
		.byte	%01000000
		.byte	%01011000
		.byte	%01111000
		.byte	%01110001
		.byte	%11111110
		.byte	%11000000
DESTROYER6:
		.byte	%00000111
		.byte	%00001000
		.byte	%00110000
		.byte	%11111000
		.byte	%11111000
		.byte	%00110000
		.byte	%00001000
		.byte	%00000111
DESTROYER7:
		.byte	%11000000
		.byte	%11111110
		.byte	%01110001
		.byte	%01111000
		.byte	%01011000
		.byte	%01000000
		.byte	%01000000
		.byte	%00100000
		;
CARRIER0:
		.byte	%00000001
		.byte	%00000111
		.byte	%00011111
		.byte	%01111111
		.byte	%10011111
		.byte	%10011111
		.byte	%11111111
		.byte	%11111110

		.byte	%11111110
		.byte	%11111001
		.byte	%11100110
		.byte	%10011001
		.byte	%01100110
		.byte	%00011001
		.byte	%00000110
		.byte	%00000001

		.byte	%10000000
		.byte	%01100000
		.byte	%10011000
		.byte	%01100110
		.byte	%10011001
		.byte	%01100111
		.byte	%10011111
		.byte	%01111111

		.byte	%01111111
		.byte	%11111111
		.byte	%11111001
		.byte	%11111001
		.byte	%11111110
		.byte	%11111000
		.byte	%11100000
		.byte	%10000000

CARRIER1:
		.byte	%00000001
		.byte	%00000110
		.byte	%00011110
		.byte	%01111111
		.byte	%10111111
		.byte	%10101111
		.byte	%10101011
		.byte	%10101010

		.byte	%10101010
		.byte	%10101011
		.byte	%10101111
		.byte	%10111111
		.byte	%01111111
		.byte	%00011110
		.byte	%00000110
		.byte	%00000001

		.byte	%10000000
		.byte	%01100000
		.byte	%01111000
		.byte	%11111110
		.byte	%11111101
		.byte	%11110101
		.byte	%11010101
		.byte	%01010101

		.byte	%01010101
		.byte	%11010101
		.byte	%11110101
		.byte	%11111101
		.byte	%11111110
		.byte	%01111000
		.byte	%01100000
		.byte	%10000000

CARRIER2:
		.byte	%00000001
		.byte	%00000110
		.byte	%00011001
		.byte	%01100110
		.byte	%10011001
		.byte	%11100110
		.byte	%11111001
		.byte	%11111110

		.byte	%11111110
		.byte	%11111111
		.byte	%10011111
		.byte	%10011111
		.byte	%01111111
		.byte	%00011111
		.byte	%00000111
		.byte	%00000001

		.byte	%10000000
		.byte	%11100000
		.byte	%11111000
		.byte	%11111110
		.byte	%11111001
		.byte	%11111001
		.byte	%11111111
		.byte	%01111111

		.byte	%01111111
		.byte	%10011111
		.byte	%01100111
		.byte	%10011001
		.byte	%01100110
		.byte	%10011000
		.byte	%01100000
		.byte	%10000000
;
; SPRITE #12:
; Omega Fury fighter craft
;
FURY0:	;
		.byte	%00010000
		.byte	%00010000
		.byte	%00111000
		.byte	%01111100
		.byte	%11101110
		.byte	%11111110
		.byte	%10010010
		.byte	%00000000
FURY1:	;
		.byte	%00000100
		.byte	%00001100
		.byte	%00011100
		.byte	%01111100
		.byte	%11101100
		.byte	%00111110
		.byte	%00111110
		.byte	%00000010
FURY2:	;
		.byte	%00000010
		.byte	%01111110
		.byte	%11111100
		.byte	%01101100
		.byte	%00111100
		.byte	%00111100
		.byte	%00001100
		.byte	%00000100
FURY3:	;
		.byte	%11000000
		.byte	%11110000
		.byte	%01111110
		.byte	%01101100
		.byte	%01111000
		.byte	%00110000
		.byte	%00110000
		.byte	%00100000
FURY4:	;
		.byte	%11100000
		.byte	%01110000
		.byte	%01111000
		.byte	%11101110
		.byte	%01111000
		.byte	%01110000
		.byte	%11100000
		.byte	%00000000
FURY5:	;
		.byte	%00100000
		.byte	%00110000
		.byte	%00110000
		.byte	%01111000
		.byte	%01101100
		.byte	%01111110
		.byte	%11110000
		.byte	%11000000
FURY6:	;
		.byte	%00000100
		.byte	%00001100
		.byte	%00111100
		.byte	%00111100
		.byte	%01101100
		.byte	%11111100
		.byte	%01111110
		.byte	%00000010
FURY7:	;
		.byte	%00000010
		.byte	%00111110
		.byte	%00111110
		.byte	%11101100
		.byte	%01111100
		.byte	%00011100
		.byte	%00001100
		.byte	%00000100
FURY8:	;
		.byte	%00000000
		.byte	%10010010
		.byte	%11111110
		.byte	%11101110
		.byte	%01111100
		.byte	%00111000
		.byte	%00010000
		.byte	%00010000
FURY9:	;
		.byte	%10000000
		.byte	%11111000
		.byte	%11111000
		.byte	%01101110
		.byte	%01111100
		.byte	%01110000
		.byte	%01100000
		.byte	%01000000
FURYA:	;
		.byte	%01000000
		.byte	%01100000
		.byte	%01111000
		.byte	%01111000
		.byte	%01101100
		.byte	%01111110
		.byte	%11111100
		.byte	%10000000
FURYB:	;
		.byte	%00001000
		.byte	%00011000
		.byte	%00011000
		.byte	%00111100
		.byte	%01101100
		.byte	%11111100
		.byte	%00001110
		.byte	%00000110
FURYC:	;
		.byte	%00001110
		.byte	%00011100
		.byte	%00111100
		.byte	%11101110
		.byte	%00111100
		.byte	%00011100
		.byte	%00001110
		.byte	%00000000
FURYD:	;
		.byte	%00000110
		.byte	%00011110
		.byte	%11111100
		.byte	%01101100
		.byte	%00111100
		.byte	%00011000
		.byte	%00011000
		.byte	%00001000
FURYE:	;
		.byte	%10000000
		.byte	%11111100
		.byte	%01111110
		.byte	%01101100
		.byte	%01111000
		.byte	%01111000
		.byte	%01100000
		.byte	%01000000
FURYF:	;
		.byte	%01000000
		.byte	%01100000
		.byte	%01110000
		.byte	%01111100
		.byte	%01101110
		.byte	%11111000
		.byte	%11111000
		.byte	%10000000
;
; SPRITE #13:
; hit mark
;
HITMARK0:
		.byte	%00000000
		.byte	%00010000
		.byte	%00111000
		.byte	%01101100
		.byte	%00111000
		.byte	%00010000
		.byte	%00000000
HITMARK1:
		.byte	%00010000
		.byte	%01010100
		.byte	%00101000
		.byte	%11101110
		.byte	%00101000
		.byte	%01010100
		.byte	%00010000
;
; SPRITE #14:
; big hit mark
;
BIGHITMARK0:
		.byte	%00000000
		.byte	%00000000
		.byte	%00000001
		.byte	%00000001
		.byte	%00000001
		.byte	%00000001
		.byte	%00000011
		.byte	%00011110
		;
		.byte	%00000011
		.byte	%00000001
		.byte	%00000001
		.byte	%00000001
		.byte	%00000001
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%10000000
		.byte	%11110000
		;
		.byte	%10000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
BIGHITMARK1:
		.byte	%00000001
		.byte	%00000001
		.byte	%00000011
		.byte	%00000011
		.byte	%11000011
		.byte	%00110011
		.byte	%00001110
		.byte	%01111010
		;
		.byte	%00001110
		.byte	%00110011
		.byte	%11000011
		.byte	%00000011
		.byte	%00000011
		.byte	%00000001
		.byte	%00000001
		.byte	%00000000
		;
		.byte	%00000000
		.byte	%00000000
		.byte	%10000000
		.byte	%10000000
		.byte	%10000110
		.byte	%10011000
		.byte	%11100000
		.byte	%10111100
		;
		.byte	%11100000
		.byte	%10011000
		.byte	%10000110
		.byte	%10000000
		.byte	%10000000
		.byte	%00000000
		.byte	%00000000
		.byte	%00000000
		;
LOGO:
		.byte	%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00000000,%00000111,%11111111,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00000000,%00011111,%11111111,%11000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		;
		.byte	%00000000,%01111111,%11111111,%11110000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00000001,%11111000,%00000000,%11111100,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00000011,%11100000,%00000000,%00111110,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00000111,%11000001,%11111100,%00011111,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00001111,%00000111,%11111111,%00000111,%10000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00011110,%00011111,%11111111,%11000011,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11110000
		.byte	%00011100,%00111110,%00000011,%11100001,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11100000
		.byte	%00111100,%01111000,%00000000,%11110001,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11100000
		;
		.byte	%00111000,%11110001,%11111100,%01111000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%01110001,%11100111,%11111111,%00111100,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%01110001,%11001111,%11111111,%10011100,%01111111,%11100001,%11111111,%00000011,%11100000,%11100000,%00011111,%11111011,%10000011,%10111111,%11100011,%10000011,%10000000
		.byte	%11100011,%10011110,%00000011,%11001110,%01111111,%11110001,%11111111,%00001111,%11100000,%11100000,%00011111,%11111011,%10000011,%10111111,%11110011,%10000011,%10000000
		.byte	%11100011,%10011100,%00000001,%11001110,%01111111,%11111001,%11111111,%00011111,%11100000,%11100000,%00011111,%11111011,%10000011,%10111111,%11111001,%11000111,%00000000
		.byte	%11100111,%00111000,%00000000,%11100111,%01110000,%00111100,%00000000,%00111110,%00000000,%01110000,%00000000,%00000011,%10000011,%10000000,%00011001,%11000111,%00000000
		.byte	%11100111,%00111000,%00000000,%11100111,%01110000,%00011100,%00000000,%00111000,%00000000,%01110000,%00000000,%00000011,%10000011,%10000000,%00011000,%11101110,%00000000
		.byte	%11100111,%00111000,%00000000,%11100111,%01110011,%10011101,%11111111,%01111001,%11100001,%11110000,%00011111,%11111011,%10000011,%10111111,%11110000,%01111100,%00000000
		;
		.byte	%11100111,%00111000,%00000000,%11100111,%01110011,%10011101,%11111111,%01110001,%11100011,%10111000,%00011111,%11111011,%10000011,%10111111,%11100000,%01111100,%00000000
		.byte	%11100111,%00111000,%00000000,%11100111,%01110011,%10011101,%11111111,%01111001,%11100011,%10111000,%00011111,%11111011,%10000011,%10111111,%11110000,%00111000,%00000000
		.byte	%11100111,%00111000,%00000000,%11100111,%01110011,%10011101,%11000000,%00111000,%01100011,%10111000,%00011100,%00000011,%10000011,%10111000,%01110000,%01110000,%00000000
		.byte	%11100111,%00111000,%00000000,%11100111,%01110011,%10011101,%11000000,%00111110,%01100111,%00011100,%00011100,%00000011,%11000111,%10111000,%00111000,%01110000,%00000000
		.byte	%11100011,%10011100,%00000001,%11001110,%01110011,%10011101,%11111111,%00011111,%11100111,%11111100,%00011100,%00000011,%11111111,%10111000,%00111000,%11100000,%00000000
		.byte	%11100011,%10011110,%00000011,%11001110,%01110011,%10011101,%11111111,%00001111,%11100111,%11111100,%00011100,%00000001,%11111111,%00111000,%00111001,%11000000,%00000000
		.byte	%01110001,%11001111,%11111111,%10011100,%01110011,%10011101,%11111111,%00000011,%11001111,%11111110,%00011100,%00000000,%01111100,%00111000,%00111001,%11000000,%00000000
		.byte	%01110001,%11100111,%11111111,%00111100,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		;
		.byte	%00111000,%11110001,%11111100,%01111000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00111100,%01111000,%00000000,%11110001,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%00000000,%00000000
		.byte	%00011100,%00111110,%00000011,%11100001,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111110,%00000000,%00000000
		.byte	%00011110,%00011111,%11111111,%11000011,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111111,%11111110,%00000000,%00000000
		.byte	%00001111,%00000111,%11111111,%00000111,%10000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00000111,%11000001,%11111100,%00011111,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00000011,%11100000,%00000000,%00111110,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00000001,%11111000,%00000000,%11111100,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		;
		.byte	%00000000,%01111111,%11111111,%11110000,%00000011,%11100000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00000000,%00011111,%11111111,%11000000,%00000100,%00010011,%10111001,%00010000,%00110001,%00110011,%10110011,%10000010,%10101011,%00011011,%10000000,%00000000,%00000000
		.byte	%00000000,%00000111,%11111111,%00000000,%00000101,%11010000,%10101011,%00110000,%00101010,%10101010,%00101001,%00000010,%10101010,%10100001,%00000000,%00000000,%00000000
		.byte	%00000000,%00000000,%00000000,%00000000,%00000101,%00010001,%10101001,%00010000,%00110010,%10110011,%00110001,%00000011,%10101011,%00010001,%00000000,%00000000,%00000000
		.byte	%00000000,%00000000,%00000000,%00000000,%00000101,%00010010,%00101001,%00010000,%00101010,%10101010,%00101001,%00000010,%10101010,%10001001,%00000000,%00000000,%00000000
		.byte	%00000000,%00000000,%00000000,%00000000,%00000101,%11010011,%10111011,%10111000,%00101001,%00110011,%10101001,%00000010,%10111010,%10110001,%00000000,%00000000,%00000000
		.byte	%00000000,%00000000,%00000000,%00000000,%00000100,%00010000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		.byte	%00000000,%00000000,%00000000,%00000000,%00000011,%11100000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000
		;
		; a simple bit-mask for register loops
;MASK:	.byte	$01, $02, $04, $08, $10, $20, $40, $80

