;
; CLONE INVASION
;

; Code and graphics by TMR
; Music by Odie


; A quick, written-from-scratch and expanded copy of the C64 demo
; Planet Invasion by the Harlow Cracking Service.
; Coded for C64CrapDebunk.Wordpress.com

; Notes: this source is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/
; Compression is handled with Exomizer 2 which can be downloaded at
; http://hem.bredband.net/magli143/exo/

; build.bat will call both to create an assembled file and then the
; crunched release version.


; Memory Map
; $0801 - $0fff		program code/data
; $1000 - $1fff		music
; $2800 - $2dff		character set
; $2e00 - $2fff		scroller sprites
; $3000 - $3fff		sprites
; $4000 -		scrolling message
; $fc00 - $feff		translation tables


; Select an output filename
		!to "clone_invasion.prg",cbm


; Pull in the binary data
		* = $1000
music		!binary "binary\extreme_force_1.prg",,2

		* = $2800
char_data	!binary "binary\plain_font_8x8.chr"

		* = $3000
		!binary "binary\sprites.spr"


; Raster split positions
raster_1_pos	= $00
raster_2_pos	= $58
raster_3_pos	= $9c
raster_4_pos	= $c8
raster_5_pos	= $ee

; Label assignments
raster_num	= $50
anim_timer	= $51
char_count	= $52
char_move_timer	= $53

scroll_pos	= $54		; two bytes used
char_data_pos	= $56		; two bytes used

scroll_ram	= $2e15

; Bit shifted translation tables used by the background effect
translate_1px	= $fc00
translate_2px	= $fd00
translate_3px	= $fe00


; Add a BASIC startline
		* = $0801
		!word code_start-2
		!byte $40,$00,$9e
		!text "2066"
		!byte $00,$00,$00


; Entry point for the code
		* = $0812

; Stop interrupts, disable the ROMS and set up NMI and IRQ interrupt pointers
code_start	sei

		lda #$35
		sta $01

		lda #<nmi_int
		sta $fffa
		lda #>nmi_int
		sta $fffb

		lda #<irq_int
		sta $fffe
		lda #>irq_int
		sta $ffff

; Set the VIC-II up for a raster IRQ interrupt
		lda #$7f
		sta $dc0d
		sta $dd0d

		lda $dc0d
		lda $dd0d

		lda #raster_1_pos
		sta $d012

		lda #$1b
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Initialise some of our own labels
		lda #$01
		sta raster_num

		lda #$00
		sta anim_timer
		sta char_count
		jsr scroll_reset

; Create the translation tables used to speed up the parallax
		ldx #$00
translate_make	txa
		lsr
		bcc *+$04
		ora #$80
		sta translate_1px,x
		lsr
		bcc *+$04
		ora #$80
		sta translate_2px,x
		lsr
		bcc *+$04
		ora #$80
		sta translate_3px,x
		inx
		bne translate_make

; Set up the screen RAM
		ldx #$00
screen_init	lda #$1b
		sta $0400,x
		sta $0720,x
		lda #$0b
		sta $d800,x
		sta $db20,x

		lda #$1c
		sta $04c8,x
		sta $0658,x
		lda #$04
		sta $d8c8,x
		sta $da58,x

		lda #$1d
		sta $0590,x
		lda #$0e
		sta $d990,x

		inx
		cpx #$c8
		bne screen_init

; Initialise the music driver
		lda #$00
		jsr music+$00


; Restart the interrupts
		cli

; Infinite loop - all of the code is executing on the interrupt
		jmp *


; IRQ interrupt handler
irq_int		pha
		txa
		pha
		tya
		pha

		lda $d019
		and #$01
		sta $d019
		bne int_go
		jmp irq_exit

; An interrupt has triggered
int_go		lda raster_num

		cmp #$02
		bne *+$05
		jmp irq_rout2

		cmp #$03
		bne *+$05
		jmp irq_rout3

		cmp #$04
		bne *+$05
		jmp irq_rout4

		cmp #$05
		bne *+$05
		jmp irq_rout5


; Raster split 1
; Set up the VIC-II for the start of the frame
irq_rout1	lda #$00
		sta $d020
		lda #$06
		sta $d021

		lda #$1a
		sta $d018

; Set up the upper border sprites
		lda #$ff
		sta $d015

		ldx #$00
		lda #$18
set_ub_sprt_x	sta $d000,x
		clc
		adc #$30
		inx
		inx
		cpx #$0e
		bne set_ub_sprt_x
		lda #$60
		sta $d010

		ldx #$00
		lda #$08
set_ub_sprt_y	sta $d001,x
		inx
		inx
		cpx #$0e
		bne set_ub_sprt_y

		lda #$7f
		sta $d017
		sta $d01d
		lda #$00
		sta $d01c

; Wait for the start of the upper border parallax
		ldx #$3c
		dex
		bne *-$01
		nop

; Write character data to the "ghostbyte" once per scanline
		ldx #$00
ub_ghost	lda char_data+$200,x
		sta $3fff
		ldy #$06
		dey
		bne *-$01
		inx
		cpx #$28
		bne ub_ghost

; Set the background colour for the main screen
		lda #$00
		sta $d021

; Place first row of sprites and configure other registers
		ldx #$00
		lda #$25
sprite_x_set_1	sta $d000,x
		clc
		adc #$2d
		inx
		inx
		cpx #$0e
		bne sprite_x_set_1

		lda sprite_7_x+$00
		sta $d00e
		lda #$60
		ora sprite_7_msb+$00
		sta $d010

		ldx #$00
		lda #$40
sprite_y_set_1	sta $d001,x
		inx
		inx
		cpx #$10
		bne sprite_y_set_1

		ldx #$00
		ldy #$00
sprite_dp_set_1	ldy sprite_settings+$00,x
		lda sprite_pointers,y
		sta $07f8,x
		lda sprite_colours,y
		sta $d027,x
		iny
		inx
		cpx #$08
		bne sprite_dp_set_1

		lda #$09
		sta $d025
		lda #$01
		sta $d026
		lda #$ff
		sta $d01c
		lda #$00
		sta $d017
		sta $d01d

; Set interrupt handler for split 3
		lda #$02
		sta raster_num
		lda #raster_2_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


; Raster split 2
; Place second row of sprites
irq_rout2	lda sprite_7_x+$01
		sta $d00e
		lda #$60
		ora sprite_7_msb+$01
		sta $d010

		ldx #$00
		lda #$72
sprite_y_set_2	sta $d001,x
		inx
		inx
		cpx #$10
		bne sprite_y_set_2

		ldx #$00
		ldy #$00
sprite_dp_set_2	ldy sprite_settings+$08,x
		lda sprite_pointers,y
		sta $07f8,x
		lda sprite_colours,y
		sta $d027,x
		iny
		inx
		cpx #$08
		bne sprite_dp_set_2

		lda #$02
		sta $d025

; Play the music
		jsr music+$03

; Set interrupt handler for split 3
		lda #$03
		sta raster_num
		lda #raster_3_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


; Raster split 3
; Place third row of sprites
irq_rout3	lda sprite_7_x+$02
		sta $d00e
		lda #$60
		ora sprite_7_msb+$02
		sta $d010

		ldx #$00
		lda #$a4
sprite_y_set_3	sta $d001,x
		inx
		inx
		cpx #$10
		bne sprite_y_set_3

		ldx #$00
		ldy #$00
sprite_dp_set_3	ldy sprite_settings+$10,x
		lda sprite_pointers,y
		sta $07f8,x
		lda sprite_colours,y
		sta $d027,x
		iny
		inx
		cpx #$08
		bne sprite_dp_set_3

		lda #$0b
		sta $d025

; Move the scrolling message
		ldx #$00
scroll_mover	asl scroll_ram+$182,x
		rol scroll_ram+$181,x
		rol scroll_ram+$180,x

		rol scroll_ram+$142,x
		rol scroll_ram+$141,x
		rol scroll_ram+$140,x

		rol scroll_ram+$102,x
		rol scroll_ram+$101,x
		rol scroll_ram+$100,x

		rol scroll_ram+$0c2,x
		rol scroll_ram+$0c1,x
		rol scroll_ram+$0c0,x

		rol scroll_ram+$082,x
		rol scroll_ram+$081,x
		rol scroll_ram+$080,x

		rol scroll_ram+$042,x
		rol scroll_ram+$041,x
		rol scroll_ram+$040,x

		rol scroll_ram+$002,x
		rol scroll_ram+$001,x
		rol scroll_ram+$000,x
		inx
		inx
		inx
		cpx #$18
		bne scroll_mover

; Do we need to copy a new character to the scroller?
		ldx char_count
		inx
		cpx #$08
		bne cc_xb

; Yes, so fetch a byte from the scroll text
		ldy #$00
scroll_mread	lda (scroll_pos),y
		bne scroll_okay
		jsr scroll_reset
		jmp scroll_mread

; Configure the character copier
scroll_okay	sta char_data_pos+$00
		lda #$00
		asl char_data_pos+$00
		rol
		asl char_data_pos+$00
		rol
		asl char_data_pos+$00
		rol
		clc
		adc #>char_data
		sta char_data_pos+$01

; And copy the definition
		ldx #$00
		ldy #$00
scroll_def_copy	lda (char_data_pos),y
		sta scroll_ram+$182,x
		iny
		inx
		inx
		inx
		cpx #$18
		bne scroll_def_copy

; Nudge the scroller onto the next character
		inc scroll_pos+$00
		bne *+$04
		inc scroll_pos+$01

		ldx #$00
cc_xb		stx char_count


; Set interrupt handler for split 4
		lda #$04
		sta raster_num
		lda #raster_4_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


; Raster split 4
; Place fourth row of sprites
irq_rout4	lda sprite_7_x+$03
		sta $d00e
		lda #$60
		ora sprite_7_msb+$03
		sta $d010

		ldx #$00
		lda #$d6
sprite_y_set_4	sta $d001,x
		inx
		inx
		cpx #$10
		bne sprite_y_set_4

		ldx #$00
		ldy #$00
sprite_dp_set_4	ldy sprite_settings+$18,x
		lda sprite_pointers,y
		sta $07f8,x
		lda sprite_colours,y
		sta $d027,x
		iny
		inx
		cpx #$08
		bne sprite_dp_set_4

		lda #$06
		sta $d025


; Shift the roaming sprites ready for the next frame
		ldx #$00
sprite_update	lda sprite_7_dir,x
		bne su_right

; Move roaming sprite left
su_left		ldy sprite_7_x,x
		dey
		cpy #$ff
		bne su_left_skip
		lda sprite_7_msb,x
		eor #$80
		sta sprite_7_msb,x
su_left_skip	tya
		sta sprite_7_x,x

		cmp #$24
		bne su_count
		lda sprite_7_msb,x
		bne su_count

		lda #$01
		sta sprite_7_dir,x

		jmp su_count

; Move roaming sprite right
su_right	ldy sprite_7_x,x
		iny
		bne su_right_skip
		lda sprite_7_msb,x
		eor #$80
		sta sprite_7_msb,x
su_right_skip	tya
		sta sprite_7_x,x

; Roaming sprite direction reverse check
		cmp #$32
		bne su_count
		lda sprite_7_msb,x
		cmp #$80
		bne su_count

		lda #$00
		sta sprite_7_dir,x

su_count	inx
		cpx #$04
		bne sprite_update


; Update sprite animations
		ldx anim_timer
		inx
		cpx #$03
		bne at_xb

		ldx #$00
anim_update	lda sprite_pointers,x
		clc
		adc #$01
		cmp anim_ends,x
		bne au_skip
		lda anim_starts,x
au_skip		sta sprite_pointers,x
		inx
		cpx #$08
		bne anim_update

		ldx #$00
at_xb		stx anim_timer


; Set interrupt handler for split 5
		lda #$05
		sta raster_num
		lda #raster_5_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


; Raster split 5
; Set up the lower border scroller sprites
irq_rout5	ldx #$00
		lda #$18
set_lb_sprt_x	sta $d000,x
		clc
		adc #$30
		inx
		inx
		cpx #$0e
		bne set_lb_sprt_x
		lda #$60
		sta $d010

		ldx #$00
		lda #$fa
set_lb_sprt_y	sta $d001,x
		inx
		inx
		cpx #$0e
		bne set_lb_sprt_y

		ldx #$00
		lda #$98+$20
set_lb_sprt_dp	sta $07f8,x
		clc
		adc #$01
		inx
		cpx #$07
		bne set_lb_sprt_dp

		ldx #$00
		lda #$0f
set_lb_sprt_col	sta $d027,x
		inx
		cpx #$08
		bne set_lb_sprt_col

		lda #$7f
		sta $d017
		sta $d01d
		lda #$00
		sta $d01c


; First part of opening the upper and lower borders
		lda #$f8
		cmp $d012
		bne *-$03
		lda #$14
		sta $d011

; Set the "ghostbyte" in advance for the first line of the border
		lda char_data+$200
		sta $3fff

; Wait for the end of the last screen scanline to set screen colour
		ldx #$1f
		dex
		bne *-$01
		bit $ea
		lda #$06
		sta $d021

; Wait until the border starts
		lda #$fb
		cmp $d012
		bne *-$03
		ldx #$06
		dex
		bne *-$01


; Write character data to the "ghostbyte" once per scanline
; (Also handles the second half of opening the borders and moves
; the character data to the right)
		ldx #$01
lb_ghost	ldy char_data+$200,x
		sty $3fff
		lda translate_1px,y
		sta char_data+$200,x

		lda #$1b
		sta $d011

		ldy #$02
		dey
		bne *-$01
		nop
		nop

		inx
		cpx #$28
		bne lb_ghost

; Wait until the end of the current scanline and set the
; "ghostbyte" to $ff
		nop
		nop
		nop
		nop
		nop

		lda #$ff
		sta $3fff


; Update the background characters
		jsr char_mover


; Set interrupt handler for split 1
		lda #$01
		sta raster_num
		lda #raster_1_pos
		sta $d012


; Restore registers and exit IRQ interrupt
irq_exit	pla
		tay
		pla
		tax
		pla
nmi_int		rti


; Subroutine to reset the scrolling message
scroll_reset	lda #<scroll_text
		sta scroll_pos+$00
		lda #>scroll_text
		sta scroll_pos+$01
		rts


; Subroutine to update the characters
char_mover	inc char_move_timer
		lda char_move_timer
		and #$01
		bne char_mover_2

char_mover_1	ldx #$00
cm1_loop	ldy char_data+$0d8,x
		lda translate_1px,y
		sta char_data+$0d8,x

		ldy char_data+$0e0,x
		lda translate_2px,y
		sta char_data+$0e0,x

		ldy char_data+$0e8,x
		lda translate_2px,y
		sta char_data+$0e8,x

		inx
		cpx #$08
		bne cm1_loop

		rts

char_mover_2	ldx #$00
cm2_loop	ldy char_data+$0d8,x
		lda translate_2px,y
		sta char_data+$0d8,x

		ldy char_data+$0e0,x
		lda translate_2px,y
		sta char_data+$0e0,x

		ldy char_data+$0e8,x
		lda translate_3px,y
		sta char_data+$0e8,x

		inx
		cpx #$08
		bne cm2_loop

		rts


; Hardware sprite data pointers for the main screen
sprite_pointers	!byte $c0,$c8,$cc,$d4,$da,$e2,$eb,$f3

anim_starts	!byte $c0,$c8,$cc,$d4,$da,$e2,$eb,$f3
anim_ends	!byte $c8,$cc,$d4,$da,$e2,$eb,$f3,$fc

; Hardware sprite colours for the main screen
sprite_colours	!byte $0f,$0a,$07,$03,$05,$04,$0d,$0e

; Table saying which sprites are - one !byte statement is
; one row of sprites
sprite_settings	!byte $01,$00,$02,$04,$03,$05,$06,$07
		!byte $02,$04,$03,$05,$06,$00,$01,$07
		!byte $04,$03,$05,$06,$00,$01,$02,$07
		!byte $06,$00,$02,$04,$03,$05,$01,$07

; Sprite positions for the "roaming" objects
sprite_7_x	!byte $20,$d0,$80,$30
sprite_7_msb	!byte $80,$00,$00,$00

; Direction of movement ($00 is left, $01 is right)
sprite_7_dir	!byte $01,$01,$01,$01


; The all-important scrolling message
		* = $4000
scroll_text	!scr "hello, good evening and welcome..."
		!scr "    "

		!scr "the aliens are back again around twenty eight years "
		!scr "after the first invasion...  and they've turned up "
		!scr "with extra colours and parallax!"
		!scr "    "

		!scr "this started out as a fairly straight clone of the harlow "
		!scr "cracking service demo ",$22,"planet invasion",$22,$20

		!scr "which was built from the ground upwards without reverse "
		!scr "engineering the original code."
		!scr "    "

		!scr "there are a couple of tweaks here and there though;  "
		!scr "the four roaming sprites can travel further across the "
		!scr "screen, sprite colours are set on an object by object "
		!scr "basis (as well as the darker multicolour being split "
		!scr "between sprite rows) and i've added some ghostbyte splits "
		!scr "in the upper and lower borders to give an extra layer of "
		!scr "parallax as well."
		!scr "    "

		!scr "this took about six hours to put together and probably "
		!scr "around the same again to add the upper/lower border effects "
		!scr "and then tidy things up internally so it wouldn't be too "
		!scr "embarrassing to release the source to github!"
		!scr "    "

		!scr "coding and graphics by t.m.r with music by odie"
		!scr "    "

		!scr "greetings to all 8-bit fans from the past, present and "
		!scr "indeed future, along with the harlow cracking service "
		!scr "and anyone daft enough to read the c64cd blog."
		!scr "    "

		!scr "i think this was about my fifth scrolltext in the "
		!scr "space of a month(!) and i've nothing else to say, "
		!scr "so this was t.m.r signing off... .. .  .        "
		!scr "    "

		!byte $00		; end of text marker