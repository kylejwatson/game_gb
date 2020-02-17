;;FIGURE OUT WHY MAP DOESNT WORK

include "gbhw.inc"

include "dma.inc"		; allows us to use dma_Copy2HRAM macro
include "sprite.inc"		; gives us spr_* macros to modify all sprites



include "tiles.z80"
include "testmap.z80"

;-------------- INTERRUPT VECTORS ------------------------
; specific memory addresses are called when a hardware interrupt triggers

; Vertical-blank triggers each time the screen finishes drawing. Video-RAM
; (VRAM) is only available during VBLANK. So this is when updating OAM /
; sprites is executed.
SECTION "Vblank", ROM0[$0040]
	JP	DMA_ROUTINE

SECTION "LCDC", ROM0[$0048]
	reti

SECTION "Timer", ROM0[$0050]
	reti

SECTION "Serial", ROM0[$0058]
	reti

SECTION "Joypad", ROM0[$0060]
	reti
;----------- END INTERRUPT VECTORS -------------------

SECTION "ROM_entry_point", ROM0[$0100]	; ROM is given control from boot here
	nop
	jp	code_begins

;------------- BEGIN ROM HEADER ----------------
; The gameboy reads this info (before handing control over to ROM)
SECTION "rom header", ROM0[$0104]
	NINTENDO_LOGO
	ROM_HEADER	"macro and dma  "

; here's where you can include additional .asm modules
include "memory.asm"

	SpriteAttr	copyright	; declare "copyright" as a sprite

code_begins:
	di	; disable interrupts
	ld	SP, $FFFF	; set stack to top of HRAM

    call	lcd_Stop

	; load ascii tiles (inserted below with chr_IBMPC1 macro) into VRAM
	ld	hl, Tiles	; ROM address where we insert ascii tiles
	ld	de, _VRAM	; destination. Going to copy ascii to video ram
	; bc = byte-count. Aka how many bytes to copy
	ld	bc, TilesEnd - Tiles
	call	mem_CopyVRAM

    ld	hl, Map
	ld	de, _SCRN0
	ld	bc, MAP_LENGTH
	call	mem_CopyVRAM
    ld	hl, Map1
	ld	de, _SCRN1
	ld	bc, Map1End - Map1
	call	mem_CopyVRAM

	ld	a, [rLCDC]
	or	LCDCF_ON
	or	LCDCF_OBJON	; enable sprites through "OBJects ON" flag
	or	LCDCF_OBJ8	; enable 8bit wide sprites (vs. 16-bit wide)
	ld	[rLCDC], a	; turn LCD back on

	; need to set palette from [black & white] to [four shades of grey]
	ld	a, %11100100	; load pallette colors (to 4 shades)
	; each shade is 2 bits. So we set darkest to lightest using 11100100
	; 11 (black) 10 (dark) 01 (light) 00 (white)
	ld	[rBGP], a	; set background pallet
	ld	[rOBP0], a	; set sprite/obj pallete 0
	ld	[rOBP1], a	; set sprite/ obj pallete 1


	dma_Copy2HRAM	; sets up routine from dma.inc that updates sprites

	ld	a, IEF_VBLANK	; --
	ld	[rIE], a	; Set only Vblank interrupt flag
	ei			; enable interrupts. Only vblank will trigger


	; don't need to wait for vblank since DMA_routine is now called
	; automatically when that happens

; ----
; DMA_ROUTINE is called each vblank now handles moving data data starting at
; _RAM ($C000 + 160) into $FE00. So now we write sprite data at $C000
; There are a total of 40 sprites available to manipulate.
; Each sprite has 4 attributes that are set in sequential bytes in memory:
;	X coordinate
;	Y coordinate
;	Tile #  (relative to start of tiles in VRAM: $8000)
;	Sprite Flags  (such as, visible, priority, X & Y flip)


	; see where we declare "copyright" as a sprite-variable above

	; set X=20, Y=10, Tile=$19, Flags=0
	PutSpriteXAddr	copyright, 10
	PutSpriteYAddr	copyright, 100
	sprite_PutTile	copyright, 1
	sprite_PutFlags	copyright, $00

_MAP = $C000 + 160
	ld hl, Map
	ld de, _MAP
	ld bc, MAP_LENGTH
	call mem_Copy
_TIMES = _MAP + MAP_LENGTH
	ld hl, Times8
	ld de, _TIMES
	ld bc, TIMES_LENGTH
	call mem_Copy
_WRAM = _TIMES + TIMES_LENGTH

;get background tile at coord
GetBackgroundTile:	MACRO
	ld a, \1
	srl a
	srl a
	srl a
	ld b, \2
	srl b
	srl b
	srl b

	add b
	ld l, a
	ld h, 0
	ld bc, _MAP
	add hl, bc
	ld a, [hl]
	ENDM

UP_VEL = _WRAM
DOWN_VEL = _WRAM+1

xor a
ld [UP_VEL], a
ld [DOWN_VEL], a
.loop
	halt	; halts cpu until interrupt triggers (vblank)
 	; by halting, we ensure that .loop only runs only each screen-refresh,
	; so only 60fps. That makes the sprite movement here manageable
	nop
    call jpad_GetKeys
	and PADF_LEFT
	jr z, .skip_left
    GetSpriteXAddr copyright
    cp 1
    jp nc, .skip_left_scroll
	ld	a, [rSCX]
	sub 2
	ld	[rSCX], a
	jr .skip_left
.skip_left_scroll
    MoveLeft copyright, 2
.skip_left
	ld a, b
	and PADF_RIGHT
	jr z, .skip_right
    GetSpriteXAddr copyright
	ld d, b
	GetBackgroundTile a, 0
	cp 4
	ld b, d
	jp z, .skip_right
    cp 160-8
    jp c, .skip_right_scroll
	ld	a, [rSCX]
	add 2
	ld	[rSCX], a
	jr .skip_right
.skip_right_scroll
    MoveRight copyright, 2
.skip_right
	ld a, [UP_VEL]
	ld c, a
	and a
	jr z, .fall_down
	MoveUp copyright, c
	dec c
	ld a, c
	ld [UP_VEL], a
	jp .loop
.fall_down
	;check for tile

	ld a, [DOWN_VEL]
	ld d, a
	cp 9
	jr z, .skip_jump
	MoveDown copyright, d
	inc d
	ld a, d
	ld [DOWN_VEL], a
	jp .loop
.skip_jump
	ld a, b
	and PADF_UP
	jp z, .loop
	ld a, 8
	ld [UP_VEL], a
	xor a
	ld [DOWN_VEL], a
	jp	.loop		; start up at top of .loop label. Repeats each vblank


; You can turn off LCD at any time, but it's bad for LCD if NOT done at vblank
lcd_Stop:
	ld	a, [rLCDC]	; LCD-Config
	and	LCDCF_ON	; compare config to lcd-on flag
	ret	z		; return if LCD is already off
.wait4vblank
	ldh	a, [rLY]   ; ldh is a faster version of ld if in [$FFxx] range
	cp	145  ; are we at line 145 yet?  (finished drawing screen then)
	jr	nz, .wait4vblank
.stopLCD
	ld	a, [rLCDC]
	xor	LCDCF_ON	; XOR lcd-on bit with lcd control bits. (toggles LCD off)
	ld	[rLCDC], a	; `a` holds result of XOR operation
	ret
jpad_GetKeys:
; Uses AF, B
; get currently pressed keys. Register A will hold keys in the following
; order: MSB --> LSB (Most Significant Bit --> Least Significant Bit)
; Down, Up, Left, Right, Start, Select, B, A
; This works by writing

	; get action buttons: A, B, Start / Select
	ld	a, JOYPAD_BUTTONS; choose bit that'll give us action button info
	ld	[rJOYPAD], a; write to joypad, telling it we'd like button info
	ld	a, [rJOYPAD]; gameboy will write (back in address) joypad info
	ld	a, [rJOYPAD]
	cpl		; take compliment
	and	$0f	; look at first 4 bits only  (lower nibble)
	swap	a	; place lower nibble into upper nibble
	ld	b, a	; store keys in b
	; get directional keys
	ld	a, JOYPAD_ARROWS
	ld	[rJOYPAD], a ; write to joypad, selecting direction keys
	ld	a, [rJOYPAD]
	ld	a, [rJOYPAD]
	ld	a, [rJOYPAD]	; delay to reliablly read keys
	ld	a, [rJOYPAD]	; since we've just swapped from reading
	ld	a, [rJOYPAD]	; buttons to arrow keys
	ld	a, [rJOYPAD]
	cpl			; take compliment
	and	$0f		; keep lower nibble
	or	b		; combine action & direction keys (result in a)
	ld	b, a

	ld	a, JOYPAD_BUTTONS | JOYPAD_ARROWS
	ld	[rJOYPAD], a		; reset joypad

	ld	a, b	; register A holds result. Each bit represents a key
	ret
