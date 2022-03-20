
;===========================================================================
; Graphics testing - VGA 640*480 16 colours
;===========================================================================

const width=80		;80 bytes per line
const gc_index=3ceh
const gc_bit_mask=8h
const gc_graph_mode=5

proc init_640x480
    {
    reg ax=0012h
    int 10h		;set video mode 640*480
    vseg=0a000h 	;video segment
    }

proc set_pixel(x,y,c)
    {
    offset=y*width+(x/8)
    bitmask=peekb (masks+(x and 7))

    reg dx=gc_index
    reg ax=bitmask*256+gc_bit_mask
    inline 0efh 	;out dx,ax

    reg ax=200h+gc_graph_mode
    inline 0efh 	;out dx,ax

    a=vseg[offset]b
    vseg[offset]b=c

    reg dx=gc_index
    reg ax=0ff00h+gc_bit_mask
    inline 0efh 	;out dx,ax

    reg ax=0000h+gc_graph_mode
    inline 0efh 	;out dx,ax
    }

;== start test =============================================================

init_640x480
col=0

for y=0 to 479
col=y
    for x=0 to 639

	set_pixel(x,y,col)
	col++

    next x
next y


while key<>27
    {
    x=rnd mod 640
    y=rnd mod 350
    col=rnd mod 15
    set_pixel(x,y,col)
    }


;wait for key=27
screen 3
stop

;===========================================================================

masks: datab 1,2,4,8,16,32,64,128
