
;==============================================================================
; sprite (raster graphics) editor, (c) Peter Campbell Software 1994
;==============================================================================

;revised program from earlier versions - updated for new sprite library files
;and 256 colour VGA (320*200 pixels).

#short
#errors off
#window memory 4000

;== vars/contstants ===========================================================

const max_size=20	;16 bytes for name, 4 bytes for address
const max_sprites=800

var xbase,ybase
var wcolour,current_colour
var no_of_sprites,old_sprite
var event_x,event_y
var mouse_key

var32 sprite_addr,disk_addr
var32 time1,time2

const sprite_x=250:sprite_y=96

const save_x=32,save_y=18
const demo_x=32,demo_y=20
const cancel_x=31,cancel_y=22

work_area ? 2+64*40	;maximum sprite size

waste? 100

;== procedures ================================================================

#include cmd_line.fi
#include extinput.fi
#include fsort.fi
#include vga.fi
#include mouse.fi

proc abort_error(m)
    {
    print bios
    print bios "ERROR ";chr 34;
    while peekb m print bios chr peekb m;:m++
    print bios chr 34;
    beep
    print bios
    stop
    }

proc initialise
    {
    sprite_seg=allocate (max_size*max_sprites)/16+1	;16K

    open #1,sprite_file
    #long
    if error then
	{
	#short
	if error<>2 then abort_error("Can't Open Library File")
	create #1,sprite_file
	if error then abort_error("Can't Create Library File")
	fill max_size*max_sprites/2 from sprite_seg|0 with 0ffffh
	sprite_changes=1
	}
    else
	{
	r=read #1,max_size*max_sprites to sprite_seg|0
	sprite_changes=0
	}

    use_mouse=mouse_init
    }

proc shutdown
    {
    cursor 24,0
    print bios
    curtoloc:colour 15
    repeat 80 print " ";

    if sprite_changes then
	{
	seek #1,0
	write #1,max_size*max_sprites from sprite_seg|0
	close #1
	}

    stop
    }

;==============================================================================

function delay(d)
    {
    time1=timer

    forever
	{
	time2=timer
	if32 (time2-time1)>=d then return
	if keypressed or mouse_key then return
	}
    }

;==============================================================================

proc setup_screen
    {
    screen 3:colour 15:cls

    colour 4		;brick tiles
    for y=0 to 24
	locate y,0
	repeat 16
	    {
	    if (y and 1)=0 then      print "ÁÄÄÂÄ";
	    else		     print "ÂÄÄÁÄ";
	    }
    next y

    open window window_title
    }

;==============================================================================

proc show_message(sm)
    {
    locate 24,0
    colour 112

    print " ";
    x=1
    while x<80
	{
	c=peekb sm
	if c then print chr c;:sm++
	else print " ";
	x++
	}
    }

;==============================================================================

function sprite_defined(sprite_no)
    {
    m=(sprite_no-1)*max_size
    if sprite_seg[m]b=255 then return 0
    return 1
    }

;==============================================================================

function list_sprites(list_no)
    {
    sp=list_no and 1111111111110000b

    y=ybase
    m=sp*max_size

    repeat 16
	{
	locate y,xbase
	if sp=list_no then
	    {
	    colour 79
	    loctocur
	    }
	else colour wcolour

	if sprite_seg[m]b<>0ffh then
	    {
	    m1=m
	    print "  ";
	    repeat 16 print chr sprite_seg[m1]b;:m1++
	    print "  ";
	    }
	else
	    {
	    repeat 20 print " ";
	    }

	y++:sp++
	m+=max_size
	}
    }

;==============================================================================

function calc_no_of_sprites
    {
    no_of_sprites=0
    n=1:m=0
    while n<=max_sprites
	{
	if sprite_seg[m]b<>255 then no_of_sprites=n
	n++:m+=max_size
	}
    }

;==============================================================================

function select_sprite(sprite_no)
    {
    open window window_select
    xbase=peekb (window_select+2)+1
    ybase=peekb (window_select+3)+1
    wcolour=peekb (window_select+6)

    if use_mouse then mouse_init

    calc_no_of_sprites
    sort(sprite_seg,0,max_size,no_of_sprites+2)
    calc_no_of_sprites

    forever
	{
	if sprite_no>no_of_sprites then sprite_no=no_of_sprites
	if sprite_no<1 then sprite_no=1

	list_sprites(sprite_no-1)

	if sprite_defined(sprite_no)
	then show_message("Use Insert/Delete/Enter to maintain sprites, press (C) to Copy.")
	else show_message("Press Insert to create a Sprite")

	if use_mouse then mouse_show
	wait for keypressed
	if use_mouse then mouse_hide

	ks=keyscan:k=lcase low ks:s=high ks

	if s=1 then return 0

	if s=71 then sprite_no=1
	if s=72 then sprite_no--
	if s=73 then sprite_no-=16
	if s=79 then sprite_no=max_sprites
	if s=80 then sprite_no++
	if s=81 then sprite_no+=16

	if (s=82) or (s=30) then return 'a'
	if (s=83) or (s=32) then return 'd'
	if (s=28) or (s=18) then return 'e'
	if s=46 then return 'c'

	if (k>='0') and (k<='z') then       ;jump to sprite name
	    {
	    repeat max_sprites
		{
		k=ucase k
		sprite_no++:if sprite_no>max_sprites then sprite_no=1
		m=(sprite_no-1)*max_size
		if k=sprite_seg[m]b then goto found_key
		}
	    found_key:
	    }
	}
    }

;==============================================================================

function reverse_screen(rvy,rvx,rvw,rvc)
    {
    for y=rvy*8-3 to rvy*8+9
    for x=rvx*8-3 to (rvx+rvw)*8+2
	c=vpoint(x,y)
	if c=7 then vplot(x,y,rvc)
	else vplot(x,y,14)
    next x
    next y
    }

;==============================================================================

function button_bar(rvx,rvy,rvw)
    {
    if (mouse_button and mouse_left_button)=0 then return 0

    rvx=rvx*8-3
    rvy=rvy*8-3

    if mouse_x<rvx then return 0
    if mouse_x>(rvx+rvw*8+5) then return 0
    if mouse_y<rvy then return 0
    if mouse_y>(rvy+12) then return 0

    mouse_key=1
    return 1
    }

;==============================================================================

function setup_edit_screen
    {
    vscreen	;320*200, 256 colours

    cursor save_y,save_x:print bios "SAVE";
    reverse_screen(save_y,save_x,4,4)

    cursor demo_y,demo_x:print bios "DEMO";
    reverse_screen(demo_y,demo_x,4,0)

    cursor cancel_y,cancel_x:print bios "CANCEL";
    reverse_screen(cancel_y,cancel_x,6,1)

    last_current_colour=-1
    }

;==============================================================================

function load_sprite(sprite_no)
    {
    if sprite_no=-1 then sprite_no=no_of_sprites+1

    old_sprite=sprite_defined(sprite_no)
    sprite_base=(sprite_no-1)*max_size
    sprite_addr=peek sprite_seg|(sprite_base+16)

    #long
    if old_sprite then
	{
	;get old dimensions
	seek #1,sprite_addr
	r=read #1,2 to work_area
	sprite_width=peekb work_area
	sprite_depth=peekb (work_area+1)

	if sprite_width>64 then abort_error("Data file is Corrupt!")
	if sprite_depth>40 then abort_error("Data file is Corrupt!")

	r=read #1,sprite_width*sprite_depth to work_area+2
	}
    else
	{
	sprite_width=32
	sprite_depth=20
	pokeb work_area,sprite_width
	pokeb work_area+1,sprite_depth
	fillb 64*40 from work_area+2 with 0	;black sprite
	}
    #short
    }

;==============================================================================

proc draw_sprite_border
    {
    leftx=2:rightx=5+sprite_width*4
    topy=2:bottomy=5+sprite_depth*4

    ;large border
    for x=leftx to rightx
	vplot(x,topy,15)
	vplot(x,bottomy,15)
    next x

    for y=topy to bottomy
	vplot(leftx,y,15)
	vplot(rightx,y,15)
    next y

    ;small border
    for x=sprite_x-2 to sprite_x+sprite_width+1
	vplot(x,sprite_y-2,4)
	vplot(x,sprite_y+sprite_depth+1,4)
    next x

    for y=sprite_y-2 to sprite_y+sprite_depth+1
	vplot(sprite_x-2,y,4)
	vplot(sprite_x+sprite_width+1,y,4)
    next y

    if not old_sprite then
	{
	vsprite(290,0,sprite_arrowx)
	vsprite(0,175,sprite_arrowy)
	}
    }

;==============================================================================

proc draw_sprite_colours
    {
    colourx=rightx+10:if colourx>256 then colourx=256
    coloury=topy+20

    ;show colour grid
    for y=0 to 15
    for x=0 to 15
	xp=colourx+x*4:yp=coloury+y*4
	cp=y*16+x

	for sx=xp to xp+3
	for sy=yp to yp+3
	vplot(sx,sy,cp)
	next sy
	next sx
    next x
    next y
    }

;==============================================================================

function vblob(vbx,vby,c)
    {
    vplot(sprite_x+vbx,sprite_y+vby,c)
    vbx=vbx*4+4
    vby=vby*4+4

    repeat 4
	{
	vplot(vbx,vby,c)
	vplot(vbx+1,vby,c)
	vplot(vbx+2,vby,c)
	vplot(vbx+3,vby,c)
	vby++
	}
    }

;==============================================================================

function get_blob(gx,gy)
    {
    m=work_area+2+gy*sprite_width+gx
    return peekb m
    }

function set_blob(gx,gy,gc)
    {
    m=work_area+2+gy*sprite_width+gx
    pokeb m,gc
    vblob(gx,gy,gc)
    }

;==============================================================================

proc draw_sprite_image
    {
    m=work_area+2

    for y=1 to sprite_depth
    for x=1 to sprite_width
	vblob(x-1,y-1,peekb m)
	m++
    next x
    next y
    }

;==============================================================================

proc draw_sprite_real
    {
    vsprite(sprite_x,sprite_y,work_area)
    }

;==============================================================================

proc draw_current_colour
    {
    if current_colour=last_current_colour then return
    last_current_colour=current_colour

    for y=0 to 15
    for x=rightx+10 to rightx+30
	vplot(x,y,current_colour)
    next x
    next y
    }

;==============================================================================

proc xor_sprite(vga_x,vga_y,vga_s)
    {
    vga_m=vga_y*320+vga_x
    vga_sw=peekb vga_s
    vga_sr=peekb (vga_s+1)
    vga_s+=2

    if (vga_x+vga_sw)>320 then vga_sm=320-vga_x
    else vga_sm=vga_sw
    if (vga_sm<1) or (vga_sm>64) then beep:return

    if (vga_y+vga_sr)>=200 then vga_sr=200-vga_y
    if (vga_sr<1) or (vga_sr>200) then beep:return

    repeat vga_sr
	{
	vga_m1=vga_m
	vga_s1=vga_s

	repeat vga_sm
	    {
	    vga_buffer[vga_m1]b=vga_buffer[vga_m1]b xor peekb vga_s1
	    vga_m1++
	    vga_s1++
	    }

	vga_s+=vga_sw
	vga_m+=320
	}
    }

;==============================================================================

function flip_horizontal
    {
    for fy=0 to sprite_depth-1

	fm=waste
	for fx=0 to sprite_width-1
	    pokeb fm,get_blob(fx,fy)
	    fm++
	next fx

	for fx=0 to sprite_width-1
	    fm--
	    set_blob(fx,fy,peekb fm)
	next fx

    next fy
    }

;==============================================================================

function flip_vertical
    {
    for fx=0 to sprite_width-1

	fm=waste
	for fy=0 to sprite_depth-1
	    pokeb fm,get_blob(fx,fy)
	    fm++
	next fy

	for fy=0 to sprite_depth-1
	    fm--
	    set_blob(fx,fy,peekb fm)
	next fy

    next fx
    }

;==============================================================================

proc demo_sprite
    {
    xor_sprite(sprite_x,sprite_y,work_area)

    forever
	{
	mouse_position
	mouse_x/=2

	xxx=mouse_x
	yyy=mouse_y:if yyy>180 then yyy=180

	vsprite(xxx,yyy,work_area)
	s=scan
;	halt
;	xor_sprite(xxx,yyy,work_area)

	if s=1 then goto exit_demo
	if mouse_button and mouse_right_button then goto exit_demo
	}

    exit_demo:
    xor_sprite(sprite_x,sprite_y,work_area)
    }

;==============================================================================

proc save_sprite
    {
    ;new sprite? setup disk address and table
    if not old_sprite then
	{
	no_of_sprites++     ;sprite_no = last sprite no.

	seek #1,eof:rax=reg ax:rdx=reg dx
	sprite_addr=(rdx*65536)+rax

	spritem=(sprite_no-1)*max_size
	sprite_seg[spritem+16]=low sprite_addr
	sprite_seg[spritem+18]=high sprite_addr
	}
    spritem=(sprite_no-1)*max_size
    ;old sprite - update disk address
    sprite_addr=peek sprite_seg|(spritem+16)

    l=16:m=sprite_name:d=spritem
    while l>0
	{
	if peekb m<>ext_inpend then c=peekb m else c=' '
	sprite_seg[d]b=c
	l--:m++
	d++
	}

    seek #1,sprite_addr
    write #1,2+sprite_width*sprite_depth from work_area

    sprite_changes=1
    }

;==============================================================================

function edit_sprite(sprite_no)
    {
    load_sprite(sprite_no)

    open window window_name
    if old_sprite then
	{
	namem=(sprite_no-1)*max_size
	moveb 16 from sprite_seg|namem to sprite_name
	}
    else fillb 16 from sprite_name with ' '

    locate 14,44:colour 32
    l=ext_string(sprite_name,17)
    close window
    if l<=1 then return

    setup_edit_screen
    draw_sprite_border
    draw_sprite_image
    draw_sprite_colours

    edit_x=0:edit_y=0

    current_colour=15
    insert_mode=1

    save_key=0
    demo_key=0
    cancel_key=0
    mouse_key=0

    last_width=sprite_width
    last_depth=sprite_depth

    if use_mouse then
	{
	mouse_init
	mouse_shape(mouse_cursor_arrow)
	mouse_handler(sprite_mouse,101011b)  ;buttons and change pos
	}

    forever
	{
	if (last_width<>sprite_width) or (last_depth<>sprite_depth) then
	    {
	    setup_edit_screen
	    draw_sprite_border
	    draw_sprite_image
	    draw_sprite_colours

	    last_width=sprite_width
	    last_depth=sprite_depth
	    pokeb work_area,sprite_width
	    pokeb work_area+1,sprite_depth
	    }

	draw_current_colour
	if use_mouse then mouse_show
	else
	    {
	    vsprite(edit_x*4+4,edit_y*4+4,sprite_cursor)
	    delay(2)
	    vblob(edit_x,edit_y,get_blob(edit_x,edit_y))
	    }
	delay(6)

	ks=keyscan
	s=high ks:k=lcase low ks

	if use_mouse then mouse_hide

	if (k>='0') and (k<='9') then
	    {
	    if insert_mode then c=current_colour else c=0
	    set_blob(edit_x,edit_y,c)
	    }

	if k='h' then flip_horizontal
	if k='v' then flip_vertical

	if s=71 then edit_x--:edit_y--
	if s=72 then edit_y--
	if s=73 then edit_x++:edit_y--
	if s=75 then edit_x--
	if s=77 then edit_x++
	if s=79 then edit_x--:edit_y++
	if s=80 then edit_y++
	if s=81 then edit_x++:edit_y++

	if s=119 then edit_x=0:edit_y=0
	if s=132 then edit_x=999:edit_y=0
	if s=115 then edit_x=0
	if s=116 then edit_x=999
	if s=117 then edit_x=0:edit_y=999
	if s=118 then edit_x=999:edit_y=999

	if s=82 then insert_mode=1	;insert
	if s=83 then insert_mode=0	;delete

	if edit_x<0 then edit_x=0
	if edit_y<0 then edit_y=0
	if edit_x>=sprite_width then edit_x=sprite_width-1
	if edit_y>=sprite_depth then edit_y=sprite_depth-1

	if (s=31) or (save_key=1) then
	    {
	    save_sprite
	    return
	    }

	if demo_key then
	    {
	    demo_sprite
	    demo_key=0
	    mouse_key=0
	    }

	if (s=1) or (cancel_key=1) then
	    {
	    if use_mouse then
		{
		mouse_handler(sprite_mouse,0)	;disable mouse events
		}
	    return
	    }
	}
    }

;==============================================================================

function delete_sprite(sprite_no)
    {
    load_sprite(sprite_no)

    vscreen

    repeat 100		;"wallpaper"
	{
	x=rnd mod 320
	y=rnd mod 200
	vsprite(x,y,work_area)
	}

    draw_sprite_border
    draw_sprite_image

    cursor save_y,save_x-1:print bios "DELETE";
    reverse_screen(save_y,save_x-1,6,4)

    cursor cancel_y,cancel_x:print bios "CANCEL";
    reverse_screen(cancel_y,cancel_x,6,1)

    wait for keypressed
    if (lcase key)='d' then
	{
	namem=(sprite_no-1)*max_size
	sprite_seg[namem]b=255
	no_of_sprites--
	sprite_changes=1
	}
    }

;==============================================================================

function copy_sprite(sprite_no)
    {
    load_sprite(sprite_no)
    namem=(sprite_no-1)*max_size
    moveb 16 from sprite_seg|namem to sprite_name

    sprite_no=no_of_sprites+1
    old_sprite=0
    save_sprite
    }

;== start =====================================================================

print bios "Sprite Editor v1.00 - (c) Peter Campbell Software 1994."
initialise

sprite_no=1	;last sprite

forever
    {
    setup_screen
    action=select_sprite(sprite_no)	;sprite_no = selected sprite

    if action=0 then shutdown

    if action='a' then edit_sprite(-1)
    if action='e' then edit_sprite(sprite_no)
    if action='d' then delete_sprite(sprite_no)
    if action='c' then copy_sprite(sprite_no)

    close windows
    }

;== mouse handler =============================================================

sprite_mouse:
inline 9ch	;pushf
pushall
reg ds=reg cs
;process event(s)

push x,y,vga_x,vga_y,vga_c,vga_m
push m,gx,gy,gc

mouse_button=reg bx
mouse_x=reg cx
mouse_y=reg dx

mouse_x/=2

;colour grid?
event_x=(mouse_x-colourx)/4
event_y=(mouse_y-coloury)/4

if (event_x>=0) and (event_x<=15) and (event_y>=0) and (event_y<=15) then
    {
    if mouse_button and 7 then	    ;any button ok
	{
	current_colour=event_y*16+event_x
	draw_current_colour
	}
    }

;draw dots?
event_x=(mouse_x-4)/4
event_y=(mouse_y-4)/4
#long
if (event_x>=0) and (event_x<sprite_width) and (event_y>=0) and (event_y<sprite_depth) then
    {
    if mouse_button and 7 then
	{
	mouse_hide
	if mouse_button and mouse_left_button then
	    {
	    set_blob(event_x,event_y,current_colour)
	    }
	else if mouse_button and mouse_right_button then
	    {
	    set_blob(event_x,event_y,0)
	    }
	else
	    {
	    current_colour=get_blob(event_x,event_y)
	    draw_current_colour
	    }
	mouse_show
	}
    }
#short

;save, cancel?
if button_bar(save_x,save_y,4) then save_key=1
if button_bar(demo_x,demo_y,4) then demo_key=1
if button_bar(cancel_x,cancel_y,6) then cancel_key=1

;smaller/larger sprite?
#long
if not old_sprite then
if mouse_button and mouse_left_button then
    {
    if mouse_y<=7 then
	{
	rx=290
	if (mouse_x>=rx) and (mouse_x<=(rx+9)) then sprite_width-=4
	if (mouse_x>=(rx+13)) and (mouse_x<=(rx+22)) then sprite_width+=4
	if sprite_width<4 then sprite_width=4
	if sprite_width>64 then sprite_width=64
	}
    if mouse_x<=9 then
	{
	ry=175
	if (mouse_y>=ry) and (mouse_y<=(ry+7)) then sprite_depth-=4
	if (mouse_y>=(ry+10)) and (mouse_y<=(ry+17)) then sprite_depth+=4
	if sprite_depth<4 then sprite_depth=4
	if sprite_depth>40 then sprite_depth=40
	}
    }
#short


pop gc,gy,gx,m
pop vga_m,vga_c,vga_y,vga_x,y,x

;return to mouse driver
popall
inline 9dh	;popf
retf

;== data/messages =============================================================

sprite_file: fname 'sprite.lib'
sprite_name: string 20

window_title:
datab 0,0,2,1,77,3,14
datab 22,10,1,'Sprite Editor v1.00 - (c) Peter Campbell Software 1994.'
datab 26

window_select:
datab 0,0,5,5,26,22,31
datab 26

window_name:
datab 0,0,35,12,62,16,32
datab 22,2,2,'Name : '
data 26

sprite_cursor:
datab 4,4
datab 15,15,15,15
datab 15,23,23,15
datab 15,23,23,15
datab 15,15,15,15

mouse_cursor_arrow:
data 1111111111111111b
data 1000011111111111b
data 1000001111111111b
data 1000000111111111b
data 1000000011111111b
data 1100000001111111b
data 1110000000111111b
data 1111000000011111b
data 1111100000001111b
data 1111110000000111b
data 1111111000000011b
data 1111111100000001b
data 1111111110000000b
data 1111111111000001b
data 1111111111100011b
data 1111111111110111b

data 0000000000000000b
data 0000000000000000b
data 0011100000000000b
data 0011110000000000b
data 0011111000000000b
data 0001111100000000b
data 0000111110000000b
data 0000011101000000b
data 0000001010100000b
data 0000000101010000b
data 0000000010101000b
data 0000000001010100b
data 0000000000101110b
data 0000000000011100b
data 0000000000001000b
data 0000000000000000b

sprite_arrowx:
datab 23,8
datab 40,40,40,40,40,40,40,40,40,40,0,0,0,40,40,40,40,40,40,40,40,40,40
datab 40,40,40,44,40,40,40,40,40,40,0,0,0,40,40,40,40,40,40,44,40,40,40
datab 40,40,44,44,40,40,40,40,40,40,0,0,0,40,40,40,40,40,40,44,44,40,40
datab 40,44,44,44,44,44,44,44,44,40,0,0,0,40,44,44,44,44,44,44,44,44,40
datab 40,44,44,44,44,44,44,44,44,40,0,0,0,40,44,44,44,44,44,44,44,44,40
datab 40,40,44,44,40,40,40,40,40,40,0,0,0,40,40,40,40,40,40,44,44,40,40
datab 40,40,40,44,40,40,40,40,40,40,0,0,0,40,40,40,40,40,40,44,40,40,40
datab 40,40,40,40,40,40,40,40,40,40,0,0,0,40,40,40,40,40,40,40,40,40,40

sprite_arrowy:
datab 10,18
datab 40,40,40,40,40,40,40,40,40,40
datab 40,40,40,40,44,44,40,40,40,40
datab 40,40,40,44,44,44,44,40,40,40
datab 40,40,44,44,44,44,44,44,40,40
datab 40,40,40,40,44,44,40,40,40,40
datab 40,40,40,40,44,44,40,40,40,40
datab 40,40,40,40,44,44,40,40,40,40
datab 40,40,40,40,40,40,40,40,40,40
datab 0,0,0,0,0,0,0,0,0,0
datab 0,0,0,0,0,0,0,0,0,0
datab 40,40,40,40,40,40,40,40,40,40
datab 40,40,40,40,44,44,40,40,40,40
datab 40,40,40,40,44,44,40,40,40,40
datab 40,40,40,40,44,44,40,40,40,40
datab 40,40,44,44,44,44,44,44,40,40
datab 40,40,40,44,44,44,44,40,40,40
datab 40,40,40,40,44,44,40,40,40,40
datab 40,40,40,40,40,40,40,40,40,40
