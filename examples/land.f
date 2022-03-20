
;==============================================================================
; landscape game
;==============================================================================

#short

;== variables =================================================================

const map_len=10000		;<32K
const key_repeat=2

const objects=400		;see object_seg (max=5000)
const object_size=11		;type, x,y,xi,yi,counter

const type_dot=1
const type_explosion=2
const type_badbullet=3
const type_bullet=4

var nc,oldx,lastx,posxi,posx,posy
var scrx
var dotxx
var objectm

var sprite_ship
var explode_x,explode_y

#include vga.fi
#include keys.fi
#include sprite.fi

;== procs =====================================================================

proc abort_error(m)
    {
    reset_keys
    screen 3
    print bios
    print bios "Exit ";chr 34;
    while peekb m print bios chr peekb m;:m++
    print bios chr 34;
    print bios
    stop
    }

;==============================================================================

proc initialise
    {
    land_seg=allocate map_len/16+1
    object_seg=allocate (objects*object_size)/16+1

    fillb objects*object_size from object_seg|0 with 0

    x=0
    y=160

    nt=400
    ny=160
    ng=0

    while x<map_len
	{
	land_seg[x]b=y

	if nt then
	    {
	    grade=0:if (nc and 3)<ng then grade=1

	    if y<ny then y+=grade
	    else y-=grade
	    nt--
	    }
	else
	    {
	    ny=190-(rnd mod 150)
	    nt=10+(rnd mod 100)
	    ng=rnd mod 5
	    }

	if x>(map_len-150) then
	    {
	    ny=160
	    nt=200
	    ng=3
	    }

	x++
	nc++	    ;counter
	}

    vscreen
    set_keys

    if open_sprite_library then abort_error("Error opening Sprite Library")
    if load_sprite("SHIP RIGHT",sprite_shipr) then abort_error("Missing Ship (R)")
    if load_sprite("SHIP LEFT",sprite_shipl) then abort_error("Missing Ship (L)")

    load_sprite("THRUST1L",sprite_thrust)
    load_sprite("THRUST2L",sprite_thrust+98)
    load_sprite("THRUST3L",sprite_thrust+196)
    load_sprite("THRUST1R",sprite_thrust+294)
    load_sprite("THRUST2R",sprite_thrust+392)
    load_sprite("THRUST3R",sprite_thrust+490)

    load_sprite("BULLET",sprite_bullet)

    for object=objects-50 to objects-1
	m=object*object_size
	object_seg[m]b=type_dot
	object_seg[m+1]=rnd mod 320
	object_seg[m+3]=rnd mod 200
	object_seg[m+5]b=16+(rnd and 15)
    next object

    fire_bullet=0
    fire_time=0
    }

;==============================================================================

proc cls_sprite(vga_x,vga_y,vga_s)
    {
    vga_m=vga_y*320+vga_x
    vga_sw=peekb vga_s
    vga_sr=peekb (vga_s+1)

    if (vga_x+vga_sw)>320 then vga_sm=320-vga_x
    else vga_sm=vga_sw
    if (vga_sm<1) or (vga_sm>64) then beep:return

    if (vga_y+vga_sr)>=200 then vga_sr=200-vga_y
    if (vga_sr<1) or (vga_sr>200) then beep:return

    repeat vga_sr
	{
	fillb vga_sm from vga_buffer|vga_m with 0
	vga_m+=320
	}
    }

;==============================================================================

proc display_land(x,c)
    {
    oldx=lastx

    x-=150
    push x

    for dx=0 to 319

	if x<0 then x+=map_len
	if x>=map_len then x-=map_len

	if lastx<0 then lastx+=map_len
	if lastx>=map_len then lastx-=map_len

	lasty=land_seg[lastx]b
	vplot(dx,lasty	,0)
	vplot(dx,lasty+1,0)

	y=land_seg[x]b
	vplot(dx,y  ,15)
	vplot(dx,y+1,14)

	lastx++:x++

    next dx

    pop x
    lastx=x
    }

;==============================================================================

proc create_explosion
    {
    x=explode_x
    y=explode_y

    xi=rnd and 255
    yi=250+(rnd mod 400)
    explode_count--

    object_seg[objectm]b=type_explosion

    object_seg[objectm+5]=xi
    object_seg[objectm+7]=yi

    object_seg[objectm+9]b=32+(rnd and 15)

    type=type_explosion
    }

;==============================================================================

proc create_bullet
    {
    if sprite_ship=sprite_shipl then
	{
	x=lastx+scrx+7
	xi=-6
	}
    else
	{
	x=lastx+scrx+18
	xi=6
	}
    y=posy+4

    object_seg[objectm]b=type_bullet
    object_seg[objectm+5]=xi
    object_seg[objectm+9]=100	    ;bullet timer

    type=type_bullet
    }

;==============================================================================

function move_dot
    {
    vplot(x,y,0)
    x+=dx

    if x<0 then x=319
    if x>319 then x=0

    vplot(x,y,object_seg[objectm+5]b)
    return 1
    }

;==============================================================================

function move_explosion
    {
    ex=x-oldx
    if ex<0 then ex+=map_len
    if ex<320 then vplot(ex,y/256,0)

    xi=object_seg[objectm+5]
    yi=object_seg[objectm+7]

    hx=high xi
    lx=low xi

    if lx>127 then hx-=(256-lx)*3
    else hx+=lx*3

    if hx<0 then
	{
	x--:hx+=256
	if x<0 then x=map_len-1
	}
    if hx>255 then
	{
	x++:hx-=256
	if x>=map_len then x=0
	}

    object_seg[objectm+5]=hx*256+lx

    y-=yi
    yi-=5	;gravity

    if y above 51199 then return 0

    object_seg[objectm+7]=yi

    ex=x-lastx
    if ex<0 then ex+=map_len
    if ex<320 then vplot(ex,y/256,object_seg[objectm+9]b)

    return 1
    }

;==============================================================================

function move_badbullet
    {
    }

;==============================================================================

function move_bullet
    {
    ex=x-oldx
    if ex<0 then ex+=map_len
    if ex<320 then cls_sprite(ex,y,sprite_bullet)

    xi=object_seg[objectm+5]
    x+=xi

    if x<0 then x=map_len-1
    if x>=map_len then x=0

    count=object_seg[objectm+9]
    if count then object_seg[objectm+9]--
    else return 0

    ex=x-lastx
    if ex<0 then ex+=map_len
    if ex<320 then vsprite(ex,y,sprite_bullet)

    return 1
    }

;==============================================================================

proc draw_objects
    {
    ;used by background dots
    dx=0
    while dotxx>799 dx--:dotxx-=800
    while dotxx<-799 dx++:dotxx+=800

    ;used by explosions
    explode_count=0
    if rnd>30000 then
	{
	explode_count=60+(rnd and 127)
	explode_x=lastx+scrx+16
	explode_y=256*(posy+10)
	}

    ;bullet
    fire_count=0
    if fire_bullet then
	{
	if fire_time then fire_time--
	else fire_count=1
	}

    objectm=0

    for object=1 to objects

	type=object_seg[objectm]b
	if type then
	    {
	    x=object_seg[objectm+1]
	    y=object_seg[objectm+3]
	    }
	else
	    {
	    if fire_count then
		{
		create_bullet
		fire_bullet=0
		fire_time=8
		fire_count=0
		goto new_object
		}
	    if explode_count then create_explosion
	    else goto skip_object
	    }

	new_object:
	call peek (object_handler+(type-1)*2)
	if reg ax=0 then object_seg[objectm]b=0

	object_seg[objectm+1]=x
	object_seg[objectm+3]=y

	skip_object:
	objectm+=object_size

    next object
    }

;== start =====================================================================

print bios "Landscape"

initialise

posy=160-16
scrx=104
thrustx=0:thrustc=0

posx=200:posxi=0
switch_direction=1
posxx=0
lastx=200-150
oldx=200-150

dotxx=0

sprite_ship=sprite_shipr

forever
    {
    display_land(posx,15)
    draw_objects

    vsprite(scrx,posy,sprite_ship)

    if thrustc then thrustc--
    else
	{
	thrustx++
	if posxi<0 then i=0-posxi else i=posxi
	thrustc=16-(i/64)
	}
    sthrust=sprite_thrust+(thrustx mod 3)*98

    if sprite_ship=sprite_shipl then vsprite(scrx+35,posy+5,sthrust)
    else vsprite(scrx-15,posy+5,sthrust+294)

    if key_press(1) then abort_error("Escape")

    if key_press(51) then
	{
	if sprite_ship=sprite_shipr then
	    {
	    posxi+=16:if posxi>1024 then posxi=1024
	    }
	else
	    {
	    posxi-=16:if posxi<-1024 then posxi=-1024
	    }
	}
    if key_press(52) then
	{
	if sprite_ship=sprite_shipl then
	    {
	    posxi-=16:if posxi<-1024 then posxi=-1024
	    }
	else
	    {
	    posxi+=16:if posxi>1024 then posxi=1024
	    }
	}

    if key_press(53) or key_press(28) then
	{
	fire_bullet=1
	}

    if switch_direction then switch_direction--

    #long
    if key_press(57) then
	{
	if switch_direction=0 then
	    {
	    if sprite_ship=sprite_shipr then
		{
		cls_sprite(scrx,posy,sprite_shipr)
		cls_sprite(scrx-15,posy+5,sthrust+294)
		sprite_ship=sprite_shipl
		}
	    else
		{
		cls_sprite(scrx,posy,sprite_shipl)
		cls_sprite(scrx+35,posy+5,sthrust)
		sprite_ship=sprite_shipr
		}
	    switch_direction=20
	    }
	}
    #short

    if key_press(30) or key_press(72) then posy--:if posy<0 then posy=0
    if key_press(44) or key_press(80) then posy++:if posy>184 then posy=184

    posxx+=posxi
    dotxx+=posxi

    while posxx>255 posx++:posxx-=256
    while posxx<-255 posx--:posxx+=256

    if posx<0 then posx+=map_len
    if posx>=map_len then posx-=map_len

    if sprite_ship=sprite_shipl then
	{
	if scrx<184 then scrx++
	}
    if sprite_ship=sprite_shipr then
	{
	if scrx>104 then scrx--
	}
    }

;==data =======================================================================

sprite_shipr: space 2+20*32
sprite_shipl: space 2+20*32

sprite_thrust: space 98*6

object_handler:
data .move_dot
data .move_explosion
data .move_badbullet
data .move_bullet

sprite_bullet: space 2+8*4
