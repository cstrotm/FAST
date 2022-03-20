
#include vga.fi
#include sincos.fi
#short

var x,y,c,speed,degrees
var32 ss32,cc32,xx32

var xx,xi,xtime

const max_items=40	    ;max 2500
const data_size=7*2
max_data ? max_items*data_size

;==============================================================================

proc vspot(degrees,size,vitem)
    {
    ss=sin(degrees)
    cc=cos(degrees)

    ss32=imul(ss,size)
    cc32=imul(cc,size)

    sx=imul(sx,130)
    sx=idiv(sx,100)

    sx=high ss32
    cx=high cc32

    sx+=xx
    cx+=100

    if (sx>=0) and (cx>=0) and (sx<=310) and (cx<=190) then vsprite (sx,cx,vitem)
    }

;==============================================================================

vscreen
randomize timer
fillb max_items*data_size from max_data with 0

xx=40
xi=1
xtime=50

forever
    {
    ks=keyscan
    s=high ks
    k=low ks

    if k=27 then goto exit_circles

    cm=max_data
    for count=1 to max_items
	degrees=peek cm
	speed=peek (cm+2)
	size=peek (cm+4)
	sizei=peek (cm+6)
	vimage=peek (cm+8)
	sx=peek (cm+10)
	cx=peek (cm+12)

	if size=0 then
	    {
	    degrees=0
	    speed=2+(rnd mod 15)
	    size=rnd and 15
	    sizei=(22-speed)+(rnd mod 10)
	    vimage=peek (images+2*(rnd and 3))
	    }
	else
	    {
	    if (sx>=0) and (cx>=0) and (sx<=310) and (cx<=190) then vsprite(sx,cx,clear)

	    degrees+=speed
	    sizei--
	    if sizei=0 then
		{
		size++
		sizei=20-speed
		}
	    }

	if size>200 then size=0
	else vspot(degrees,size,vimage)

	poke cm,degrees
	poke cm+2,speed
	poke cm+4,size
	poke cm+6,sizei
	poke cm+8,vimage
	poke cm+10,sx
	poke cm+12,cx

	cm+=data_size
    next count

    xtime--
    if xtime=0 then
	{
	xx+=xi
	if xx<40 then xx=40:xi=1
	else if xx>270 then xx=270:xi=-1
	else
	    {
;	    if xi>0 then xi++
;	    else if xi<1 then xi--
	    }
	xtime=10
	}

    repeat 10000 {}
    }

exit_circles:

screen 3
stop

;==============================================================================

images: data item1,item2,item3,item4

item1:
datab 4,3
datab 40,40,40,40,40,40
datab 40,20,20,20,20,40
datab 40,20,23,23,20,40
datab 40,20,20,20,20,40
datab 40,40,40,40,40,40

item2:
datab 10,8
datab 0,0,0,0,0,0,0,0,0,0
datab 0,0,0,0,0,0,36,0,0,0
datab 0,0,0,36,32,0,46,0,0,0
datab 0,0,37,40,42,41,0,0,0,0
datab 0,0,0,40,40,42,0,46,0,0
datab 0,33,34,35,33,34,39,40,0,0
datab 0,0,0,32,0,36,0,37,0,0
datab 0,0,0,0,0,0,0,0,0,0

item3:
datab 10,8
datab 0,0,0,24,24,0,0,24,0,0
datab 0,0,0,0,23,0,0,0,0,0
datab 0,0,0,0,27,0,28,0,0,0
datab 0,0,0,0,31,30,26,28,0,0
datab 0,26,25,28,31,30,28,0,25,0
datab 0,0,28,29,31,29,0,26,0,0
datab 24,0,0,0,30,0,27,26,24,0
datab 0,0,0,0,24,0,0,24,0,0

item4:
datab 10,8
datab 0,82,0,82,0,82,0,82,0,0
datab 0,82,0,82,0,82,0,82,0,0
datab 0,82,0,82,0,82,0,82,0,0
datab 0,82,0,82,0,82,0,82,0,0
datab 0,82,0,82,0,82,0,82,0,0
datab 0,82,0,82,0,82,0,82,0,0
datab 0,82,0,82,0,82,0,82,0,0
datab 0,82,0,82,0,82,0,82,0,0

clear:
datab 10,8
datab 0,0,0,0,0,0,0,0,0,0
datab 0,0,0,0,0,0,0,0,0,0
datab 0,0,0,0,0,0,0,0,0,0
datab 0,0,0,0,0,0,0,0,0,0
datab 0,0,0,0,0,0,0,0,0,0
datab 0,0,0,0,0,0,0,0,0,0
datab 0,0,0,0,0,0,0,0,0,0
datab 0,0,0,0,0,0,0,0,0,0
