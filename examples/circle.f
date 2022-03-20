
#include vga.fi
#include sincos.fi
#short

var x,y,c,speed
var xfactor,yfactor
var32 ss32,cc32,xx32

const max_circles=4
const max_size=8*2
max_data ? max_circles*max_size

;==============================================================================

proc vcircle(vx,vy,vr)
    {
    if vr<10 then vrs=8
    else if vr<50 then vrs=2
    else vrs=1

    for degrees=0 to 1023 step vrs

    ss=sin(degrees)
    cc=cos(degrees)

    ss32=imul(ss,vr*2)
    cc32=imul(cc,vr*2)

    sx=high ss32
    cx=high cc32

    sx=imul(sx,xfactor)
    sx=idiv(sx,100)

    cx=imul(cx,yfactor)
    cx=idiv(cx,100)

    sx+=vx
    cx+=vy

    if (sx>=0) and (cx>=0) and (sx<=319) and (cx<=199) then
	{
	vplot (sx,cx,c)
	}

    if (degrees mod speed)=0 then c++:if c>127 then c=32

    next degrees
    }

;==============================================================================

vscreen
randomize timer
fillb max_circles*max_size from max_data with 0

forever
    {
    ks=keyscan
    s=high ks
    k=low ks

    if k=27 then goto exit_circles

    cm=max_data
    for count=1 to max_circles
	start=peek cm
	finish=peek (cm+2)
	x=peek (cm+4)
	y=peek (cm+6)
	c=peek (cm+8)
	speed=peek (cm+10)
	xfactor=peek (cm+12)
	yfactor=peek (cm+14)

	if start=finish then
	    {
	    #if 0
	    start=1
	    finish=10+(rnd mod 150)
	    x=10+(rnd mod 300)
	    y=10+(rnd mod 180)
	    speed=1+(rnd and 63)
	    xfactor=20+(rnd mod 160)
	    yfactor=20+(rnd mod 160)
	    c=32+(rnd mod 96)
	    #else
	    start=1+(rnd and 15)
	    finish=100+(rnd mod 100)
	    x=160
	    y=100
	    speed=1+(rnd and 63)
	    xfactor=100
	    yfactor=70
	    c=32+(rnd mod 96)
	    #endif
	    }
	else
	    {
	    if start<finish then start++ else start--
	    }

	push c
	vcircle(x,y,start)
	pop c

	if (start mod count)=0 then c++:if c>127 then c=32

	poke cm,start
	poke cm+2,finish
	poke cm+4,x
	poke cm+6,y
	poke cm+8,c
	poke cm+10,speed
	poke cm+12,xfactor
	poke cm+14,yfactor

	cm+=max_size
    next count
    }

exit_circles:

screen 3
stop
