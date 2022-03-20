
#include vga.fi
#include sincos.fi
#short

var x,y,c,speed
var xfactor,yfactor
var32 ss32,cc32,xx32

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

    next degrees
    }

;==============================================================================

function rnd_process(r,min,max)
    {
    rp=rnd

    if rp>25000 then r--
    if rp>32000 then r-=2
    if rp<-25000 then r++
    if rp<-32000 then r+=2

    if r<min then r=min
    if r>max then r=max

    return r
    }

;==============================================================================

vscreen
randomize timer

x=160:y=100
xfactor=100
yfactor=70
size=50
count=0

while key=0
    {
    vcircle(x,y,size)

    if count then count--
    else
	{
	c++:if c>127 then c=32
	count=30
	}

    x=rnd_process(x,20,279)
    y=rnd_process(y,20,159)
    xfactor=rnd_process(xfactor,10,200)
    yfactor=rnd_process(yfactor,10,200)
    size=rnd_process(size,20,200)
    }

screen 3
stop
