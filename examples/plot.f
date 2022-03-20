
;==============================================================================
; test plotting with VGA 320*200 256 colour
;==============================================================================

reg ax=0013h	;set video mode
int 10h

vbuffer=0a000h

proc ppalette(pr,blue,green,red)
    {
    reg cx=green*256+blue
    reg dx=red*256
    reg bx=pr
    reg ax=1010h
    int 10h
    }

;for r=0 to 63
;ppalette(r,r,r,r)
;ppalette(64+r,r,r/2,r/3)
;ppalette(128+r,r/3,r,r/5)
;ppalette(192+r,r/7,r/2,r)
;next r

proc vplot(x,y,c)
    {
    m=y*320+x
    vbuffer[m]b=c
    }

proc vsprite(x,y,s)
    {
    m=y*320+x
    sw=peekb s
    sr=peekb (s+1)
    if (x+sw)>320 then sm=320-x
    else sm=sw
    s+=2
    repeat sr
	{
	m1=m
	moveb sm from s to vbuffer|m
	s+=sw
	m=m1+320
	}
    }

for col=0 to 240 step 16
cursor 0,0:print bios col

y=10:c=col
repeat 16
    {
    for y=y to y+6
    for x=0 to 319
    vplot(x,y,c)
    next x
    next y
    y+=4
    c++
    }

wait for key
next col

;for y=0 to 199
;for x=0 to 319
;vplot(x,y,x)
;next x
;next y

var32 count
count=0
forever
    {
    vsprite(rnd mod 310,rnd mod 190,sp_car)
    if key then
	{
	screen 3
	printh bios high count;low count
	stop
	}
    count++
    }

;==============================================================================

sp_car:
datab 8,10
datab 0,0,0,0,0,0,0,0
datab 0,32,33,34,35,36,37,0
datab 0,0,32,0,0,0,0,0
datab 0,0,0,32,34,0,0,0
datab 0,32,35,35,35,35,33,0
datab 0,32,35,35,35,35,33,0
datab 0,0,0,32,34,0,0,0
datab 0,0,32,0,0,0,0,0
datab 0,32,33,34,35,36,37,0
datab 0,0,0,0,0,0,0,0
