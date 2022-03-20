
#include vga.fi
#short
vscreen

c=32
count=0

while key=0
    {
    if count then count--
    else
	{
	colour c
	c++:if c>127 then c=32
	count=5+(rnd mod 60)

	x1=rnd mod 320
	x2=rnd mod 320
	y1=rnd mod 200
	y2=rnd mod 200

	d=rnd
	halt
	}

    vga_line x1,y1 to x2,y2

    if d and 1 then x1++
    if d and 2 then x1--
    if d and 4 then x2++
    if d and 8 then x2--

    if d and 16 then y1++
    if d and 32 then y1--
    if d and 64 then y2++
    if d and 128 then y2--

    if x1<0 then x1=0
    if x2<0 then x2=0
    if y1<0 then y1=0
    if y2<0 then y2=0

    if x1>319 then x1=319
    if x2>319 then x1=319
    if y1>199 then y1=199
    if y2>199 then y2=199
    }

screen 3
stop
