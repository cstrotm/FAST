
#include vga.fi
vscreen

c=32
count=0

randomize timer

x1=rnd mod 320
y1=rnd mod 200

while key=0
    {
    if rnd and 1024 then x1--
    if rnd and 512 then x1++
    if rnd and 128 then y1--
    if rnd and 64  then y1++

    if x1<0 then x1=0
    if y1<0 then y1=0

    if count then count--
    else
	{
	colour c
	c++:if c>127 then c=32
	count=80
	}

    if x1>319 then x1=319
    if y1>199 then y1=199

    vga_line 30,20 to x1,y1
    vga_line 289,20 to x1,y1
    vga_line 30,179 to x1,y1
    vga_line 289,179 to x1,y1
    }

screen 3
stop
