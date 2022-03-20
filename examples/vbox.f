
#include vga.fi
#short
vscreen

x1=160:y1=100
x2=160:y2=100
x3=160:y3=100
x4=160:y4=100

randomize timer

c=32
count=0

while key=0
    {
    if rnd<0 then x1-- else x1++
    if rnd<0 then x2-- else x2++
    if rnd<0 then x3-- else x3++
    if rnd<0 then x4-- else x4++

    if rnd<0 then y1-- else y1++
    if rnd<0 then y2-- else y2++
    if rnd<0 then y3-- else y3++
    if rnd<0 then y4-- else y4++

    if x1<0 then x1=0
    if x2<0 then x2=0
    if x3<0 then x3=0
    if x4<0 then x4=0

    if y1<0 then y1=0
    if y2<0 then y2=0
    if y3<0 then y3=0
    if y4<0 then y4=0

    if x1>319 then x1=319
    if x2>319 then x2=319
    if x3>319 then x3=319
    if x4>319 then x4=319

    if y1>199 then y1=199
    if y2>199 then y2=199
    if y3>199 then y3=199
    if y4>199 then y4=199

    if count then count--
    else
	{
	colour c
	c++:if c>127 then c=32
	count=80
	}

    vga_line x1,y1 to x2,y2
    vga_line x2,y2 to x3,y3
    vga_line x3,y3 to x4,y4
    vga_line x4,y4 to x1,y1
    }

screen 3
stop
