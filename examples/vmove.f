
#include vga.fi
#short
vscreen

const dots=250

dot_area ? dots*5

;setup dots
m=dot_area
repeat dots
    {
    poke m,160
    poke m+2,100
    pokeb m+4,32+(rnd mod 80)
    m+=5
    }

while key=0
    {
    m=dot_area
    repeat dots
	{
	x1=peek m
	y1=peek (m+2)
	c1=peekb (m+4)

	if rnd<0 then x1-- else x1++
	if rnd<0 then y1-- else y1++

	if x1<0 then x1=0
	if y1<0 then y1=0
	if x1>319 then x1=319
	if y1>199 then y1=199

	vplot (x1,y1,c1)

	poke m,x1
	poke m+2,y1
	m+=5
	}
    }

screen 3
stop
