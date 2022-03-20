
#include vga.fi
vscreen

c=0
while key=0
    {
    colour c
    repeat 40
	{
	x1=10+rnd mod 300
	y1=10+rnd mod 179

	x2=10+rnd mod 300
	y2=10+rnd mod 179

	vga_line x1,y1 to x2,y2
	}
    c++
    }

screen 3
stop
