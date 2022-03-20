
#include vga.fi
#short
vscreen

const points=160
point_data ? points * 5

;==============================================================================

proc reset_points
    {
    m=point_data
    repeat points
	{
	poke m,0
	poke m+2,0
	pokeb m+4,1+(rnd and 15)
	m+=5
	}
    }

;==============================================================================

proc draw_points
    {
    counter++
    m=point_data
    col=32
    speed=1
    repeat points
	{
	x=peek m:y=peek (m+2)
	colour col
	vga_line x,y to 160,100

	#long
	if (counter mod speed)=0 then
	    {
	    #short
	    if (y=0) and (x<319) then x++
	    else if (x=319) and (y<199) then y++
	    else if (x>0) and (y=199) then x--
	    else if (x=0) and (y>1) then y--

	    poke m,x:poke m+2,y
	    }

	col++
	speed++
	m+=5
	}
    }

;==============================================================================

reset_points

while key=0
    {
    draw_points
    }

;==============================================================================

screen 3
stop
