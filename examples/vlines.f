
#include vga.fi
vscreen
var32 start_time

proc draw_box(zoom,boxc)
    {
    x1=160-zoom
    x2=160+zoom
    y1=100-zoom
    y2=100+zoom

    colour boxc
    vga_line x1,y1 to x1,y2
    vga_line x1,y1 to x2,y1
    vga_line x1,y2 to x2,y2
    vga_line x2,y1 to x2,y2
    }

;==============================================================================

start_time=timer

for c=0 to 90
for z=1 to 94
draw_box(z+5,c*3)
next z
next c

start_time=timer-start_time

wait for key
screen 3

printh bios "time=";high start_time;low start_time

stop
