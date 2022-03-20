
;===========================================================================
; FAST mouse example
;===========================================================================

work ? 200

#include mouse.fi

;== start ==================================================================

print bios "Fast Mouse Routines"
if mouse_init=0 then
    {
    print bios "Mouse Driver cannot be Initialised (not loaded)"
    stop
    }

mouse_show

while keyscan=0
    {
    mouse_position
    locate 0,0
    if mouse_button and 1 then print "Left "; else print "     ";
    if mouse_button and 4 then print "Middle "; else print "       ";
    if mouse_button and 2 then print "Right "; else print "      ";
    print mouse_x;",";mouse_y;"    ";
    repeat 65000 {}
    m=8*160+40
    mouse_hide
    move 40 from video|m to work
    scroll 20,8,59,17,1
    m=17*160+40
    move 40 from work to video|m
    mouse_show
    }

mouse_hide
