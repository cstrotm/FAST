
;==============================================================================
; screen saver which blanks screen but carries on processing until key
;==============================================================================

;most screen savers interupt the current task - this is unacceptable for
;machines which may be processing long tasks - such as database updates,
;reports or other tasks; eg: communications and file transfer.

var32 last_key_time,current_time,n32

#stack memory 256

last_key_time=peek 0040h|006ch

on int 1
    {
    current_time=timer
    n32=current_time-last_key_time

    n32/=1092		;18.2 * 60 (minutes)
    if (low n32)>=6 then
	{
	disable interupts
	dummy=in 03dah
	dummy=in 03bah
	out 03c0h,0	    ;disable attribute register
	enable interupts

	screen_blank=1
	}
    }

;==============================================================================

goto skip_interupt

keyboard_interupt:
pushall
inline 0eh  ;push cs
inline 1fh  ;pop ds

if screen_blank then
    {
    disable interupts
    dummy=in 03dah
    dummy=in 03bah
    out 03c0h,20h	;enable attribute register
    enable interupts

    screen_blank=0
    }

last_key_time=peek 0040h|006ch

popall

inline 0eah	;JMPF CS: [OLD_INT]
ooff: data 0
oseg: data 0

;==============================================================================

skip_interupt:

print bios
print bios "Fast Screen Blanker, (c) Peter Campbell Software 1994"
print bios "This screen saver blanks the screen but does not interupt running programs."
print bios
print bios "Screen will auto-blank after 6 minutes of no key activity."

dos 35(09):poke oseg,reg es:poke ooff,reg bx
reg dx=keyboard_interupt:dos 25(09)

screen_blank=0

stop resident
