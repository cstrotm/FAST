
;==============================================================================
; Extended Memory Testing
;==============================================================================

#short
#include xms.fi

buffer ? 10240

var largest

proc xms_play		;move memory
    {
    repeat 5000
	{
	xms_offset=0
	xms_get(1024,reg ds,buffer,.xms_offset)
	}
    }

;==============================================================================

print bios "XMS Testing Program (FAST)"
print bios

if not xms_init then
    {
    print bios "ERROR: XMS driver not found."
    beep:stop
    }
largest=xms_query
xms_allocate(largest)
print bios "Allocated ";largest;"K of XMS memory."

c=timer
xms_play
t=timer
print bios "time=";t-c;"/18.2 seconds."
xms_deallocate

stop

