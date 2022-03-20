
;change keyboard LED's

;status byte 0040:0017

const scrolllock = 16
const numlock	 = 32
const capslock	 = 64

;this stupid little program toggles the scroll lock when num lock is on

on int 1
    {
    status=peekb 40h|17h

    if status and numlock then
	{
	counter++
	if counter and 1 then	;9 times a second
	    {
	    status=status xor scrolllock
	    pokeb 40h|17h,status
	    }
	}
    }

stop resident
