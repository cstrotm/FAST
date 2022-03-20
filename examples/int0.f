
dos 35(0):poke pioff,reg bx:poke piseg,reg es
setint 0h to int0_handle
stop resident

;===========================================================================

int0_handle:

reg ds=reg cs
pop from_off
pop from_seg
locate 20,71:printh from_seg;":";from_off;

inline 0eah
pioff:
data 0
piseg:
data 0

