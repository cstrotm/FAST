
;test program to print code address on debug (INT 03)

setint 3 to fast3
stop resident

fast3:
pushall

reg ds=reg cs
ip=peek reg ss|(reg sp+20)
cs=peek reg ss|(reg sp+22)

locate 0,71:printh cs;":";ip;

exit3:
popall
iret


