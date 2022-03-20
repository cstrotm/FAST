;===========================================================================
;Print screen utility to disk.
;===========================================================================

;temporary conversion of DSCR - create one file called "DSCR.TMP"
;all screen dumps are appended to this file

#short
#errors off

print bios "DSCR: ";

setint 5 to printscr
pokeb 0|500h,0

print bios "installed Ok."
stop resident

;===========================================================================

buffer ? 25*82+1

printscr:
pushall
reg ds=reg cs
if peekb 0|500h then goto exitscr
video=0b800h-(800h*mono)+page*100h
pokeb 0|500h,1

m=0:b=buffer
repeat 25
    {
    ob=b
    repeat 80
	{
	c=video[m]b:if (c=0) or (c=255) then c=' '
	pokeb b,c
	b++:m+=2
	}
    while (b>ob) and (peekb(b-1)=' ')  b--

    pokeb b,13:b++
    pokeb b,10:b++
    }

;== save file =================================================================

open #1,"dscr.tmp"
if error then
    {
    create #1,"dscr.tmp"
    if error then beep:goto save_error
    }

seek #1,eof
write #1,b-buffer from buffer
if error then beep
close #1
if error then beep

save_error:
pokeb 0|500h,0

exitscr:
popall
iret
