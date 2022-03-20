;===========================================================================
;Print screen utility to disk.
;===========================================================================

#short
#inpend=0

setint 5 to printscr
print bios "DSCR installed."
pokeb 0|500h,0
stop resident

;===========================================================================

function filename_ok
    {
    if peekb (name+2)=0 then return 0
    last=searchb 50 from name+2 for 0
    len=last-(name+2)
    place=searchb len from name+2 for '.'
    if place=0 then
	{
	moveb 5 from ext_scr to last
	}
    return 1

    ext_scr: fname '.scr'
    }

;===========================================================================

buffer ? 25*82+1

printscr:
pushall
reg ds=reg cs
if peekb 0|500h then goto exitscr
video=0b800h-(800h*mono)+page*100h
pokeb 0|500h,1

m=2*160:b=buffer
repeat 23
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
pokeb b,26

oc=curpos
open window wname
cursor 20,28:inputs name
#errors off
if filename_ok then save name+2,buffer,(b+1)-buffer
#errors on
if error then beep:beep
close window
curpos=oc

pokeb 0|500h,0

exitscr:
popall
iret

wname:
datab 0,0,10,18,69,22,30
datab 22,2,2,'Filename [.scr]'
datab 26

name:
string 40
