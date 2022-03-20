;===========================================================================
;Print screen utility.	 ** This now combines SDUMP and DUMP3.
;===========================================================================

#short

x1=0:y1=0
x2=79:y2=24
dos 35(5):olds=reg es:oldo=reg bx
setint 5 to printscr
st=allocate 4096/16
print bios "PSCR installed."
stop resident

proc sdump
    {
    mode 640,and
    lprint chr 27;chr '3';chr 24;
    for y=0 to 199 step 8
    lprint chr 27;chr '*';chr 5;chr 128;chr 2;
    for x=0 to 639
    bit=0
    for a=0 to 7:bit*=2+(point x,y+a):next a
    lprint chr(bit);
    next x
    lprint
    next y
    repeat 4 lprint
    }

proc dump3
    {
    mode 320,and
    lprint chr 27;chr '3';chr 22;
    for y=0 to 199 step 4
    lprint chr 27;chr '*';chr 5;chr 128;chr 2;
    for x=0 to 319
    bit=0:bit2=0
    for a=0 to 3
    bit*=4:bit2*=4
    pp=point x,(y+a)
    if pp=1 then bit+=1
    if pp=2 then bit+=1:bit2+=2
    if pp=3 then bit+=3:bit2+=3
    next a
    lprint chr bit;chr bit2;
    next x:lprint
    next y
    repeat 4 lprint
    }

proc clearit move 2000 from st|0 to video|0

proc corners
    {
    tx=x1:ty=y1
    bx=x2:by=y2
    if bx<tx then swap bx,tx
    if by<ty then swap by,ty
    }

proc displayit
    {
    corners
    offset=tx*2+ty*160+1
    repeat by-ty+1
	{
	push offset
	repeat bx-tx+1
	    {
	    video[offset]=video[offset] xor 127
	    offset+=2
	    }
	pop offset:offset+=160
	}
    }

proc print_screen
    {
    corners
    for y=ty to by
    for x=tx to bx
    char=low (scrchr y,x)
    reg dx=0,ax=char:int 17h
    next x
    reg dx=0,ax=10:int 17h
    reg dx=0,ax=13:int 17h
    next y
    }


printscr:
pushall
reg ds=reg cs
if peekb 0|500h then goto exitscr
video=0b800h-(800h*mono)+page*100h
pokeb 0|500h,1

m=screen
#long
if m=4 then dump3
else if m=6 then sdump
else if m=3 then
    {
#short
    move 2000 from video|0 to st|0

    pskey:
    displayit
    wait for keypressed
    ks=keyscan:k=low ks
    if k=27 then goto anp
    if k=13 then print_screen:goto anp
    clearit

    if (ks=19200) and (x2>0) then x2--
    if (ks=19712) and (x2<79) then x2++
    if (ks=18432) and (y2>0) then y2--
    if (ks=20480) and (y2<24) then y2++

    if (k='4') and (x1>0) then x1--
    if (k='6') and (x1<79) then x1++
    if (k='8') and (y1>0) then y1--
    if (k='2') and (y1<24) then y1++
    goto pskey

    anp:
    move 2000 from st|0 to video|0
    }
else
    {
    push psret,olds,oldo
    retf
    psret:
    }

pokeb 0|500h,0

exitscr:
popall
iret
