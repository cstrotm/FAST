;Sector editor.

dr=80h ;C: by default.
if (peekb 81h=' ') and (peekb 83h=':') then
    {
    dr=(ucase peekb 82h)-'A'
    if dr>1 then dr+=80h-2
    }

if dr<80h then
    {
    print bios "Tracks:  ";:tracks=input:print bios
    print bios "Heads:	 ";:heads=input:print bios
    print bios "Sectors: ";:sectors=input:print bios
    if (tracks=0) or (heads=0) or (sectors)=0 then beep:stop
    }
else
    {
    reg dx=dr,ax=800h:int 13h
    if carry then print bios "SED: Can't read disk parameters!":terminate
    cx=reg cx
    heads=1+high reg dx
    tracks=high cx+((low cx and 192)*4)
    sectors=cx and 63
    }

screen 3
buffer ? 512

s=1:t=0:h=0
text=0:opened=0

proc clearb(sx) locate 24,sx:repeat 80-sx print " ";

proc show_where
    {
    colour 120
    locate 5,0:print "Track:";t;:printb "  Head:";h;"  Sector:";s;"  ";
    m=5*160+1:repeat 80 video[m]b=120:m+=2
    colour 7
    }

function nexts
    {
    s++
    if s>sectors then s=1:h++:if h>(heads-1) then h=0:t++
    if t>=tracks then t=tracks-1:s=sectors:h=heads-1:return 0
    return 1
    }

function inp(a,b)
    {
    start:
    cursor 24,0:print bios "? ";
    i=input
    if (i<a) or (i>b) then beep:goto start
    clearb(0)
    return i
    }

proc abort cls:terminate

proc display
    {
    bm=buffer
    for dy=7 to 22
    for dx=0 to 15
    if text then
	{
	c1=peekb bm;
	c2=peekb (bm+1)
	locate dy,dx*2+4:print chr c1;chr c2;
	c1=c1 and 127
	c2=c2 and 127
	if c1<32 then c1='.'
	if c2<32 then c2='.'
	locate dy,44+dx*2:print chr c1;chr c2;
	}
    else
	{
	locate dy,dx*5
	printh peek bm;
	}
    bm+=2
    next dx,dy
    }

function reads
    {
    show_where
    read2:
    edited=0
    hl=h*256+dr
    cl=t*256+s+(t/256)*64
    reg es=reg cs,bx=buffer,dx=hl,cx=cl,ax=0201h:int 13h
    if carry then
	{
	show_where
	locate 24,0:print "Read error, skip ";:colour 15:print "S";
	colour 7:print "ector or ";:colour 15:print "H";
	colour 7:print "ead?";
	loctocur
	retryk:
	wait for keypressed
	k=ucase	key
	if (k<>'S') and	(k<>'H') and (k<>27) then goto retryk
	clearb(0)

	if k='S' then r=nexts:goto read2
	if k='H' then s=sectors:r=nexts:goto read2

	show_where
	return 0
	}

    show_where
    return 1
    }

cls
locate 0,30:print "Sector EDitor"
locate 2,0:print "Track Head Sector ESCape Ascii heX Find Open Write Close"
m=2*160
repeat 80
    {
    b=video[m]b
    if (b=(ucase b)) and (b>='A') then video[m+1]b=120
    m+=2
    }

main:
locate 4,0:if opened
    then print "Disk file open.";
    else print "               ";
locate 1,62:print "1>Tracks:  ";tracks;"  ";
locate 2,62:printb "2>Heads:   ";heads;"  ";
locate 3,62:printb "3>Sectors: ";sectors;"  ";
m=160+62*2+1
repeat 3 video[m]b=120:m+=160

if not reads then abort

disp2:
display
enter:
clearb(0)
wait for keypressed
ks=keyscan
k=ucase low ks
sc=high ks

if k=27 then abort
if k='A' then
    {
    text=1
    scroll 0,7,79,22,0
    goto disp2
    }
if k='X' then
    {
    text=0
    scroll 0,7,79,22,0
    goto disp2
    }

if (k='O') and (opened=0) then
    {
    cursor 24,0:print bios "File name: ";
    inputs file
    if peekb (file+2)<>13 then
	{
	asciiz file
	create #1,file+2:if error then goto main
	opened=1
	goto main
	}
    }
if (k='C') and (opened=1) then
    {
    close #1:if error then beep
    opened=0
    goto main
    }
if (k='W') and (opened=1) then
    {
    write #1,512 from buffer:if error then beep
    noise 500,600:noise off
    }

if k='F' then
    {
    cursor 24,0:print bios "Find: ";
    inputs fstr
    if peekb (fstr+2)=13 then goto enter
    os=s:ot=t:oh=h
    forever
	{
	anotherf:
	if not nexts then beep:t=ot:h=oh:s=os:goto main
	if not reads then goto main

	if key=27 then beep:t=ot:h=oh:s=os:goto main

	m=buffer
	f=peekb (fstr+2)
	anobyte:
	m=searchb buffer+512-m from m for f
	if not m then goto nextfind
	f=fstr+3:m++
	while peekb f<>13
	    {
	    if peekb f<>peekb m then goto nfbyte
	    m++
	    f++
	    }

	display
	y=(m-buffer)/32
	x=(m-buffer) and 31
	if text
	    then cursor y+7,x+4
	    else cursor y+7,(x/2)*5+(x and 1)*2
	colour 15:locate 24,50:print "Found: F for next match.";:colour 7
	beep:wait for keypressed
	clearb(50)
	if (ucase key)='F' then goto anotherf
	goto main

	nfbyte:
	if m<(buffer+512) then goto anobyte

	nextfind:
	}
    }

if k='T' then t=inp(0,tracks-1):goto main
if k='H' then h=inp(0,heads-1):goto main
if k='S' then s=inp(1,sectors):goto main

if k='1' then tracks=inp(0,1023):goto main
if k='2' then heads=inp(0,15):goto main
if k='3' then sectors=inp(1,63):goto main

if sc=80 then
    {
    t++:if t>=tracks then t=tracks-1:s=sectors:h=heads-1
    goto main
    }
if sc=81 then
    {
    if not nexts then beep
    goto main
    }
if sc=72 then
    {
    t--:if t<0 then t=0:s=1:h=0
    goto main
    }
if sc=73 then
    {
    s--
    if s<1 then s=sectors:h--:if h<0 then h=heads-1:t--
    if t<0 then t=0
    goto main
    }

goto enter

fstr:
string 20

file:
string 25
