;
;FAST SPRITE DESIGNER. FAST V1.4+
;
;
;Edits files of format "end,repeat[width,height,data]"
;
;

#short
#errors off

var sp,x,y,ea,height,width

proc psprite(spn,spaddr)
    {
    sx=spn and 7
    sy=spn/8
    sprite sx*80,sy*40+30,sp|spaddr
    }

function advance(adv)
    {
    return 2+adv+(peekb adv)*2*peekb (adv+1)
    }

proc abort
    {
    screen 3
    print bios "Save? ";
    wait_abort:
    k=lcase key
    if k='n' then terminate
    if k<>'y' then goto wait_abort
    save_again:
    save file+2,sp|0,sp[0]
    if error then
	{
	print bios "Save error, R to retry.":beep
	wait for keyscan
	if (ucase key)='R' then goto save_again
	}
    terminate
    }

proc clear_bot
    {
    colour 0:cls
    cursor 0,0:print bios "Sprite file: ";
    m=file+2:while peekb m print bios chr peek m;:m++
    print bios cr lf "Memory available = ";65535-sp[0]
    }

proc put(px,py)
    {
    mode 640,and
    colour 1
    plot 500+px,50+py
    bx=40+px*4
    by=40+py*4
    for pa=0 to 3
    for pb=0 to 3
      plot bx+pb,by+pa
    next pb,pa
    }

proc display
    {
    m=ea+2
    for y=0 to height-1
    for x=0 to width-1
      b=sp[m]:m+=2
      bit=32768
      for bx=0 to 15
	if b and bit then put(x*16+bx,y)
	bit=rrightz bit
      next bx
    next x,y
    }

proc edit(ea)
    {
    #long
    if ea>=sp[0] then
	{
	clear_bot
	tryw:
	cursor 5,0:print bios "Width: (1-4) ";
	width=inputb:if not width then return
	if width above 4 then goto tryw

	tryh:
	cursor 7,0:print bios cr lf "Height: (1-32) ";
	height=inputb:if not height then goto tryw
	if height above 32 then goto tryh

	sp[ea]b=width
	sp[ea+1]b=height
	fill width*height from sp|ea+2 with 0
	sp[0]+=2+width*height*2
	}
    else
	{
	width=sp[ea]b
	height=sp[ea+1]b
	}
    #short
    clear_bot

    mode 640,and
    display

    x=0:y=0

    eloop:
    s=scan
    if s=1 then goto xedit

    goto eloop

    xedit:
    }

sp=allocate 4096:if error then beep:terminate

print bios "Sprite file: ";
inputs file
if peekb (file+2)=13 then terminate
asciiz file
print bios

open #1,file+2
if error then create #1,file+2:if error then error 100
sp[0]=2
read_len=read #1,65535 to sp|0
close #1:if error then error 101

screen 6

start:
colour 0:cls
sbase=2

next_page:
seln=0
redisplay:
clear_bot
mode 640,and

addr=sbase
for sn=0 to 31
if addr>=sp[0] then goto selct
psprite(sn,addr)
addr=advance(addr)
next sn
sn--

selct:
addr=sbase
if seln then repeat seln addr=advance(addr)
mode 640,xor
if addr<sp[0] then
    {
    psprite(seln,addr)
    psprite(seln,addr)
    }
s=scan
if s=1 then abort
if s=81 then
    {
    if sn=31
      then repeat 32 sbase=advance(sbase)
      else sbase=2
    goto next_page
    }
if s=75 then seln--
if s=77 then seln++
if s=72 then seln-=4
if s=80 then seln+=4
if s=71 then seln=0
if seln>sn then seln=sn
if seln<0 then seln=0
if s=28 then edit(addr):goto redisplay
if s=30 then edit(sp[0]):goto redisplay
if s=32 then
    {

    goto start
    }
goto selct


file:
string 30
