;Penetrator/Scrambler sort of game.

var xx,scr,x,top,bot,yourx,oldy,yy,xx,back,play
const cc=112,maxdloop=1500
const maxrockets=6,maxbullets=10
const key_repeat=1
high_score=0
rocks ? maxrockets*3
bulls ? maxbullets*2

speed=input

#include keys.fi
set_keys

start:

diff=12
dloop=maxdloop

function sn(vy,vx) return video[vy*160+vx*2+1]b

function above(ay,ax)
    {
    while sn(ay,ax)=cc ay--
    return ay+1
    }

procedure bullets
    {
    colour 15
    fired++
    for m=bulls to bulls-1+2*maxbullets step 2
    bx=peekb m:by=peekb (m+1)
    if bx then locate by,bx-1:print " ";
    if (not bx) and ((fired and 3)=0) and (key_press(28)) then
	{
	bx=yourx+1:by=yy
	}
    if bx then bx+=2
    if bx>78 then bx=0
    if bx then
	{
	b1=sn(by,bx-2):b2=sn(by,bx-1):b3=sn(by,bx)
	if (b1=cc) or (b2=cc) or (b3=cc) then
	    {
	    xx=bx:if b2=cc then xx=bx-1
	    if b1=cc then xx=bx-2
	    oy=above(by,xx)
	    if (oy>8) and (oy<=by) then video[oy*160+xx*2]=0f20h
	    bx=0
	    }
	else locate by,bx:print chr 16;
	}

    pokeb m,bx:pokeb m+1,by
    next m
    }

procedure rockets
    {
    for m=rocks to rocks-2+3*maxrockets step 3
    rx=peekb m:ry=peekb (m+1):rj=peekb (m+2)
    if rx>1 then
	{
	colour 15:locate ry,rx-1:print " ";
	}
    if rx then rx--
    if rj then
	{
	rj++
	if rj>2 then rj=1:ry--
	if sn(ry,rx)<>15 then rx=0:rj=0
	}
    if rx
    then
	{
	if (rnd>32200) or ((rnd>28000) and ((rx>yourx+2) and (rx<yourx+15)))
	then
	    {
	    rj=1
	    for rr=200 to 300 step 5:noise 1,rr:next rr:noise off
	    }
	}
    else
	{
	if (rnd>31500) and (bot>20) then
	    {
	    rx=78
	    ry=bot-1
	    rj=0
	    }
	}
    if rx then colour 14:locate ry,rx:print chr 234;
    pokeb m,rx:pokeb m+1,ry:pokeb m+2,rj
    next m
    }

procedure moveyou
    {
    mok++
    if play and ((mok and 3)=0) then
	{
	if key_press(30) then yy--
	if key_press(44) then yy++
	}
    }

procedure drawit
    {
    for a=160*6+2+x*2 to top*160+159 step 160
	video[a]=cc*256+' '
    next a
    for a=bot*160+2+x*2 to 24*160 step 160
	video[a]=cc*256+' '
    next a
    }

procedure delay if speed then repeat speed {}

procedure moveit
    {
    a=rnd
    z=(a and 1)+1
    if a and 16 then top+=z
    if ((a and 32)=32) or ((a and 57344)=0) then top-=z
    if a and 64 then bot+=z
    if a and 256 then bot-=z
    if top<6 then top=6
    if bot<top+diff then bot=top+diff
    if bot>23 then bot=23
    if top>bot-diff then top=bot-diff
    }

procedure scrollit
    {
    scr=962
    colour 15
    for sy=6 to 23
	if sy=oldy then
	    {
	    locate sy,yourx:print "   ";
	    back=sn(yy,yourx+3)
	    }
	move 77 from video|scr+2 to video|scr
	video[scr+154]=15*256+32
	scr+=160
	if sy=yy then
	    {
	    locate yy,yourx:print chr 219;chr 216;chr 216;
	    newy=yy
	    }
    next sy
    oldy=newy
    }

colour 15
cls
cursor 32,0

for x=0 to 79
locate 0,x:print chr 196;
locate 24,x:print chr 196;
locate 5,x:print chr 196;
if x<25 then
    {
    locate x,0:print chr 179;
    locate x,79:print chr 179;
    }
next x
locate 0,0:print chr 218;
locate 24,0:print chr 192;
locate 0,79:print chr 191;
locate 24,79:print chr 217;
locate 5,0:print chr 195;
locate 5,79:print chr 180;

locate 2,22:print "P . E . N . E . T . R . A . T . O . R"
locate 4,66:print "HIGH ";high_score;

top=7
for x=0 to 77
  bot=23
  drawit
  moveit
next x

x=77:oldy=0
yourx=30
score=0:locate 4,4:print "SCORE ";score;
locate 4,37:flash 1:print "READY?";:flash 0

play=0
iff=200

repeat 100
    {
    noise 1,iff:iff+=20
    bot=23
    drawit
    moveit
    yy=21
    moveyou
    scrollit
    delay
    }

noise off
locate 4,37:print "      ";

begin_life:

fillb 3*maxrockets from rocks with 0
fill maxbullets from bulls with 0

play=1
alive=1
while alive
    {
    drawit
    moveit
    moveyou
    bullets
    scrollit
    rockets
    if key_press(1) then alive=0

    if dloop then dloop--
    else
	{
	diff--:dloop=maxdloop
	if diff<6 then diff=6
	}
    delay
    score++
    locate 4,10:print score;
    }


exit_prog:
reset_keys
noise off
cls
cursor 0,0
terminate
