;FAST OTHELLO.
const wx=30
var sy,so,level,d,other,you,inlevel,x,y,c,first_x,first_y,b
hl ? 8*8*2
bo ? 8*8
bs ? 5*8*8

function gbo(gx,gy) return bo-9+gx+gy*8

function gbs(gboard,gx,gy) return bs-73+gboard*64+gy*8+gx

function ghl(gx,gy) return hl+gy*16+gx*2-18

function gdirect(gx,gy) return peek (direct-6+gx*4+gy*2)

proc storeb(bl) move 32 from bo to bs+(bl-1)*64

proc restoreb(bl) move 32 from bs+(bl-1)*64 to bo

procedure set_place(colr)
    {
    pos=(y*3-2)*160+(x*5+wx+1)*2+1
    repeat 2
      {
      repeat 4 video[pos]b=video[pos] xor colr:pos+=2
      pos+=160-8
      }
    }

function get_points
    {
    points=1
    if (b=1) or (b=8) or (c=1) or (c=8) then points=9
    if ((c=1) or (c=8)) and ((b=1) or (b=8)) then points=180
    if ((c=2) or (c=7)) and ((b=2) or (b=7)) then points=-180
    if (((c=2) or (c=7)) and ((b=1) or (b=8))) or (((c=1) or (c=8)) and ((b=2) or (b=7)))
      then points=10
    return points
    }

procedure total
    {
    sc=0

    b=1:c=1
    for tm=0 to 63
      if peekb (bo+tm)=sy then sc+=get_points*3
      if peekb (bo+tm)=so then sc-=get_points
    c++:if c=9 then c=1:b++
    next tm

    tm=ghl(first_x,first_y)
    if sc<peek tm then poke tm,sc
    }

procedure testdirect
    {
    ok=0
    xx=x:yy=y:first=1
    testloop:
    xx+=gdirect(d,1):yy+=gdirect(d,2)
    if (peekb gbo(xx,yy)=0) or (xx<1) or (yy<1) or (xx>8) or (yy>8)
	then goto exit
    if (peekb gbo(xx,yy)<>other) and first then goto exit
    first=0
    if peekb gbo(xx,yy)<>you then goto testloop
    ok=1
    exit:
    d++
    }

procedure valid
    {
    ok=0
    if peekb gbo(x,y) then goto endv
    d=1
    while (d<9) and (ok=0) testdirect
    endv:
    }

procedure flip
    {
    d=1
    flip_rep:
    testdirect
    if ok then
	{
	xx=x:yy=y
	lx=x:ly=y
	test_flip:
	pokeb gbo(xx,yy),you
	xx+=gdirect(d-1,1):yy+=gdirect(d-1,2)
	if peekb gbo(xx,yy)<>you then goto test_flip
	}
    if d<9 then goto flip_rep
    }

procedure checkend
    {
    a=0
    for x=1 to 8
    for y=1 to 8
    if not peekb gbo(x,y) then a++
    next y,x
    }

procedure process_level
    {
    inlevel--
    swap you,other
    for x=1 to 8
    for y=1 to 8
    valid
    if ok then
      {
      storeb(inlevel)
      if level=inlevel then first_x=x:first_y=y:inlevel=level

      flip
      if inlevel>1 then
	{
	push x,y:process_level:pop y,x
	}
      else
	{
	total
	}
      restoreb(inlevel)
      }
    next y,x
    inlevel++
    }

procedure getcomp
    {
    storeb(level+1)
    fill 64 from hl with 20000

    so=other:sy=you
    swap you,other

    inlevel=level+1
    process_level

    other=so:you=sy
    restoreb(level+1)

    hlx=0:hly=0
    loss=-10000

    for x=1 to 8
    for y=1 to 8
	tm=peek ghl(x,y)
	if (tm<>20000) and (tm>loss) then loss=tm:hlx=x:hly=y
    next y,x

    x=hlx:y=hly
    valid
    if not ok then
      {
      for x=1 to 8
      for y=1 to 8
	valid
	if ok then hlx=x:hly=y
      next y,x
      }

    x=hlx:y=hly
    }

procedure sd
    {
    tx=x:ty=y
    for x=1 to 8
    for y=1 to 8
      valid
      if ok then set_place(78h)
    next y,x
    x=tx:y=ty
    }

procedure show
    {
    sd
    locate 20,0
    print "Press any key to continue."
    wait for key
    locate 20,0
    print "                          "
    sd
    }

procedure getyou
    {
    quit=0
    x=lx:y=ly
    curset:
    valid:cc=40h:if ok then cc=78h
    set_place(cc)
    wait for keypressed
    ks=keyscan:k=lcase low ks:s=high ks
    set_place(cc)
    if k='q' then quit=1:k=13
    if k='s' then show
    if s=72 then
	{
	y--:if y<1 then y=8
	}
    if s=80 then
	{
	y++:if y>8 then y=1
	}
    if s=75 then
	{
	x--:if x<1 then x=8
	}
    if s=77 then
	{
	x++:if x>8 then x=1
	}
    if k<>13 then goto curset
    lx=x:ly=y
    }

;	     START MAIN PROGRAM
start_game:

lpass=0
colour 7
cls
you=1:lx=1:ly=1
locate 3,36
colour 15:print "OTHELLO"
locate 7,10
colour 6
print "How many human players (0-2)?";
get_players:
loctocur
pl=inputb
if pl>2 then goto get_players
if pl<>2 then
    {
    locate 10,10
    print "Computer(2) skill level (1-4)?";
    get_sk1:
    loctocur
    lv2=inputb
    if (lv2<1) or (lv2>4) then goto get_sk1
    }
if not pl then
    {
    locate 13,10
    print "Computer(1) Skill level (1-4)?";
    get_sk2:
    loctocur
    lv1=inputb
    if (lv1<1) or (lv1>4) then goto get_sk2
    }
 else
    {
    locate 13,10
    print "Who starts? (B=BLACK W=WHITE) ";
    get_start:
    loctocur
    wait for keypressed
    k=lcase key
    if (k<>'b') and (k<>'w') then goto get_start
    you=2:if k='b' then you=1
    }

colour 7
cls
fill 32 from bo with 0

pokeb gbo(4,4),1
pokeb gbo(5,5),1
pokeb gbo(4,5),2
pokeb gbo(5,4),2
locate 0,10
colour 15:print "OTHELLO"

colour 7
for y=1 to 8
for x=1 to 8
if y>1 then
  {
  locate y*3-3,x*5+wx+1
  repeat 4 print chr 205;
  if x<8 then print chr 206;
  }
if x<>1 then
  {
  locate y*3-2,x*5+wx
  print chr 186;
  locate y*3-1,x*5+wx
  print chr 186;
  }
next x,y

for x=1 to 8
  locate 0,x*5+wx+1
  repeat 4 print chr 205;
  print chr 203;
  locate 24,x*5+wx+1
  repeat 4 print chr 205;
  print chr 202;
  locate x*3-3,wx+5
  print chr 204;
  locate x*3-2,wx+5
  print chr 186;
  locate x*3-1,wx+5
  print chr 186;
  locate x*3-3,wx+45
  print chr 185;
  locate x*3-2,wx+45
  print chr 186;
  locate x*3-1,wx+45
  print chr 186;
next x

locate 0,wx+5:print chr 201;
locate 24,wx+5:print chr 200;
locate 0,wx+45:print chr 187;
locate 24,wx+45:print chr 188;

display:
colour 15
for y=1 to 8
for x=1 to 8
  h=y*3-2:l=5*x+wx+2
  c=32
  if peekb gbo(x,y)=1 then c=176
  if peekb gbo(x,y)=2 then c=219
  locate h,l
  print chr c;chr c;
  locate h+1,l
  print chr c;chr c;
next x,y

colour 6
b1=0:b2=0
for y=1 to 8
for x=1 to 8
  if peekb gbo(x,y)=1 then b1++
  if peekb gbo(x,y)=2 then b2++
next x,y
locate 14,0
print "ENTER to go. Q to quit."
print "S displays valid moves."

if pl<>2 then
  {
  locate 11,14
  printb "L";chr('0'+lv2);
  if pl=0 then locate 5,14:printb "L";chr('0'+lv1);
  }

locate 3,1
print "PLAYER ONE   ";chr 176;chr 176;
locate 9,1
print "PLAYER TWO   ";chr 219;chr 219;
locate 4,14
print chr 176;chr 176;
locate 10,14
print chr 219;chr 219;
locate 5,1
printb "BLACK = ";B1;
locate 11,1
printb "WHITE = ";B2;
cursor 3,17
if you=2 then cursor 9,17
checkend
if a then goto retry
ascore:
locate 20,1
a=2-(b1>b2)
if b1=b2
    then print "THE GAME IS A DRAW!"
    else print "PLAYER "chr ('0'+a)" HAS WON.";
goto another

retry:
other=1+(you=1)
passed=1
for x=1 to 8
for y=1 to 8
  valid
  if ok then passed=0
next y,x
if passed then
  {
  if lpass then goto ascore
  lpass=1
  locate 22,1
  print "PASS! Press ENTER"
  wait for key=13
  locate 22,1
  print "                     ";
  goto turn
  }
lpass=0

comp=1
level=lv1:if you=2 then level=lv2
if (pl=0) or ((pl=1) and (you=2))
  then {
       quit=0
       getcomp
       if key='q' then quit=1
       }
  else {
       getyou
       comp=0
       }
colour 6
if quit then goto abort

valid
if (ok=0) and (comp=1) then goto turn
if not ok then beep:goto retry
flip

turn:
you++
if you=3 then you=1
goto display

abort:
locate 22,1
print "Do you want to quit (Y/N)?";
abort_key:
k=lcase key
if (k<>'y') and (k<>'n') then goto abort_key
locate 22,1
print "                               ";
if k='n' then goto retry
p1:
another:
pp:
locate 22,1
print "Another game (Y/N)?";
anok:
k=lcase key
if k='y' then goto start_game
if k<>'n' then goto anok
cursor 24,0
terminate

direct:
data 0,-1
data 1,-1
data 1,0
data 1,1
data 0,1
data -1,1
data -1,0
data -1,-1
