#short
var x1,y1
b ? 8*8
p ? 8*2

const basey=5,basex=20

proc prod(pdx,pdy)
    {
    pb=peekb (b+pdx+pdy*8)
    if pb>127 then pb=pb-256
    if pb<>100 then
	{
	print " ";
	if pb>=0 then
	    {
	    print chr '0'+pb/10;chr '0'+pb mod 10;
	    }
	else
	    {
	    print "-";chr '0'+(0-pb) mod 10;
	    }
	print " ";
	}
    else print "    ";
    }


begin:
sc1=0:sc2=0
colour 7:cls
xx=0:yy=0
total=0
for x=0 to 7:for y=0 to 7
v=(rnd mod 25)-9
pokeb b+x+y*8,v
total+=v
next y,x
locate 2,29:colour 47h:print "     N U M B E R S     "
colour 60h
retry1:
locate 10,20:print "How many players (0,1,2)";
cursor 10,50:players=input
if (players<0) or (players>2) then goto retry1
if players=2 then goto start2
retry2:
locate 12,20:print "Skill level for computer (1-4)"
cursor 12,50:skill=input
if (skill<1) or (skill>4) then goto retry2

start2:
pl=1
start:

colour 7
cls
locate 2,29:colour 47h:print "     N U M B E R S     "
colour 30h
for y=0 to 8
for x=0 to 7
locate basey+y*2,basex-1+x*5
if x=0 then print chr 204; else locpos+=2
c=206:if x=7 then c=185
if y=0 then c=203
if y=8 then c=202
repeat 4 print chr 205;
print chr c;
locate basey+1+x*2,basex-1+y*5:print chr 186;
next x,y
locate basey,19:print chr 201;
locate basey,59:print chr 187;
locate basey+14,19:print chr 200;
locate basey+14,59:print chr 188;

colour 40h
for x=0 to 7:for y=0 to 7
locate basey+1+y*2,basex+x*5
prod(x,y)
next y,x

lx=100
loop:

locate 8,5:colour 17h
print "PLAYER 1"
locate 8,67:print "PLAYER 2"
colour 71h
if players=0 then locate 9,6:print " PC ";skill;" ";

if players<2 then locate 9,68:print " PC ";skill;" ";

colour 79
locate 11,4:print "SCORE ";
if sc1<0 then print "-";0-sc1; else print sc1;
colour 7:print "   ";
colour 79
locate 11,66:print "SCORE ";
if sc2<0 then print "-";0-sc2; else print sc2;
colour 7:print "   ";

locate 13,8:print "  ":locate 13,70:print "  "
locate 13,pl*62-54:colour 7:print "GO"

if lx<>100 then
  {
  lx=xx:ly=yy
  locate basey+1+yy*2,basex+xx*5:colour 4
  prod(xx,yy)
  }
z=0
for a=0 to 7
if (pl=1) and (peekb (b+xx+a*8)<>100) then z=1
if (pl=2) and (peekb (b+a+yy*8)<>100) then z=1
next a
if not z then goto endgame

go:
locate basey+1+yy*2,basex+xx*5:colour 4
prod(xx,yy)
if (players=0) and (pl=1) then goto computer
if (players<2) and (pl=2) then goto computer
wait for keypressed
ks=keyscan:k=low ks:s=high ks
if k=27 then goto endgame
if (k=13) and (peekb(b+xx+yy*8)<>100) then goto take
locate basey+1+yy*2,basex+xx*5:colour 40h
prod(xx,yy)
if pl=1 then
  {
  if s=72 then yy--
  if yy<0 then yy=7
  if s=80 then yy++
  if yy>7 then yy=0
  }
if pl=2 then
  {
  if s=75 then xx--
  if xx<0 then xx=7
  if s=77 then xx++
  if xx>7 then xx=0
  }
goto go

computer:
diff=0
pp2=1:if pl=1 then pp2=2
lp=pp2:pp=pl
fill 8 from p with 1000
gosub exchange
if skill=1 then	gosub level1
else if	skill=2	then gosub level2
else if	skill=3	then gosub level3
else gosub level4		  ;on ns gosub level1,level2,level3,level4
diff=-1000:m=0
for a=0 to 7
if peek (p+a*2)<>1000 then
  {
  if (peek (p+a*2)=diff) and (rnd<0) then m=a:diff=peek (p+a*2)
  if (peek (p+a*2)>diff) then m=a:diff=peek (p+a*2)
  }
next a
if pl=1 then yy=m else xx=m
#long
if (peekb(b+xx+yy*8)=100) or (m=0) then
  {
#short
  for a=0 to 7
  if (pl=1) and (peekb (b+xx+a*8)<>100) then yy=a
  if (pl=2) and (peekb (b+a+yy*8)<>100) then xx=a
  next a
  }

take:
pb=b+xx+yy*8
pbs=peekb pb
if pbs>127 then pbs=pbs-256

if pl=1 then sc1+=pbs else sc2+=pbs

pokeb pb,100
if lx<>100 then colour 40h:locate basey+1+ly*2,basex+lx*5:print "    "
pl=pl xor 3
lx=0:goto loop

endgame:
colour 7
locate 23,22
margin=sc2-sc1:wp=2:if sc1>sc2 then wp=1:margin=0-margin
if sc1<>sc2 then print "Player ";WP;" Wins with a Margin of ";margin
colour 70h
locate 24,1:print "Another game (Y/N) ";
retry_ano:
k=lcase	key
if k='y' then goto begin
if k<>'n' then goto retry_ano

cursor 24,0
print bios
stop

level1:
if skill=1 then x=xx:y=yy
gosub exchange
aa1=diff
for a1=0 to 7
if peekb(b+x+y*8)<>100 then
  {
  if pp=pl then diff+=peekb(b+x+y*8) else diff-=peekb(b+x+y*8)
  gosub gettotal:diff=aa1
  }
x+=x1:y+=y1
next a1
return

level2:
if skill=2 then x=xx:y=yy
gosub exchange
aa2=diff
for a2=0 to 7
#long
if peekb(b+x+y*8)<>100 then
  {
#short
  if pp=pl then diff+=peekb(b+x+y*8) else diff-=peekb(b+x+y*8)
  push x:push y:push peekb (b+x+y*8)
  pokeb b+x+y*8,100
  gosub level1
  gosub exchange
  pop z,y,x
  pokeb b+x+y*8,z:diff=aa2
  }
x+=x1:y+=y1
next a2
return

level3:
if skill=3 then x=xx:y=yy
gosub exchange
aa3=diff
for a3=0 to 7
#long
if peekb(b+x+y*8)<>100 then
  {
#short
  if pp=pl then diff+=peekb(b+x+y*8) else diff-=peekb(b+x+y*8)
  push x:push y:push peekb(b+x+y*8)
  pokeb b+x+y*8,100
  gosub level2
  gosub exchange
  pop z,y,x:pokeb b+x+y*8,z:diff=aa3
  }
x+=x1:y+=y1
next a3
return

level4:
if skill=4 then x=xx:y=yy
gosub exchange
aa4=diff
for a4=0 to 7
#long
if peekb(b+x+y*8)<>100 then
  {
#short
  if pp=pl then diff+=peekb(b+x+y*8) else diff-=peekb(b+x+y*8)
  push x,y,peekb (b+x+y*8)
  pokeb b+x+y*8,100
  gosub level3
  gosub exchange
  pop z,y,x:pokeb b+x+y*8,z:diff=aa4
  }
x+=x1:y+=y1
next a4
return


exchange:
x1=0:y1=0
swap pp,lp
if pp=1 then y=1:y1=1 else x=1:x1=1
return

gettotal:
s=peekb (b+x+y*8)
a=a1
if skill=2 then a=a2
if skill=3 then a=a3
if skill=4 then a=a4
if diff<peek (p+a*2) then poke p+a*2,diff
return
