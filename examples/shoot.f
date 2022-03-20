#short
const bullets=20,bull_size=4,key_repeat=1,ammo=999,games=10
const scaley=3,scalex=22
const exps=500,exp_size=5
const things=45,thing_size=5
var x,y,timing,last1,last2,playing,sc1,sc2,sbx,hit1,hit2,m,bdir,am1,am2
expl  ? exps*exp_size
bull1 ? bullets*bull_size
bull2 ? bullets*bull_size

video=0b800h
mode 640,and
#include keys.fi
set_keys

n=timer mod 200
while n x=rnd:n--

computer=0
n=81h
while peekb n<>13
    {
    if (ucase peekb n)='C' then computer=1
    n++
    }

proc abort reset_keys:screen 3:terminate

proc setup_screen
    {
    screen 6
    colour 0:cls
    colour 1
    }

proc setup_players
    {
    am1=ammo:am2=ammo
    x1=10:y1=(rnd mod 160)+20
    x2=620:y2=(rnd mod 160)+20
    fill bullets*bull_size from bull1 with 0
    fillb exps*exp_size from expl with 0
    }

proc move_players
    {
    #long
    if timing and 3 then
	{
	#short
	if key_press(60) then y1-=scaley:if y1<22 then
	    {
	    y1=22
;	    sprite x1,y1,null_thing
;	    y1=176
	    }
	if key_press(62) then y1+=scaley:if y1>176 then
	    {
	    y1=176
;	    sprite x1,y1,null_thing
;	    y1=22
	    }
	#long

	if computer then
	    {
	    cm=bull1
	    ls=y2
	    r=rnd
	    if r>-20000 then goto comp_any
	    repeat bullets*2
		{
		x=peek cm:bd=peekb (cm+3)
		if (x>400) and (bd<128) then
		    {
		    #short
		    y=peekb (cm+2)
		    if y<=(y2+9) then
			{
			if y2>160 then y2-=scaley else y2+=scaley
			}
		    if y>(y2+9) then
			{
			if y2<36 then y2+=scaley else y2-=scaley
			}
		    #long
		    }
		cm+=bull_size
		}
	    comp_any:
	    #short
	    if y2>(ls+scaley) then y2=ls+scaley
	    if y2<(ls-scaley) then y2=ls-scaley
	    if ls=y2 then
		{
		nr=rnd
		if (nr>=0) and (y1<y2) then y2-=scaley
		if (nr<0) and (y1>y2) then y2+=scaley
		}
	    #long
	    }
	else
	    {
	    #short
	    if key_press(75) then y2-=scaley
	    if key_press(79) then y2+=scaley
	    #long
	    }
	#short
	if y2<22 then
	    {
	    y2=22
;	    sprite x2,y2,null_thing
;	    y2=176
	    }
	if y2>176 then
	    {
	    y2=176
;	    sprite x2,y2,null_thing
;	    y2=22
	    }
	#long
	}
    sprite x1,y1,player1
    sprite x2,y2,player2
    #short
    }

proc print_score
    {
    if (last1<>sc1) or (last2<>sc2) then
	{
	cursor 0,2:print bios sc1;"  ";
	cursor 0,73:print bios sc2;"  ";
	cursor 1,3:print bios hit1;"  - ";am1;"  ";
	cursor 1,67:print bios hit2;"  - ";am2;"  ";
	last1=sc1:last2=sc2
	}
    }

proc setup_things
    {
    m=thingd
    repeat things
	{
	pokeb m+3,255
	nr=rnd
	if nr>26000 then
	    {
	    pokeb m,nr
	    pokeb m+1,(rnd mod 160)+20
	    pokeb m+2,100
	    t=rnd mod 3
	    pokeb m+3,t
	    i=40:if t=1 then i=10
	    if t=2 then i=20
	    pokeb m+4,i
	    }
	m+=thing_size
	}
    }

proc crater(cx,cy,cc,cd)
    {
    cm=expl
    repeat exps
	{
	#long
	if cc then
	    {
	    px=peek cm
	    if not px then
		{
		#short
		poke cm,cx:pokeb cm+2,cy
		nr=rnd
		cyi=nr and 7:if nr and 16384 then cyi=0-cyi
		cxi=1+((nr and 11110000b)/16)
		if cd=-1 then cxi=0-cxi
		if cd=0 then if nr and 32768 then cxi=0-cxi

		pokeb cm+3,cxi
		pokeb cm+4,cyi
		cc--
		repeat 8 noise 1,2000+(rnd and 1023)
		noise off
		#long
		}
	    }
	else return

	#short
	cm+=exp_size
	}
    }

proc test_hit
    {
    #long
    if playing then
	{
	if x<(x1+14) then
	    {
	    #short
	    if (y>y1) and (y<(y1+15)) then
		{
		crater(x1,y1,exps,1)
		sc2+=200
		hit2++
		playing=0
		}
	    #long
	    }
	else
	    {
	    #short
	    if (y>y2) and (y<(y2+15)) then
		{
		crater(x2,y2,exps,-1)
		sc1+=200
		hit1++
		playing=0
		}
	    #long
	    }
	}
    #short
    }

proc clear_bullet
    {
    sprite x,y,sp_null
    x=0
    }

function bullet_hit
    {
    tm=thingd
    repeat things
	{
	tt=peekb (tm+3)
	#long
	if tt<>255 then
	    {
	    tx=peekb tm*2+64
	    ty=peekb (tm+1)
	    sa=peek (stable+tt*2)
	    if hit x,y,sp_bull1 with tx,ty,sa 1 then
		{
		#short
		if tt=5 then
		    {
		    sprite x,y,sp_null
		    pokeb m+3,(256-bdir)
		    return 0
		    }
		if tt>2 then
		    {
		    scn=2:if tt=4 then scn=5
		    if sbx=(x1+16) then sc1+=scn else sc2+=scn
		    }
		n=peekb (tm+4)
		n--
		pokeb tm+4,n
		if not n then
		    {
		    sprite tx,ty,null_thing
		    pokeb tm+3,255
		    }
		return 1
		#long
		}
	    }
	tm+=thing_size
	#short
	}
    return 0
    }

proc move_bullets
    {
    bk=41:bi=2:sbx=x1+16:sby=y1     ;bi=scalex 2?
    m=bull1
    repeat 2
	{
	fired=timing mod 5
	repeat bullets
	    {
	    x=peek m
	    #long
	    if x then
		{
		#short
		y=peekb (m+2):bdir=peekb (m+3)
		if (x<(x1+14)) or (x>x2-24) then
		    {
		    test_hit
		    clear_bullet
		    goto next_bullet
		    }

		if bdir<16 then if rnd<-2000 then bdir++
		if bdir>240 then if rnd<-2000 then bdir--
		pokeb m+3,bdir

		if bdir<128
		    then spb=sp_bull1:x+=bdir
		    else x-=256-bdir:spb=sp_bull2

		sprite x,y,spb
		if bullet_hit then
		    {
		    crater(x,y,40,0)
		    clear_bullet
		    goto next_bullet
		    }
		#long
		}
	    else
		{
		if playing then
		    {
		    #short
		    if computer and (bk=78) then
			{
			if (fired=0) and (rnd>27000) then goto fire_it
			}
		    #long
		    if (key_press(bk)<>0) and (fired=0) then
			{
			fire_it:

			if bk=41 then
			    {
			    if am1 then
				{
				am1--
				cursor 1,3:print bios hit1;"  - ";am1;"  ";
				pokeb m+2,sby+8
				pokeb m+3,bi
				x=sbx
				fired=1
				for n=1000 to 1050:noise 2,n:next n:noise off
				}
			    }
			else
			    {
			    if am2 then
				{
				am2--
				cursor 1,67:print bios hit2;"  - ";am2;"  ";
				pokeb m+2,sby+8
				pokeb m+3,bi
				x=sbx
				fired=1
				for n=1000 to 1050:noise 2,n:next n:noise off
				}
			    }
			}
		    }
		}
	    #short
	    next_bullet:
	    poke m,x
	    m+=bull_size
	    }
	bk=78:bi=-16:sbx=x2-24:sby=y2	;bi=-2?
	}
    }

proc plot_exp(c)
    {
    colour c
    plot x-1,y
    plot x+1,y
    plot x,y-1
    plot x,y+1
    }

proc move_exp
    {
    m=expl
    repeat exps
	{
	x=peek m
	#long
	if x then
	    {
	    #short
	    y=peekb (m+2)
	    xi=peekb (m+3):yi=peekb (m+4)
	    plot_exp(0)
	    if xi<128 then x+=xi else x-=(256-xi)
	    if yi<128 then y+=yi else y-=(256-yi)
	    if (x<1) or (x>638) or (y<21) or (y>198) then
		{
		x=0:goto next_exp
		}

	    plot_exp(1)
	    pokeb m+2,y
	    pokeb m+3,xi:pokeb m+4,yi

	    next_exp:
	    poke m,x
	    #long
	    }
	#short
	m+=exp_size
	}
    }

proc move_things
    {
    m=thingd
    repeat things
	{
	t=peekb (m+3)
	#long
	if t<>255 then
	    {
	    #short
	    y=peekb (m+1)
	    yi=peekb (m+2)
	    x=peekb m
	    sa=peek (stable+t*2)

	    if yi<>100 then
		{
		iii=1:if yi>127 then iii=-1:yi=256-yi
		if (timing and yi)=0 then y+=iii
		if (y<20) or (y>180) then
		    {
		    sprite x*2+64,y,null_thing
		    pokeb m+3,255
		    goto next_thing
		    }
		}

	    sprite x*2+64,y,sa
	    pokeb m,x:pokeb m+1,y
	    next_thing:
	    #long
	    }
	else
	    {
	    nr=rnd
	    if nr>31500 then
		{
		pokeb m,nr
		nr=rnd
		tt=3:iii=7:if nr and 16384 then tt=4:iii=0
		if (nr and 40960)=32768 then tt=5:iii=3
		y=20:yi=iii
		if nr and 4096 then y=180:yi=0-iii
		pokeb m+1,y:pokeb m+2,yi
		pokeb m+3,tt
		pokeb m+4,1
		}
	    }
	#short
	m+=thing_size
	}
    }

proc game_it
    {
    timing++
    print_score
    move_bullets
    move_exp
    move_things
    if not (am1 or am2) then am1=ammo:am2=ammo:last1=-1
    }

;BEGIN GAME
start_game:

sc1=0:sc2=0
hit1=0:hit2=0

while (hit1<games) and (hit2<games)
    {
    setup_screen
    setup_players
    playing=1
    last1=-1:last2=-1
    setup_things

    game_it
    move_players
    repeat 3
	{
	for m=2500 to 3800 step 8
	noise 30,m
	next m
	}
    noise off

    while playing
	{
	game_it
	move_players
	if key_press(1) then playing=0
	if key_press(55) then abort
;	repeat 10 repeat 1000 {}
	}

    repeat 160
	{
	repeat 300 {}
	game_it
	}
    }

cursor 10,14
repeat 52 print bios "-";
cursor 11,25
win=1:if hit2=games then win=2
print bios "   SHOOT ->>>  Player ";chr win+'0';" wins!   ";
cursor 12,14
repeat 52 print bios "-";

repeat 50
    {
    noise 1000,(rnd and 1023)+1500
    }
noise off
cursor 24,0:print bios "Another go?  ";

wait_proper:
k=ucase key
if k='Y' then goto start_game
if (k<>'N') and (k<>27) then goto wait_proper
print bios "Wrong!";:beep
abort

thingd:
space things*thing_size

player1:
datab 1,22
shape "                "
shape "                "
shape "                "
shape "**              "
shape "***             "
shape "****            "
shape "*****           "
shape "*******         "
shape "*********       "
shape "**********      "
shape "*************** "
shape "*************** "
shape "**********      "
shape "*********       "
shape "*******         "
shape "*****           "
shape "****            "
shape "***             "
shape "**              "
shape "                "
shape "                "
shape "                "

player2:
datab 1,22
shape "                "
shape "                "
shape "                "
shape "              **"
shape "             ***"
shape "            ****"
shape "           *****"
shape "         *******"
shape "       *********"
shape "      **********"
shape " ***************"
shape " ***************"
shape "      **********"
shape "       *********"
shape "         *******"
shape "           *****"
shape "            ****"
shape "             ***"
shape "              **"
shape "                "
shape "                "
shape "                "

sp_bull1:
datab 3,4
data 0
shape "                  *******       "
data 0
shape "                ****************"
data 0
shape "                ****************"
data 0
shape "                  *******       "

sp_bull2:
datab 3,4
shape "       *******                  "
data 0
shape "****************                "
data 0
shape "****************                "
data 0
shape "       *******                  "
data 0

sp_null:
datab 3,4
data 0,0,0
data 0,0,0
data 0,0,0
data 0,0,0
data 0,0,0

stable:
data swall,stree,srock,sperson,scar,smirror

swall:
datab 1,20
shape "********        "
shape "***    **       "
shape "****    **      "
shape "** **    **     "
shape "**  **    **    "
shape "**   **    **   "
shape "***   **    **  "
shape "** *   **    ** "
shape "**  *   ********"
shape "***  *  **    **"
shape "** *  * **    **"
shape "**  *  ***    **"
shape " **  *  **    **"
shape "  **  * **    **"
shape "   **  ***    **"
shape "    **  **    **"
shape "     ** **    **"
shape "      ****    **"
shape "       ***    **"
shape "        ********"

stree:
datab 1,14
shape "    ** *******  "
shape "  *** *     * * "
shape "   * * **  *   *"
shape "  * *    *     *"
shape "   **   *    ** "
shape "    *** **  *   "
shape "     **   **    "
shape "      ****      "
shape "      ***       "
shape "      ***       "
shape "      ***       "
shape "      ***       "
shape "     ****       "
shape "     *****      "

srock:
datab 1,11
shape "      *****     "
shape "     *** ****   "
shape "   **     ****  "
shape "   ****    **   "
shape "  **  **  ***** "
shape " **  **  **  ** "
shape "** ***    ***** "
shape "****      *  ** "
shape "**         *****"
shape "****************"
shape " ***********    "

sperson:
datab 1,12
shape "                "
shape "     ****       "
shape "     ****       "
shape "      **        "
shape "    ****** *    "
shape "   * **** *     "
shape "     *****      "
shape "    **  **      "
shape "    **  **      "
shape "    **  **      "
shape "    **          "
shape "                "

scar:
datab 1,16
shape "                "
shape "                "
shape "                "
shape "***          ***"
shape "*****  **  *****"
shape "****************"
shape "***  ******  ***"
shape "    ***  ***    "
shape "    ***  ***    "
shape "***  ******  ***"
shape "****************"
shape "*****  **  *****"
shape "***          ***"
shape "                "
shape "                "
shape "                "

smirror:
datab 1,9
shape "                "
shape "  *          *  "
shape " **          ** "
shape "****        ****"
shape "****************"
shape "****        ****"
shape " **          ** "
shape "  *          *  "
shape "                "

null_thing:
datab 1,22
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
shape "                "
