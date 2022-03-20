;A game using the sprites.

var ce,score,high_score,lives
var a,b

const spacedots=200,bullets=7,enemy=7,backdots=2000,deftib=2,key_repeat=1
const spaceloop=80

back_dots ? backdots*4
edata	  ? enemy*4
bdata	  ? bullets*3
dots	  ? spacedots*10

#include vga.fi
#include keys.fi

;== functions/procedures ======================================================

function fix_sprite(m,f)
    {
    w=peekb m
    r=peekb (m+1)
    m+=2
    repeat r
	{
	repeat w
	    {
	    if peekb m=' ' then pokeb m,0
	    if f then if peekb m='*' then pokeb m,f
	    m++
	    }
	}
    }

proc delay(dc)
    {
    repeat dc repeat 1000 {}
    }

function setup_space
    {
    colour 7
    cursor 12,18:print bios "SPACE";
    c=0:m=dots
    for y=96 to 103
    for x=144 to 183
    if vpoint(x,y) then
	{
	poke m,x:pokeb m+2,y
	poke m+5,rnd mod 425:poke m+7,rnd mod 200
	s=rnd
	if s and 4 then poke m+5,0-peek (m+5)
	if s and 2 then poke m+7,0-peek (m+7)
	m+=10:c++
	if c>=spacedots then return
	}
    next x
    next y
    }

proc show_space
    {
    colour 0:cls

    for rep=1 to spaceloop
	m=dots
	for a=1 to c
	    x=peek m
	    y=peekb (m+2)
	    lx=peekb (m+3):ly=peekb (m+4)
	    lx+=peek (m+5):ly+=peek (m+7)
	    pokeb m+3,lx:pokeb m+4,ly
	    if lx>255 then x+=high lx
	    if ly>255 then y+=high ly
	    if lx<0 then x-=256-high lx
	    if ly<0 then y-=256-high ly
	    poke m,x:pokeb m+2,y
	    m+=10
	    if rep=spaceloop then vplot (x,y,cdot)     ;??
	next a
    next rep

    for rep=1 to spaceloop
	m=dots
	for a=1 to c
	    x=peek m:y=peekb (m+2)
	    vplot (x,y,cblack)
	    lx=peekb (m+3):ly=peekb (m+4)
	    lx-=peek (m+5):ly-=peek (m+7)
	    pokeb m+3,lx:pokeb m+4,ly
	    if lx>255 then x+=high lx
	    if ly>255 then y+=high ly
	    if lx<0 then x-=256-high lx
	    if ly<0 then y-=256-high ly
	    vplot (x,y,cdot)
	    poke m,x:pokeb m+2,y
	    m+=10
	next a
	for b=1 to 100 step 6:for a=1 to 30:noise 20,b*5+a*10+rep*5:next a,b
    next rep
    repeat 20
	{
	for b=1 to 100 step 4:for a=1 to 30:noise 20,b*5+a*10+rep*5:next a,b
	}
    noise off
    }

proc set_background
    {
    add=back_dots
    repeat backdots
	{
	poke add,rnd mod 320
	pokeb add+2,(rnd mod 175)+25
	pokeb add+3,16+(rnd and 15)
	add+=4
	}
    }

proc print_lives
    {
    xx=lives-1
    while xx>0
	{
	vsprite (xx*40+125,0,spyou)
	xx--
	}
    }

proc print_score
    {
    cursor 0,0:print bios "Score ";score;"  High ";high_score;
    }

proc start_life
    {
    colour 0:cls
    print_lives
    x=40:y=90
    fillb bullets*3 from bdata with 0
    fillb enemy*4 from edata with 0
    tib=0:dtime=0
    times=0:frame=0
    print_score
    }

proc display_background
    {
    add=back_dots
    repeat backdots
	{
	gdx=peek add:gdy=peekb (add+2)
	vplot (gdx,gdy,cblack)
	gdx-=2:if gdx<0 then gdx=319
	vplot (gdx,gdy,peekb (add+3))
	poke add,gdx
	add+=4
	}
    }

proc process_you
    {
    if dtime=1 then return 0
    if dtime then dtime--
    else
	{
	vsprite (x-18,y,thrust+frame*226)
	vsprite (x-18,y+10,thrust+frame*226)
	vsprite (x,y,spyou)
	if key_press(44) then y+=3
	if key_press(30) then y-=3
	if y<30 then y=30
	if y>175 then y=175
	}
    return 1
    }

proc process_bullets
    {
    add=bdata:fire++
    if tib>0 then tib--
    repeat bullets
	{
	gbx=peek add:gby=peekb (add+2)
	f=key_press(28)
	if (dtime=0) and (gby=0) and f and ((fire and 3)=0) and (tib=0) then
	    {
	    for a=600 to 1000 step 12:noise 2,a:next a:noise off
	    gbx=x+8:gby=y+10
	    tib=deftib
	    }
	if gby then
	    {
	    if gbx>310 then vsprite (gbx,gby,null_bull):gby=0:goto end_b
	    gbx+=8
	    vsprite (gbx,gby,bullet)

	    add2=edata
	    repeat enemy
		{
		if not peekb (add2+3) then
		    {
		    pokeb bullet,1
		    pokeb enemysp,1
		    if hit gbx,gby,bullet with peek add2,peekb (add2+2),enemysp 1
			then
			{
			score+=10
			if score>high_score then high_score=score
			print_score
			vsprite (gbx,gby,null_bull)
			pokeb add2+3,5
			gby=0
			}
		    pokeb bullet,16
		    pokeb enemysp,16
		    add2+=4
		    }
		}
	    }
	end_b:
	poke add,gbx:pokeb add+2,gby
	add+=3
	}
    }

proc process_death
    {
    add2=dots:ce=0
    for a=y+3 to y+18 step 2
    for b=x to x+31 step 2
    poke add2,b:pokeb add2+2,a
    pokeb add2+3,(rnd and 7)+1:poke add2+4,rnd
    add2+=10:ce++
    if ce>=spacedots then return
    next b
    next a
    }

proc process_enemy
    {
    add=edata
    repbase=1
    rep=repbase
    repeat enemy
	{
	ex=peek add:ey=peekb (add+2)
	time=peekb (add+3)
	if (ey=0) then ex=319:ey=(rnd mod 170)+10:time=0
	if time then
	    {
	    time--
	    for a=1 to 40:noise 1,1000+rnd and 2047:next a:noise off
	    if time then vsprite (ex,ey+3,explosion+(6-time)/2*82)
		else vsprite (ex,ey+3,en_null):ey=0
	    goto end_e
	    }
	if ex<x+45 then
	    {
	    hf=0
	    pokeb enemysp,1
	    if dtime=0 then hf=hit x,y,spyou with ex,ey,enemysp 3
	    if hf then process_death:dtime=35
	    if ex<10 then
		{
		vsprite (ex,ey+3,en_null)
		ey=0
		goto end_e
		}
	    }
	rr=rnd
	if ex>(70+repbase*7) then
	    {
	    if rr and 1 then ey+=rr mod 3 else ey-=rr mod 3
	    }
	else
	    {
	    if ey>y
	    then ey-=rr and 3
	    else ey+=rr and 3
	    }
	if ey<25 then ey=25
	if ey>185 then ey=185
	ex-=rep
	pokeb enemysp,32
	vsprite (ex,ey,enemysp)
	end_e:
	poke add,ex:pokeb add+2,ey:pokeb add+3,time
	add+=4
	rep++:if rep>10 then rep=10
	}
    }

proc process_explosion
    {
    if dtime then
	{
	add=dots
	repeat ce
	    {
	    ex=peek add:ey=peekb (add+2)
	    xi=peekb (add+3)
	    yi=peek (add+4)
	    noise 1,rnd and 8191
	    vplot (ex,ey,cblack)
	    ex+=xi
	    if yi and 128
	    then ey-=(yi mod 3):ey-=rnd and 1
	    else ey+=yi mod 3:ey+=rnd and 1
	    vplot (ex,ey,40)
	    poke add,ex:pokeb add+2,ey
	    add+=10
	    }
	noise off
	}
    }

proc process_timing
    {
    delay(50)
    times++
    if times>360 then
	{
	repbase+=rnd and 1
	if repbase>5 then repbase=5
	times=0
	}
    dd2:
    frame++:if frame>2 then frame=0
    }

proc game_over
    {
    for y=0 to 20
    cursor y+1,y:print bios "GAME OVER";
    noise 600,y*50+2000
    delay(50)
    cursor y+1,y:print bios "         ";
    next y
    noise off
    }

;== setup =====================================================================

fix_sprite(thrust,0)
fix_sprite(thrust+226,0)
fix_sprite(thrust+452,0)
fix_sprite(spyou,1)
fix_sprite(null_bull,0)
fix_sprite(bullet,2)
fix_sprite(explosion,0)
fix_sprite(explosion+82,0)
fix_sprite(explosion+164,0)
fix_sprite(en_null,0)
fix_sprite(enemysp,115)

high_score=0

vscreen
set_keys
set_background

;== start game ================================================================

start:
setup_space
show_space
score=0:lives=5

while lives
    {
    start_life

    while process_you
	{
	process_timing
	display_background
	process_bullets
	process_explosion
	process_enemy
	if key_press(1) then goto exit_game
	}

    lives--
    }

game_over
goto start

exit_game:
reset_keys
screen 3
stop

;== graphics ==================================================================

spyou:
datab 32,22
datab '                                '
datab '                                '
datab '                                '
datab '    ******                      '
datab '****************                '
datab '******                          '
datab '*******                         '
datab '******************              '
datab '** * * ** ***** ***********     '
datab '******** ***** ********  ****** '
datab '*** * * ***** ******************'
datab '*** * * ***** ******************'
datab '******** ***** ********  ****** '
datab '** * * ** ***** ***********     '
datab '******************              '
datab '*******                         '
datab '******                          '
datab '****************                '
datab '    ******                      '
datab '                                '
datab '                                '
datab '                                '

thrust:
datab 16,14
datab '                '
datab '                '
datab '                '
datab '           PZ   '
datab '          ef W  '
datab '         W  [ V '
datab '       e  d  c d'
datab '      aa c  e  f'
datab '         g ac T '
datab '          Q  X  '
datab '           YZ   '
datab '                '
datab '                '
datab '                '

datab 16,14
datab '                '
datab '                '
datab '                '
datab '                '
datab '        Y VY    '
datab '      PS U   R  '
datab '  a f e   c c e '
datab '   [ ]  e d  a c'
datab '      cc e  d   '
datab '        VW Q    '
datab '                '
datab '                '
datab '                '
datab '                '

datab 16,14
datab '                '
datab '                '
datab '                '
datab '                '
datab '                '
datab '           RS   '
datab '   d  f e a  e a'
datab 'c e d c  e e f f'
datab '        R  W    '
datab '                '
datab '                '
datab '                '
datab '                '
datab '                '

null_bull:
datab 16,2
datab '                '
datab '                '

bullet:
datab 16,2
datab '        ********'
datab '        ********'

enemysp:
datab 32,11
datab '                                '
datab '                                '
datab '                                '
datab '  **************                '
datab ' ****     ***                   '
datab '************                    '
datab ' *****    ***                   '
datab '  **************                '
datab '                                '
datab '                                '
datab '                                '

en_null:
datab 16,5
datab '                '
datab '                '
datab '                '
datab '                '
datab '                '

explosion:
datab 16,5
datab '  *   *   *  *  '
datab '  *     *     * '
datab ' *             *'
datab '   * *    *    *'
datab '      * * *  *  '

datab 16,5
datab '        *  *    '
datab '    * *   *     '
datab '    *      *    '
datab '     *   *      '
datab '       *        '

datab 16,5
datab '                '
datab '      *         '
datab '     ****       '
datab '       *        '
datab '                '
