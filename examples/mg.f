;MOVE GAME in FAST V5.93+

;To do:
;Aliens
;Trees
;Bullet detection

const bullets=4,bsize=4,aliens=6,asize=6
const max_dots=400,dot_len=8,key_repeat=1
var killed
bs	 ? bullets*bsize
as	 ? aliens*asize
dot_data ? dot_len*max_dots

#include keys.fi
set_keys


proc abort
    {
    reset_keys
    screen 3
    terminate
    }

colour 0
screen 6 ; Hi-res graphics
cls
colour 3

high=0

proc setup
    {
    colour 0
    cls
    colour 3
    x=310:y=130:d=0
    fired=0
    ap=spy_up
    m=bs
    repeat bullets
	{
	poke m,-1
	m+=bsize
	}
    m=as
    repeat aliens
	{
	poke m,-1
	m+=asize
	}
    }

proc dead
    {
    lives--
    }

proc setup_dots
    {
    fillb dot_len*max_dots from dot_data with 0
    }

proc update_dots
    {
    added=0
    m=dot_data
    repeat max_dots
	{
	dotx=peek m:doty=peek(m+2)
	if dotx then
	    {
	    dotxd=peekb(m+4):dotxi=peekb(m+5)
	    dotyd=peekb(m+6):dotyi=peekb(m+7)
	    colour 0
	    plot dotx,doty

	    if dotxi<128 then
		{
		dotxd+=dotxi*4:if dotxd>255 then dotx+=high dotxd
		}
	    else
		{
		dotxd+=(dotxi-128)*4:if dotxd>255 then dotx-=high dotxd
		}
	    if dotyi<128 then
		{
		dotyd+=dotyi*4:if dotyd>255 then doty+=high dotyd
		}
	    else
		{
		dotyd+=(dotyi-128)*4:if dotyd>255 then doty-=high dotyd
		}
	    if dotx<1 then dotx=639
	    if dotx>639 then dotx=1
	    if doty<0 then doty=199
	    if doty>199 then doty=0

	    colour 1
	    plot dotx,doty
	    }
	else
	    {
	    if not added then
		{
		dotx=rnd mod 640
		doty=rnd mod 200
		dotxi=rnd
		dotyi=rnd
		added=1
		}
	    }

	poke m,dotx:poke m+2,doty
	pokeb m+4,dotxd:pokeb m+5,dotxi
	pokeb m+6,dotyd:pokeb m+7,dotyi
	m+=dot_len
	}
    }

proc move_you
    {
    tim=not tim
    if not killed then
	{
	moved=0
	if key_press(72) then moved=1
	if key_press(73) then moved=2
	if key_press(77) then moved=3
	if key_press(81) then moved=4
	if key_press(80) then moved=5
	if key_press(79) then moved=6
	if key_press(75) then moved=7
	if key_press(71) then moved=8
	if moved then d=moved

	if moved=1 then y-=2:ap=spy_up
	if moved=2 then x+=3:y-=1+tim:ap=spy_ri
	if moved=3 then x+=4:ap=spy_ri
	if moved=4 then x+=3:y+=1+tim:ap=spy_ri
	if moved=5 then y+=2:ap=spy_dn
	if moved=6 then x-=3:y+=1+tim:ap=spy_le
	if moved=7 then x-=4:ap=spy_le
	if moved=8 then x-=3:y-=1+tim:ap=spy_le
	if y<0 then y=0
	if x<0 then x=0
	if y>180 then y=180
	if x>608 then x=608
	}
    else
	{
	repeat 100 noise 1,rnd and 511
	}

    mode 640,and
    sprite x,y,ap
    }

proc move_bullets
    {
    if fired then fired--
    m=bs
    repeat bullets
	{
	bx=peek m:by=peekb (m+2):bd=peekb (m+3)
	put=0
	if (bx=-1) and (fired=0) and (key_press(78)) then
	    {
	    bx=x:by=y+2:bd=d
	    if not bd then bd=1
	    fired=6
	    }
	if bx<>-1 then
	    {
	    sprite bx,by,spy_bull
	    px=bx:py=by
	    put=1
	    bx+=peek (diri-4+bd*4)
	    by+=peek (diri-2+bd*4)
	    if bx<0 then bx=-1
	    if bx>620 then bx=-1
	    if by<0 then bx=-1
	    if by>190 then bx=-1
	    }
	if (bx=-1) and put then
	    {
	    sprite px,py+5,spy_null
	    }
	poke m,bx:pokeb m+2,by:pokeb m+3,bd
	m+=bsize
	}
    }

proc move_aliens
    {
    }


; Start game.

lives=3
score=0
setup_dots

while lives
    {
    setup
    killed=0:alive=10

    while alive
	{
	if killed then alive--
	if key_press(1) then killed=1

	move_you
	repeat 3000 {}
	move_bullets
	move_aliens
	update_dots
	}

    noise off
    dead
    }

abort

spy_dn:
datab 2,16
shape "                                "
shape "                                "
shape "    *****              *****    "
shape "    *   * ************ *   *    "
shape "    *   ****************   *    "
shape "    *   ***          ***   *    "
shape "    *   * *   ****   * *   *    "
shape "    *   * *   ****   * *   *    "
shape "    *   * *    **    * *   *    "
shape "    *   *****  **  *****   *    "
shape "    *   **   ******   **   *    "
shape "    *   *      **      *   *    "
shape "    *****      **      *****    "
shape "               **               "
shape "                                "
shape "                                "

spy_up:
datab 2,16
shape "                                "
shape "                                "
shape "               **               "
shape "    *****      **      *****    "
shape "    *   *      **      *   *    "
shape "    *   **   ******   **   *    "
shape "    *   *****  **  *****   *    "
shape "    *   * *    **    * *   *    "
shape "    *   * *   ****   * *   *    "
shape "    *   * *   ****   * *   *    "
shape "    *   ***          ***   *    "
shape "    *   ****************   *    "
shape "    *   * ************ *   *    "
shape "    *****              *****    "
shape "                                "
shape "                                "

spy_le:
datab 2,16
shape "                                "
shape "                                "
shape "                                "
shape "      ** *** *** *** *** ***    "
shape "      *                    *    "
shape "      *** *** *** *** *** **    "
shape "            *** ** *******      "
shape "           **       *  ****     "
shape "    ******************   **     "
shape "           **       *  ****     "
shape "            *** ** *******      "
shape "      ** *** *** *** *** ***    "
shape "      *                    *    "
shape "      **** *** *** *** *** *    "
shape "                                "
shape "                                "

spy_ri:
datab 2,16
shape "                                "
shape "                                "
shape "                                "
shape "    *** *** *** *** *** **      "
shape "    *                    *      "
shape "    ** *** *** *** *** ***      "
shape "      ******* ** ***            "
shape "     ****  *       **           "
shape "     **   ******************    "
shape "     ****  *       **           "
shape "      ******* ** ***            "
shape "    *** *** *** *** *** **      "
shape "    *                    *      "
shape "    * *** *** *** *** ****      "
shape "                                "
shape "                                "

spy_bull:
datab 2,13
shape "                                "
shape "                                "
shape "                                "
shape "                                "
shape "                                "
shape "              **                "
shape "          ** **** **            "
shape "              **                "
shape "                                "
shape "                                "
shape "                                "
shape "                                "
shape "                                "

spy_null:
datab 2,3
shape "                                "
shape "                                "
shape "                                "

diri:
data 0,-5
data 8,-4
data 10,0
data 8,4
data 0,5
data -8,4
data -10,0
data -8,-4
