;MISSILE
video=0b800h

;To do...
; Computer intelligence.
; Move base.

const max_clouds=5,max_dots=250
const cloud_len=5,dot_len=12
const bx1=35,bx2=605,computer=1,human=0
const sa=176,sf=192,windforce=150
const key_repeat=3
var by1,by2,x,rad,changing,active,volx
var lasta1,lasta2,lastf1,lastf2,s
var dx,dy,dotxi,dotyi,score1,score2,first_dead,dead
var compx,compy,p2
var32 x1,x2,y1,y2,xi1,xi2,yi1,yi2
var32 dotx,doty,wind_on_dot

cloud_data     ? max_clouds*cloud_len
dot_data       ? max_dots*dot_len
dots_available ? max_dots


n=timer mod 200
randomize n
land=rnd and 3

#include keys.fi
set_keys

proc delay(dc)
    {
    repeat dc repeat 1000 {}
    }

proc set_dot(dp,dvalue)
    {
    pokeb dots_available+dp,dvalue
    }

proc set_dots
    {
    fillb max_dots from dots_available with 0
    m=dot_data
    repeat max_dots
	{
	poke m+2,0
	m+=dot_len
	}
    }

proc pscores
    {
    cursor 0,0:print bios score1;
    cursor 0,74:print bios score2;
    }

proc put_scale(psx)
    {
    colour 0
    for x=psx to psx+83
    for y=165 to 197
    plot x,y
    next y,x
    colour 1

    for x=psx+4 to psx+79
    plot x,sf+3
    plot x,sf-1
    plot x,sa+3
    plot x,sa-1
    next x

    for y=0 to 2
    plot psx+4,y+sa
    plot psx+79,y+sa
    plot psx+4,y+sf
    plot psx+79,y+sf
    next y

    sprite psx+25,sa-8,sprite_angle
    sprite psx+25,sf-8,sprite_force
    }

proc draw_scales
    {
    put_scale(bx1-32)
    put_scale(bx2-52)
    }

proc phigh(hx,hy)
    {
    if rnd>-5000 then return
    if (hx=volx) or (hx<0) or (hx>639) or (hy>199) or (hy<20) then return
    for phy=hy-10 to hy
      if point hx,phy then goto clear_top
    next phy
    return
    clear_top:
    colour 0
    plot hx,phy
    colour 1
    }

proc abort
    {
    reset_keys
    screen 3
    terminate
    }

proc cls_sp(cspx,cspy)
    {
    cspx-=4
    if cspx<0 then cspx=0
    sprite cspx,cspy,clear_sprite
    }

proc set_speed
    {
    speed+=(rnd mod 151)-75
    if speed<-600 then speed=-600
    if speed>600  then speed=600
    }

proc set_wind
    {
    wind=(rnd mod 1201)-600
    set_speed
    }

proc change_wind
    {
    if changing then changing--
    else
	{
	changing=25
	if wind<speed then wind++
	if wind>speed then wind--
	if wind=speed then set_speed
	}
    }

proc set_clouds
    {
    m=cloud_data
    repeat max_clouds
	{
	poke m,0
	m+=cloud_len
	}
    }

proc drawb(px,py)
    {
    sprite px-15,py-9,base_sprite
    }

proc draw_base
    {
    drawb(bx1,by1)
    drawb(bx2,by2)
    }

proc lline(yto)
    {
    dot=2
    for yto=yto to 199
    if dot then
	{
	dot--
	if rnd<0 then plot x,yto
	}
    else
	{
	pp=1
	if land=1 then if (x xor yto) and 1 then pp=0
	if land=2 then if yto and 1 then pp=0
	if land=3 then pp=rnd and 7
	if pp then plot x,yto
	}
    next yto
    }

proc landscape
    {
    colour 1
    lowest=200
    const lbot=64,ltop=190
    by1=(rnd mod 100)+62
    for x=0 to 69:lline(by1):next x

    y=by1:yi=(rnd mod 401)-200
    ylow=0
    lastth=0

    volx=0:voly=200
    for x=70 to 569
    lx=rnd
    lline(y)

    if (y<73) and (x>90) and (x<549) then
	{
	if (y<voly) or ((y<=voly) and (rnd>27500)) then
	    {
	    volx=x:voly=y
	    vol_speed=(rnd mod 6000)-28000
	    vol_depth=(rnd mod 8)+2
	    plot x,voly+2
	    active=0
	    lastth=10 ; No more huts/trees for 10 dots.
	    }
	}

    if y<lowest then lowest=y
    ylow+=yi
    if ylow<-255 then
	{
	y-=(0-ylow)/255
	ylow=0
	}
    if ylow>255 then
	{
	y+=ylow/255
	ylow=0
	}
    if y<lbot then y=lbot:yi+=300
    if y>ltop then y=ltop:yi-=300

    yi+=(lx/550)-59

    if lastth then lastth--
    else
	{
	if (yi>0) and (yi<13) then
	    {
	    sprite x-7,y-10,sprite_tree:lastth=16
	    goto nextth
	    }
	if (yi<0) and (yi>-8) then
	    {
	    sprite x-7,y-7,sprite_hut:lastth=16
	    }
	nextth:
	}

    if yi<-600 then yi+=250
    if yi>600 then yi-=250
    if (x>550) and (y>161) and (yi>-200) then yi-=200
    next x

    if y>161 then y=161
    by2=y
    for x=570 to 639:lline(by2):next x
    lowest-=15
    }

proc draw_clouds
    {
    m=cloud_data
    clw=lowest-8
    repeat max_clouds
	{
	cx=peek m:cxl=peekb (m+2)
	cy=peekb (m+3)
	cs=peekb (m+4)
	if not cx then
	    {
	    cxx=rnd
	    if cxx>32000 then
		{
		cy=cxx mod clw:cy+=8
		cx=3:if wind<0 then cx=610
		cs=rnd and 1
		}
	    }
	if cx then
	    {
	    if wind<-255 then cx--
	    if wind>255 then cx++
	    cxl+=(wind and 255)+cs*128
	    if cxl>255 then
		{
		if wind>0 then cx+=cxl/255 else cx-=cxl/255
		cxl=0
		}
	    if (cx<3) or (cx>610) then cls_sp(cx,cy):cx=0:goto store_cloud
	    sprite cx,cy,cloud_sprite+cs*12
	    }
	store_cloud:
	poke m,cx:pokeb m+2,cxl
	pokeb m+3,cy
	pokeb m+4,cs
	m+=cloud_len
	}
    }

proc setup_players
    {
    x1=0:x2=0
    angle1=40:angle2=40
    force1=25:force2=25
    lasta1=0:lasta2=0
    lastf1=0:lastf2=0
    compx=100:compy=by2+20
    last_fire=0
    }

proc fbox(bx,by)
    {
    if bx and 1 then
	{
	plot bx,by
	plot bx,by+2
	}
    else plot bx,by+1
    }

proc aflines
    {
    if angle1>lasta1 then
	{
	colour 1
	for afx=lasta1 to angle1:fbox(bx1-28+afx,sa):next afx
	}
    if angle1<lasta1 then
	{
	colour 0
	for afx=angle1+1 to lasta1:fbox(bx1-28+afx,sa):next afx
	}

    if angle2>lasta2 then
	{
	colour 1
	for afx=lasta2 to angle2:fbox(bx2-48+afx,sa):next afx
	}
    if angle2<lasta2 then
	{
	colour 0
	for afx=angle2+1 to lasta2:fbox(bx2-48+afx,sa):next afx
	}


    if force1>lastf1 then
	{
	colour 1
	for afx=lastf1 to force1:fbox(bx1-28+afx,sf):next afx
	}
    if force1<lastf1 then
	{
	colour 0
	for afx=force1+1 to lastf1:fbox(bx1-28+afx,sf):next afx
	}

    if force2>lastf2 then
	{
	colour 1
	for afx=lastf2 to force2:fbox(bx2-48+afx,sf):next afx
	}
    if force2<lastf2 then
	{
	colour 0
	for afx=force2+1 to lastf2:fbox(bx2-48+afx,sf):next afx
	}

    lasta1=angle1
    lasta2=angle2
    lastf1=force1
    lastf2=force2
    }

proc move_player(pn)
    {
    if pn=1 then
	{
	if key_press(59) then angle1--
	if key_press(60) then angle1++
	if key_press(61) then force1--
	if key_press(62) then force1++

	if angle1<0 then angle1=0
	if angle1>74 then angle1=74
	if force1<0 then force1=0
	if force1>74 then force1=74
	}
    else
	{
	if key_press(71) then angle2--
	if key_press(73) then angle2++
	if key_press(79) then force2--
	if key_press(81) then force2++

	if angle2<0 then angle2=0
	if angle2>74 then angle2=74
	if force2<0 then force2=0
	if force2>74 then force2=74
	}

    aflines
    }

proc move_computer
    {
    if (high x2=0) then
	{
	if last_fire then last_fire--:return

	if compy<lowest then
	    {
	    if compy<20 then force2-=4:angle2--
	    force2--
	    goto fire_computer
	    }
	if compx>(bx1+15) then
	    {
	    force2+=(compx-bx1)/30
	    if (angle2>38) and (by1<95) and (compy>by2)
		then angle2-=(compy-by2)/15
	    if compx<240
	    then angle2-=(rnd and 7)=4
	    else angle2+=2
	    }
	if compx<(bx1-7) then
	    {
	    if compy<by1
	    then force2-=(by1-compy)/9:angle2+=3
	    else angle2--
	    }

	fire_computer:

	if force2<0 then force2=0
	if force2>74 then force2=74
	if angle2<0 then angle2=0
	if angle2>74 then angle2=74

	x2=bx2*65536
	y2=(by2-11)*65536
	xi2=90-angle2
	xi2*=force2
	a=100+(rnd mod 11)
	xi2*=a
;	xi2=force2*(90-angle2)*(100+(rnd mod 11))
	yi2=angle2*force2*70
	last_fire=20
	}
    }

function mis(sx,sy)
    {
    sx-=2:sy--
    tpoint=0
    for ma=1 to 3
    if point sx+ma,sy then tpoint=1
    plot sx+ma,sy
    if point sx+ma,(sy+2) then tpoint=1
    plot sx+ma,sy+2
    next ma

    for ma=0 to 4
    if point sx+ma,(sy+1) then tpoint=1
    plot sx+ma,sy+1
    next ma

    return tpoint
    }

proc put_dot(pdx,pdy)
    {
    pdm=dot_data
    colour 0:plot pdx,pdy
    dv=searchb max_dots from dots_available for 0
    if dv then
	{
	dv-=dots_available
	set_dot(dv,1)
	pdm=dot_data+dv*dot_len
	poke pdm+2,pdx:poke pdm+6,pdy
	poke pdm+8,(rnd mod 1601)-800
	poke pdm+10,rnd mod 600
	}
    }

proc crater(cx,cy,p)
    {
    for cx=cx-4 to cx+4
    for ca=cy-2 to cy+3
    put_dot(cx,ca)
    next ca

    if cy>60 then
	{
	for cry=cy-(rnd and 15)-20 to cy-3
	if point cx,cry then put_dot(cx,cry)
	next cry
	}

    next cx
    }

proc volcanoe
    {
    if volx then
	{
	if (point volx,(voly+2)) and (voly<85) then
	    {
	    if (active<>0) and (rnd<vol_speed) then put_dot(volx,voly+vol_depth)
	    if rnd>32450 then active=not active
	    }
	if not point volx,(voly+2) then
	    {
	    voly++
	    vol_speed+=800
	    }
	}
    }

proc draw_dots
    {
    m=dot_data
    nlen=0
    if wind>0
      then wind_on_dot=wind*200
      else wind_on_dot=0-((65536-wind)*200)
    for dot_count=0 to max_dots-1
	dotx=peek m
	if high dotx then
	    {
	    doty=peek (m+4)
	    dotxi=peek (m+8)
	    dotyi=peek (m+10)

	    dy=high doty
	    if (dy>=0) then colour 0:plot high dotx,dy
	    dotx+=wind_on_dot
	    if dotxi>0
	      then dotx+=dotxi*256
	      else dotx-=(65536-dotxi)*256
	    if dotyi>0
	      then doty-=dotyi*256
	      else doty+=(65536-dotyi)*256

	    dotyi-=10
	    dx=high dotx:dy=high doty
	    if (dx<0) or (dx>639) or (dy>195) then goto clear_dot

	    if (dx<>0) and (dy>=0)
	      then if point dx,dy then
		{
		clear_dot:
		phigh(dx,dy)
		dotx=0:dx=0:set_dot(dot_count,0)
		}
	    if (dx<>0) and (dy>=0) then
		{
		colour 1:plot dx,dy
		nlen++
		}

	    poke m,low dotx:poke m+2,high dotx
	    poke m+4,low doty:poke m+6,high doty
	    poke m+8,dotxi
	    poke m+10,dotyi
	    }
	m+=dot_len
    next dot_count
    while nlen
	{
	noise 1,3000+(rnd and 4095)
	nlen--
	}
    noise off
    }

proc missiles
    {
    iwx=abs wind
    if not dead then
	{
	if (high x1=0) and (key_press(41)) then
	    {
	    x1=bx1*65536
	    y1=(by1-11)*65536
	    xi1=90-angle1
	    xi1*=force1
	    a=100+(rnd mod 11)
	    xi1*=a
;	    xi1=force1*(90-angle1)*(100+(rnd mod 11))
	    yi1=angle1*force1*70
	    }
	if (high x2=0) and (key_press(80)) then
	    {
	    x2=bx2*65536
	    y2=(by2-11)*65536
	    xi2=90-angle2
	    xi2*=force2
	    a=100+(rnd mod 11)
	    xi2*=a
;	    xi2=force2*(90-angle2)*(100+(rnd mod 11))
	    yi2=angle2*force2*70
	    }
	}

    if high x1 then
	{
	idy=high y1
	if idy>0 then colour 0:touch=mis(high x1,high y1)
	x1+=xi1
	iwi=(windforce+idy)*iwx
	if idy>0 then if wind<0 then x1-=iwi else x1+=iwi
	y1-=yi1
	yi1-=2150
	idx=high x1:idy=high y1
	if (idx<3) or (idx>635) or (idy>192) then x1=0
	if idy>164 then
	    {
	    if (idx<70) or (idx>570) then x1=0
	    }
	if high x1 then
	    {
	    touch=0
	    if idy>0 then colour 1:touch=mis(idx,idy)
	    if touch then
		{
		colour 0:touch=mis(idx,idy)
		x1=0
		crater(idx,idy,1)
		}
	    }
	}

    if high x2 then
	{
	idy=high y2
	if idy>0 then colour 0:touch=mis(high x2,high y2)
	x2-=xi2
	iwi=(windforce+idy)*iwx
	if idy>0 then if wind<0 then x2-=iwi else x2+=iwi
	y2-=yi2
	yi2-=2300
	idx=high x2:idy=high y2
	compx=idx:compy=idy
	if (idx<3) or (idx>635) or (idy>192) then x2=0
	if idy>164 then
	    {
	    if (idx<70) or (idx>570) then x2=0
	    }
	if high x2 then
	    {
	    touch=0
	    if idy>0 then colour 1:touch=mis(idx,idy)
	    if touch then
		{
		colour 0:touch=mis(idx,idy)
		x2=0
		crater(idx,idy,2)
		}
	    }
	}
    }

function alive
    {
    if not first_dead then
	{
	for x=bx1-3 to bx1+3
	if not point x,(by1-2) then first_dead=1
	if not point x+(bx2-bx1),(by2-2) then first_dead=2
	next x
	if first_dead then
	    {
	    if first_dead=2 then ax=bx2:ay=by2 else ax=bx1:ay=by1
	    for x=ax-14 to ax+14
	    for y=ay-9 to ay
	    put_dot(x,y)
	    next y,x
	    }
	}
    return first_dead
    }

proc setup
    {
    screen 6:colour 0:cls
    mode 640,and

    landscape
    draw_base
    set_wind
    set_clouds
    set_dots
    draw_scales

    pscores
    }

proc playing
    {
    if not dead then
	{
	move_player(1)
	if p2=computer then move_computer else move_player(2)
	}
    missiles
    }

start:
score1=0:score2=0
setup

cursor 0,26:print bios "N U C L E A R    A T T A C K"
cursor 3,26:print bios "1=ONE PLAYER   2=TWO PLAYERS"

forever
    {
    if key_press(2) then p2=computer:goto begin_play
    if key_press(3) then p2=human:goto begin_play
    if key_press(1) then abort
    }

begin_play:
cursor 0,26:print bios "                            "
cursor 3,25:print bios "                              "

next_landscape:
while keypressed k=key
first_dead=0
setup_players
dead=0

while not dead
    {
    if key_press(1) then abort

    playing
    draw_clouds
    draw_dots
    volcanoe
    change_wind
    dead=alive
    delay(20)
    }

repeat 320 ; Continue screen display for a short while.
    {
    playing
    draw_clouds
    draw_dots
    volcanoe
    change_wind
    }

if dead=1 then score2+=500 else score1+=500
pscores
old_land=land
while old_land=land land=rnd and 3 ;Get new land pattern.

setup
goto next_landscape

base_sprite:
datab 2,10
shape "              **                "
shape "           ********             "
shape "     *********************      "
shape "     *** * * * * * * * ***      "
shape "    **** * * * * * * * ****     "
shape "   **** ***         *** ****    "
shape "  **** ****************** ****  "
shape "  ****************************  "
shape "  ****************************  "
shape "  ****************************  "

sprite_tree:
datab 1,12
shape "     * * *      "
shape "     *  * **    "
shape "   ** *  * **   "
shape "    *  ** *     "
shape "     *****  *   "
shape "       ** **    "
shape "    ******      "
shape "      ***       "
shape "      ***       "
shape "      ***       "
shape "     ****       "
shape "     ** ***     "

sprite_hut:
datab 1,8
shape "     **         "
shape "   **********   "
shape " ************** "
shape "  ************  "
shape "  ***   **   *  "
shape "  ***   **   *  "
shape "  ***   ******  "
shape "  ***   ******  "

clear_sprite:
datab 2,7
shape "                                "
shape "                                "
shape "                                "
shape "                                "
shape "                                "
shape "                                "
shape "                                "

cloud_sprite:
datab 1,5
shape "     *******    "
shape "    **     **   "
shape "     **   **    "
shape "       *** **   "
shape "                "

datab 2,7
shape "       ****     ***********     "
shape "     ***  ********        **    "
shape "    **                    **    "
shape "     **                  **     "
shape "      **                **      "
shape "       *****     ****** ***     "
shape "           *******    ****      "

sprite_angle:
datab 2,5
shape "****  ****  ****  *     ****    "
shape "*  *  *  *  *     *     *       "
shape "****  *  *  *  *  *     ***     "
shape "*  *  *  *  *  *  *     *       "
shape "*  *  *  *  ****  ****  ****    "

sprite_force:
datab 2,5
shape "****  ****  ****  ****  ****    "
shape "*     *  *  *  *  *     *       "
shape "***   *  *  ****  *     ***     "
shape "*     *  *  * *   *     *       "
shape "*     ****  *  *  ****  ****    "
