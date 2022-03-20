;==============================================================================
;MISSILE VGA 320*200 256 colour version
;==============================================================================

;To do...
; Computer intelligence.
; Move base.

const height=200

const max_clouds=5,max_dots=1200
const cloud_len=5,dot_len=12
const bx1=28,bx2=292,computer=1,human=0
const sa=height-29,sf=height-13,windforce=150
const key_repeat=3

var by1,by2,x,rad,changing,active,volx
var s
var dx,dy,dotxi,dotyi,score1,score2,first_dead,dead
var compx,compy,p2

var32 x1,x2,y1,y2,xi1,xi2,yi1,yi2
var32 dotx,doty,wind_on_dot

cloud_data     ? max_clouds*cloud_len
dot_data       ? max_dots*dot_len
dots_available ? max_dots

n=timer mod 200
randomize n

#include keys.fi
#include vga.fi
set_keys

proc abort
    {
    reset_keys
    screen 3
    terminate
    }

;==============================================================================

;colour codes
const colour_off=0
const colour_scale=1
const colour_line=2
const colour_land=3
const colour_box=4
const colour_missile=5
const colour_dot=6	;not used
const colour_base=1
const colour_tree=2
const colour_hut=3
const colour_cloud=24
const colour_angle=5
const colour_force=6

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

fix_sprite(base_sprite,colour_base)
fix_sprite(sprite_tree,colour_tree)
fix_sprite(sprite_hut,colour_hut)
fix_sprite(clear_sprite,colour_off)
fix_sprite(cloud_sprite,colour_cloud)
fix_sprite(cloud_sprite+82,colour_cloud+4)
fix_sprite(sprite_angle,colour_angle)

;==============================================================================

proc noise_fire
    {
    for nf=3400 to 8000 step 15
    noise 20,nf
    next nf
    noise off
    }

;==============================================================================

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
    colour 15
    cursor 0,0:print bios score1;
    cursor 0,74:print bios score2;
    }

proc put_scale(psx)
    {
    vsprite(psx,height-20,sprite_angle)
    }

proc draw_scales
    {
    put_scale(bx1-20)
    put_scale(bx2-20)
    }

proc phigh(hx,hy)
    {
    if rnd>-5000 then return
    if (hx=volx) or (hx<0) or (hx>639) or (hy>=height) or (hy<120) then return
    for phy=hy-10 to hy
      if vpoint(hx,phy) then goto clear_top
    next phy
    return
    clear_top:
    vplot(hx,phy,colour_off)
    }

proc cls_sp(cspx,cspy)
    {
    cspx-=4
    if cspx<0 then cspx=0
    vsprite(cspx,cspy,clear_sprite)
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
    vsprite(px-15,py-9,base_sprite)
    }

proc draw_base
    {
    drawb(bx1,by1)
    drawb(bx2,by2)
    }

proc lline(yto)
    {
    for yto=yto to height-1

    dark=(yto*16)/height
    c=peekb (land_table+(rnd mod dark))
    vplot(x,yto,c)

    next yto
    }

proc landscape
    {
    lowest=height
    const lbot=60,ltop=height-30
    by1=(rnd mod 105)+45
    for x=0 to 59:lline(by1):next x

    y=by1:yi=(rnd mod 401)-200
    ylow=0
    lastth=0

    volx=0:voly=height-30
    for x=60 to 259
    lx=rnd
    lline(y)

    if (y<=lbot) and (x>80) and (x<239) then
	{
	if (y<voly) or ((y<=voly) and (rnd>27500)) then
	    {
	    volx=x:voly=y
	    vol_speed=-2000
	    vplot(x,voly+2,colour_land)
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
	    vsprite(x-7,y-10,sprite_tree):lastth=16
	    goto nextth
	    }
	if (yi<0) and (yi>-8) then
	    {
	    vsprite(x-7,y-7,sprite_hut):lastth=16
	    }
	nextth:
	}

    if yi<-600 then yi+=250
    if yi>600 then yi-=250
    if (x>250) and (y>ltop) and (yi>-200) then yi-=200
    next x

    if y>ltop then y=ltop
    by2=y
    for x=260 to 319:lline(by2):next x
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
		cx=3:if wind<0 then cx=319
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
	    if (cx<3) or (cx>315) then cls_sp(cx,cy):cx=0:goto store_cloud
	    vsprite(cx,cy,cloud_sprite+cs*82)
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
    compx=100:compy=by2+20
    last_fire=0
    }

proc fbox(bx,by,ba)
    {
    bx-=12
    ba=(ba*100)/231	;74 = 100% of 32

    for by=by to by+2
    c=ba:fc=colour_force
    for x=bx to bx+31
	if c then c--
	if c=0 then fc=colour_off
	vplot(x,by,fc)
    next x
    next by
    }

proc aflines
    {
    fbox(bx1,height-17,angle1)
    fbox(bx1,height-10,force1)

    fbox(bx2,height-17,angle2)
    fbox(bx2,height-10,force2)
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
	last_fire=200+(rnd and 255)
	noise_fire
	}
    }

function mis(sx,sy,misc)
    {
    sx-=2:sy--
    tpoint=0
    for ma=1 to 3
    v=vpoint(sx+ma,sy)
    if (v<>0) and ((v<72) or (v>87)) then tpoint=1
    vplot(sx+ma,sy,misc)
    v=vpoint(sx+ma,(sy+2))
    if (v<>0) and ((v<72) or (v>87)) then tpoint=1
    vplot(sx+ma,sy+2,misc)
    next ma

    for ma=0 to 4
    v=vpoint(sx+ma,(sy+1))
    if (v<>0) and ((v<72) or (v>87)) then tpoint=1
    vplot(sx+ma,sy+1,misc)
    next ma

    return tpoint
    }

proc put_dot(pdx,pdy)
    {
    pdm=dot_data
    vplot(pdx,pdy,colour_off)
    dv=searchb max_dots from dots_available for 0
    if dv then
	{
	dv-=dots_available
	set_dot(dv,1)
	pdm=dot_data+dv*dot_len
	poke pdm+2,pdx:poke pdm+6,pdy
	poke pdm+8,(rnd mod 1401)-700
	poke pdm+10,rnd mod 800
	}
    }

proc crater(cx,cy,p)
    {
    for cx=cx-10 to cx+10

    for ca=cy-8 to cy+4
    put_dot(cx,ca)
    next ca

    if cy>40 then
	{
	for cry=cy-(rnd and 15)-20 to cy-3
	if vpoint(cx,cry) then put_dot(cx,cry)
	if rnd>20000 then put_dot(cx,cry)
	next cry
	}

    next cx
    }

proc volcanoe
    {
    if volx then
	{
	if (vpoint(volx,(voly+1))) and (voly<185) then
	    {
;	    if (active<>0) and (rnd<vol_speed) then put_dot(volx,voly)
	    put_dot(volx,voly)
;	    if rnd>32600 then active=not active
	    }
	else if voly<185 then voly++
	}
    }

proc draw_dots
    {
    m=dot_data
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
	    if (dy>=0) then vplot(high dotx,dy,colour_off)
	    dotx+=wind_on_dot
	    if dotxi>0
	      then dotx+=dotxi*256
	      else dotx-=(65536-dotxi)*256
	    if dotyi>0
	      then doty-=dotyi*256
	      else doty+=(65536-dotyi)*256

	    dotyi-=10
	    dx=high dotx:dy=high doty
	    if (dx<0) or (dx>319) or (dy>(height-10)) then goto clear_dot

	    if (dx<>0) and (dy>=0) then
		{
		v=vpoint(dx,dy)
		if (v<>0) and ((v<72) or (v>87)) then
		    {
		    clear_dot:
		    phigh(dx,dy)
		    dotx=0:dx=0:set_dot(dot_count,0)
		    }
		}
	    if (dx<>0) and (dy>=0) then
		{
		vplot(dx,dy,72+(dot_count and 15))
		}

	    poke m,low dotx:poke m+2,high dotx
	    poke m+4,low doty:poke m+6,high doty
	    poke m+8,dotxi
	    poke m+10,dotyi

	    }
	else set_dot(dot_count,0)	;fix zero dots
	m+=dot_len
    next dot_count
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
	    a=70+(rnd mod 11)
	    xi1*=a
;	    xi1=force1*(90-angle1)*(70+(rnd mod 11))
	    yi1=angle1*force1*50
	    noise_fire
	    }
	if (high x2=0) and (key_press(80)) then
	    {
	    x2=bx2*65536
	    y2=(by2-11)*65536
	    xi2=90-angle2
	    xi2*=force2
	    a=70+(rnd mod 11)
	    xi2*=a
;	    xi2=force2*(90-angle2)*(70+(rnd mod 11))
	    yi2=angle2*force2*50
	    noise_fire
	    }
	}

    if high x1 then
	{
	idy=high y1
	if idy>0 then touch=mis(high x1,high y1,colour_off)
	x1+=xi1
	iwi=(windforce+idy)*iwx
	if idy>0 then if wind<0 then x1-=iwi else x1+=iwi
	y1-=yi1
	yi1-=2150
	idx=high x1:idy=high y1
	if (idx<3) or (idx>315) or (idy>(height-10)) then x1=0
	if idy>(height-25) then
	    {
	    if (idx<60) or (idx>260) then x1=0
	    }
	if high x1 then
	    {
	    touch=0
	    if idy>0 then touch=mis(idx,idy,colour_missile)
	    if touch then
		{
		touch=mis(idx,idy,colour_off)
		x1=0
		crater(idx,idy,1)
		}
	    }
	}

    if high x2 then
	{
	idy=high y2
	if idy>0 then touch=mis(high x2,high y2,colour_off)
	x2-=xi2
	iwi=(windforce+idy)*iwx
	if idy>0 then if wind<0 then x2-=iwi else x2+=iwi
	y2-=yi2
	yi2-=2300
	idx=high x2:idy=high y2
	compx=idx:compy=idy
	if (idx<3) or (idx>315) or (idy>(height-10)) then x2=0
	if idy>(height-25) then
	    {
	    if (idx<60) or (idx>260) then x2=0
	    }
	if high x2 then
	    {
	    touch=0
	    if idy>0 then touch=mis(idx,idy,colour_missile)
	    if touch then
		{
		touch=mis(idx,idy,colour_off)
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
	if not vpoint(x,(by1-2)) then first_dead=1
	if not vpoint(x+(bx2-bx1),(by2-2)) then first_dead=2
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
    vscreen

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

colour 15
cursor 0,13:print bios "NUCLEAR ATTACK"
cursor 1,13:print bios "PLAYERS? (1/2)"

forever
    {
    if key_press(2) then p2=computer:goto begin_play
    if key_press(3) then p2=human:goto begin_play
    if key_press(1) then abort
    draw_clouds
    draw_dots
    volcanoe
    change_wind
    delay(60)
    }

begin_play:
cursor 0,13:print bios "              "
cursor 1,13:print bios "              "

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
    delay(15)
    }

;Continue screen display for a short while.
repeat 180
    {
    playing
    draw_clouds
    draw_dots
    volcanoe
    change_wind
    delay(60)
    }

if dead=1 then score2+=500 else score1+=500
pscores

setup
goto next_landscape

base_sprite:
datab 32,10
datab '              **                '
datab '           ********             '
datab '     *********************      '
datab '     *** * * * * * * * ***      '
datab '    **** * * * * * * * ****     '
datab '   **** ***         *** ****    '
datab '  **** ****************** ****  '
datab '  ****************************  '
datab '  ****************************  '
datab '  ****************************  '

sprite_tree:
datab 16,12
datab '     * * *      '
datab '     *  * **    '
datab '   ** *  * **   '
datab '    *  ** *     '
datab '     *****  *   '
datab '       ** **    '
datab '    ******      '
datab '      ***       '
datab '      ***       '
datab '      ***       '
datab '     ****       '
datab '     ** ***     '

sprite_hut:
datab 16,8
datab '     **         '
datab '   **********   '
datab ' ************** '
datab '  ************  '
datab '  ***   **   *  '
datab '  ***   **   *  '
datab '  ***   ******  '
datab '  ***   ******  '

clear_sprite:
datab 32,7
datab '                                '
datab '                                '
datab '                                '
datab '                                '
datab '                                '
datab '                                '
datab '                                '

cloud_sprite:
datab 16,5
datab '     *******    '
datab '    *********   '
datab '     *******    '
datab '       ******   '
datab '                '

datab 20,5
datab '       **** ***     '
datab '     ***********    '
datab '    ************    '
datab '     **********     '
datab '      ********      '

sprite_angle:
datab 41,16
datab '*****************************************'
datab '*                                       *'
datab '*  ***                                  *'
datab '* *   *                                 *'
datab '* *****                                 *'
datab '* *   *                                 *'
datab '* *   *                                 *'
datab '*                                       *'
datab '*                                       *'
datab '* *****                                 *'
datab '* *                                     *'
datab '* ***                                   *'
datab '* *                                     *'
datab '* *                                     *'
datab '*                                       *'
datab '*****************************************'

land_table:
datab 195,194,193,192,121,120,119,118,117
datab 50,49,48,47,46,45,10
