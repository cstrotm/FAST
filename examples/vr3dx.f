
;==============================================================================
; 3D display routines : vga 320*200 256 colours
;==============================================================================

#include vga.fi
#include sincos.fi
#short

var32 calcx,calcy,calcz
var32 n32,m32,a32,b32
var32 x32,y32,z32
var rotatex,rotatey,rotatez
var lsx,lsy,zoom,zfactor

;== procedures ================================================================

function screen_coordinate(cx,cy,cz)
    {
    sx=0
    sy=0

    #if 1
    cosz=cos(rotatez)
    z32=imul(cz,cosz)
    if (high z32) and 8000h then
	{
	z32=0-z32
	z32/=32768
	cz=0-(low z32)
	}
    else
	{
	z32/=32768
	cz=low z32
	}
    #endif

    cz+=zfactor     ;force zoom to positive

    cx=imul(cx,cz)
    cx=idiv(cx,4)

    cy=imul(cy,cz)
    cy=idiv(cy,4)

    cosx=cos(rotatex)
    cosy=cos(rotatey)
    cosz=cos(rotatez)

    sinx=sin(rotatex)
    siny=sin(rotatey)
    sinz=sin(rotatez)

    x32=imul(cx,cosx):sx+=high x32
    y32=imul(cx,sinx):sy+=high y32

    x32=imul(cy,cosy):sy+=high x32
    y32=imul(cy,siny):sx+=high y32

    x32=imul(cz,cosz):sx-=high x32
    y32=imul(cz,cosz):sy+=high y32

    sx+=160
    sy+=100
    }

;==============================================================================

proc draw_object(doo,doc)
    {
    colour doc
    lsx=-1

    while peek doo
	{
	x=peek doo

	if x=-1 then
	    {
	    lsx=-1
	    goto next_line
	    }

	y=peek (doo+2)
	zoom=peek (doo+4)

	screen_coordinate(x,y,zoom)

	if sx<0 then sx=0
	if sx>319 then sx=319
	if sy<0 then sy=0
	if sy>199 then sy=199

	if lsx<>-1 then
	    {
	    vga_line lsx,lsy to sx,sy
	    }

	lsx=sx
	lsy=sy

	next_line:
	doo+=6
	}
    }

;== start =====================================================================

vscreen

zfactor=20
rotatex=0:rotatexi=0
rotatey=0:rotateyi=0
rotatez=0:rotatezi=0
col=32:count=0

forever
    {
    draw_object(object,col)

    if count then count--
    else
	{
	col++:if col>127 then col=32
	count=20
	}

    #if 0
    cursor 0,0
    print bios "x=";x;" y=";y;" zf=";zfactor;" rx=";rotatex;" ry=";rotatey;
    cursor 1,0
    print bios "sx=";sx;" sy=";sy;
    #endif

;   draw_object(object,0)

    rotatex+=rotatexi
    rotatey+=rotateyi
    rotatez+=rotatezi

    kscan=scan
    if kscan=1 then goto stop_vr3d
    if kscan=75 then rotatexi--
    if kscan=77 then rotatexi++
    if kscan=72 then rotateyi--
    if kscan=80 then rotateyi++

    if kscan=30 then rotatezi--
    if kscan=44 then rotatezi++

    if kscan=74 then zfactor--:if zfactor<20 then zfactor=20
    if kscan=78 then zfactor++:if zfactor>500 then zfactor=500
    }

stop_vr3d:
screen 3
stop

;==============================================================================

object:
data 10,10,12
data 10,10,-12
data -10,10,-12
data -10,-10,-12
data -10,-10,12
data 10,-10,12
data 10,10,12
data -10,10,12
data -10,-10,12
data -1,0,0	    ;stop
data 10,-10,12
data 10,-10,-12
data 10,10,-12
data -1,0,0	    ;stop
data 10,-10,-12
data -10,-10,-12
data -1,0,0	    ;stop
data -10,10,12
data -10,10,-12
data 0,0,0	    ;end
