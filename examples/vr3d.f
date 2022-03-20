
;==============================================================================
; 3D display routines : vga 320*200 256 colours
;==============================================================================

#include vga.fi
#include sincos.fi
#short

const object_fastbase=1
const object_box=1
const object_other=0

var32 calcx,calcy,calcz
var32 n32,m32,a32,b32
var32 x32,y32,z32

var32 cx32,cy32,cz32,sx32,sy32
var32 gcx32,gcy32,gcz32

var rotatex,rotatey,rotatez
var lsx,lsy,zoom,zfactor

var32 SINCOSM
SINCOSM=32768
SINCOSM=65536

;== procedures ================================================================

function screen_coordinate(x,y,z)
    {
    #if object_fastbase
    x-=530
    y-=75
    #endif

    z+=zfactor
    z-=400

    cosx=cos(rotatex)
    cosy=cos(rotatey)
    cosz=cos(rotatez)

    sinx=sin(rotatex)
    siny=sin(rotatey)
    sinz=sin(rotatez)

    cx32 = imul(cosx,x) 	    ;xz plane rotate
    sy32 = imul(sinx,z)
    sx32 = imul(sinx,x)
    cy32 = imul(cosx,z)

    gcx32 = cx32 + sy32
    gcx=high gcx32

    gcz32 = cy32 - sx32
    gcz=high gcz32

    cx32 = imul(cosy,gcz)     ;yz plane rotate
    sy32 = imul(siny,y)
    sx32 = imul(siny,gcz)
    cy32 = imul(cosy,y)

    gcz32 = cx32 + sy32
    gcz=high gcz32

    gcy32 = cy32 - sx32
    gcy=high gcy32

    cx32 = imul(cosz,gcx)     ;xy plane rotate
    sy32 = imul(sinz,gcy)
    sx32 = imul(sinz,gcx)
    cy32 = imul(cosz,gcy)

    gcx32 = cx32 + sy32
    gcx=high gcx32

    gcy32 = cy32 - sx32
    gcy=high gcy32

;   gcx=idiv(gcx,2)
;   gcy=idiv(gcy,2)

    sx=160+gcx
    sy=100+gcy
    }

;==============================================================================

proc draw_object(doo,doc)
    {
    colour doc
    lsx=-1

    while peek doo or peek (doo+2)
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

zfactor=0
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
    printh bios "xx=";x;" yy=";y;" zf=";zfactor
    printh bios "rx=";rotatex;" ry=";rotatey;" rz=";rotatez;
    #endif

    halt
    wait for keypressed or rotatexi or rotateyi or rotatezi
    kscan=scan

    draw_object(object,0)

    rotatex+=rotatexi
    rotatey+=rotateyi
    rotatez+=rotatezi

    if kscan=1 then goto stop_vr3d
    if kscan=75 then rotatexi--
    if kscan=77 then rotatexi++
    if kscan=72 then rotateyi--
    if kscan=80 then rotateyi++

    if kscan=30 then rotatezi--
    if kscan=44 then rotatezi++

    if kscan=74 then zfactor--
    if kscan=78 then zfactor++
    }

stop_vr3d:
screen 3
stop

;==============================================================================

object:
#if object_box
data 200,200,200
data 200,200,150
data -200,200,150
data -200,-200,150
data -200,-200,200
data 200,-200,200
data 200,200,200
data -200,200,200
data -200,-200,200
data -1,0,0	    ;stop
data 200,-200,200
data 200,-200,150
data 200,200,150
data -1,0,0	    ;stop
data 200,-200,150
data -200,-200,150
data -1,0,0	    ;stop
data -200,200,200
data -200,200,150

data -1,0,0	    ;stop

data 100,100,100
data 100,100,20
data -100,100,20
data -100,-100,20
data -100,-100,100
data 100,-100,100
data 100,100,100
data -100,100,100
data -100,-100,100
data -1,0,0	    ;stop
data 100,-100,100
data 100,-100,20
data 100,100,20
data -1,0,0	    ;stop
data 100,-100,20
data -100,-100,20
data -1,0,0	    ;stop
data -100,100,100
data -100,100,20
#endif

data -1,0,0	    ;stop

#if object_fastbase
data -270, 90, 370
data -270, 90, 160
data -240, 90, 160
data -240, 90, 250
data -159, 90, 250
data -159, 90, 271
data -240, 90, 271
data -240, 90, 346
data -138, 90, 346
data -138, 90, 370
data -102, 90, 160
data -42, 90, 370
data 36, 90, 370
data 90, 90, 160
data 57, 90, 160
data 36, 90, 250
data -42, 90, 250
data -69, 90, 160
data -36, 90, 274
data 27, 90, 274
data 0, 90, 346
data 132, 90, 160
data 276, 90, 160
data 306, 90, 196
data 276, 90, 238
data 171, 90, 238
data 168, 90, 268
data 171, 90, 331
data 219, 90, 349
data 282, 90, 349
data 285, 90, 373
data 138, 90, 367
data 120, 90, 307
data 129, 90, 238
data 162, 90, 196
data 243, 90, 211
data 261, 90, 187
data 141, 90, 196
data 333, 90, 373
data 504, 90, 373
data 510, 90, 349
data 435, 90, 331
data 435, 90, 163
data 396, 90, 163
data 393, 90, 328
data 342, 90, 331
data 540, 90, 373
data 528, 90, 166
data 666, 90, 166
data 696, 90, 205
data 684, 90, 250
data 657, 90, 271
data 591, 90, 265
data 672, 90, 286
data 693, 90, 331
data 651, 90, 370
data 570, 90, 343
data 564, 90, 280
data 636, 90, 301
data 639, 90, 334
data 561, 90, 244
data 564, 90, 187
data 645, 90, 196
data 651, 90, 220
data 627, 90, 250
data 728, 90, 160
data 788, 90, 370
data 866, 90, 370
data 920, 90, 160
data 887, 90, 160
data 866, 90, 250
data 788, 90, 250
data 761, 90, 160
data 794, 90, 274
data 857, 90, 274
data 830, 90, 346
data 962, 90, 160
data 1106, 90, 160
data 1136, 90, 196
data 1106, 90, 238
data 1001, 90, 238
data 998, 90, 268
data 1001, 90, 331
data 1049, 90, 349
data 1112, 90, 349
data 1115, 90, 373
data 968, 90, 367
data 950, 90, 307
data 959, 90, 238
data 992, 90, 196
data 1073, 90, 211
data 1091, 90, 187
data 971, 90, 196
data 1158, 90, 370
data 1161, 90, 163
data 1329, 90, 157
data 1326, 90, 184
data 1194, 90, 187
data 1203, 90, 256
data 1311, 90, 250
data 1314, 90, 280
data 1209, 90, 283
data 1194, 90, 343
data 1311, 90, 346
data 1329, 90, 373
data -270, 60, 370
data -270, 60, 160
data -240, 60, 160
data -240, 60, 250
data -159, 60, 250
data -159, 60, 271
data -240, 60, 271
data -240, 60, 346
data -138, 60, 346
data -138, 60, 370
data -102, 60, 160
data -42, 60, 370
data 36, 60, 370
data 90, 60, 160
data 57, 60, 160
data 36, 60, 250
data -42, 60, 250
data -69, 60, 160
data -36, 60, 274
data 27, 60, 274
data 0, 60, 346
data 132, 60, 160
data 276, 60, 160
data 306, 60, 196
data 276, 60, 238
data 171, 60, 238
data 168, 60, 268
data 171, 60, 331
data 219, 60, 349
data 282, 60, 349
data 285, 60, 373
data 138, 60, 367
data 120, 60, 307
data 129, 60, 238
data 162, 60, 196
data 243, 60, 211
data 261, 60, 187
data 141, 60, 196
data 333, 60, 373
data 504, 60, 373
data 510, 60, 349
data 435, 60, 331
data 435, 60, 163
data 396, 60, 163
data 393, 60, 328
data 342, 60, 331
data 540, 60, 373
data 528, 60, 166
data 666, 60, 166
data 696, 60, 205
data 684, 60, 250
data 657, 60, 271
data 591, 60, 265
data 672, 60, 286
data 693, 60, 331
data 651, 60, 370
data 570, 60, 343
data 564, 60, 280
data 636, 60, 301
data 639, 60, 334
data 561, 60, 244
data 564, 60, 187
data 645, 60, 196
data 651, 60, 220
data 627, 60, 250
data 728, 60, 160
data 788, 60, 370
data 866, 60, 370
data 920, 60, 160
data 887, 60, 160
data 866, 60, 250
data 788, 60, 250
data 761, 60, 160
data 794, 60, 274
data 857, 60, 274
data 830, 60, 346
data 962, 60, 160
data 1106, 60, 160
data 1136, 60, 196
data 1106, 60, 238
data 1001, 60, 238
data 998, 60, 268
data 1001, 60, 331
data 1049, 60, 349
data 1112, 60, 349
data 1115, 60, 373
data 968, 60, 367
data 950, 60, 307
data 959, 60, 238
data 992, 60, 196
data 1073, 60, 211
data 1091, 60, 187
data 971, 60, 196
data 1158, 60, 370
data 1161, 60, 163
data 1329, 60, 157
data 1326, 60, 184
data 1194, 60, 187
data 1203, 60, 256
data 1311, 60, 250
data 1314, 60, 280
data 1209, 60, 283
data 1194, 60, 343
data 1311, 60, 346
data 1329, 60, 373
#endif

data -1,0,0	    ;stop

#if object_other
data -250,-250,600
data -250,100,600
data -250,100,200
data -250,500,180
data 250,500,180
data 250,100,200
data 250,-250,200
data -1,0,0
data -250,100,600
data 250,100,600
data 250,-250,600
data -1,0,0
data 250,100,600
data 250,100,200
data -250,100,200
data -250,-250,200
#endif

data 0,0,0	    ;end
