
;==============================================================================
;	    FastBase Menu Program, written by Peter Campbell
;	     (c) The FastBase Development Company Ltd 1993.
;==============================================================================
;
;FBMENU.M file strucutre:
;
;M [colour,] Menu Title 		(menu)
;   I [security,] Item Name		(item)
;	D command_line			(dos command)
;	S menu number (0,1,2 ... )	(sub-menu)
;	X				(dos prompt)
;	G length,prompt 		(get response)
;
;F [security,] define function key (same as item definition), includes D, S & X.
;
;FBMENU uses the Multiplexer interupt to test if the menu is already loaded.
;To restart the menu enter "FBMENU" - the original menu will be displayed.
;
;FBMENU intercepts INT 21h to check for 'enter a string' (function 0Ah).
;The commands in the fbmenu.m file can also be used in programs like DEBUG
;which use this DOS function for keyboard input.
;
;Help: Press ALT-S for screen saver (normally takes 10 minutes).
;      Press ALT-X to deactivate fbmenu, next load of fbmenu will start new,
;      however the old copy will remain in memory (8K approx).
;

#short
#errors off
#include \fast\cmd_line.fi

const max_menus=7
const time_out=10
const menu_width=40
var menu_level,menu_pos,mseg,sy,menul,menun
var offset,segment,menu_security,fbmenu_flag
var functions,option,cls_screen
var responses

const max_response=3
response ? max_response*50

menuy ? max_menus
menu_no ? max_menus

back_colour=7
top_colour=120
top_colour2=120
error_colour=15
default_menu=120
scroll_colour=15
f1_colour=120
f2_colour=15

;== some procedures ===========================================================

function skip_blank(sb)
    {
    while (mseg[sb]b=' ') or (mseg[sb]b=9) or (mseg[sb]b=13) or (mseg[sb]b=10) sb++
    return sb
    }

function get_number(m)
    {
    n=0
    get_digit:
    c=mseg[m]b
    if (c>='0') and (c<='9') then
	{
	n=n*10+(c-'0')
	m++
	goto get_digit
	}
    if n then m++	;skip comma
    return n
    }

function get_item(gim,gin,code)
    {
    forever
	{
	gim=skip_blank(gim)
	c=mseg[gim]b
	if c=26 then return 0
	if c=code then
	    {
	    if (c='I') or (c='F') then
		{
		if menu_security<get_number(gim+1) then goto skip_item_level
		else gim=m-1
		}
	    if gin=0 then return gim
	    gin--
	    }
	else if c='M' then return 0
	skip_item_level:
	gim++
	while mseg[gim]b<>13 gim++
	}
    }

;== load menu =================================================================

print bios "FastBase Menu (c) The FastBase Development Company Ltd 1993."
print bios

reg ax=0c7ffh
int 2fh
if reg ax=0c700h then
    {
    ;already installed
    reg ax=0c7fbh	    ;function to set fbmenu_flag=1
    int 2fh		    ;multiplexer
    stop
    }

print bios "initialising ... ";

open #1,"fbmenu.m"
if error then
    {
    print bios "error: file ""fbmenu.m"" missing."
    beep:stop
    }

seek #1,eof:maxm=reg ax
seek #1,0
mseg=allocate maxm/16+1
r=read #1,maxm to mseg|0
mseg[r]b=1ah
close #1

menu_security=0 	;low level

if cmd_nos=1 then
    {
    c=peekb cmd_add(1)
    if (c>='1') and (c<='9') then menu_security=c-'0'
    }

dm=get_item(0,0,'M')
m=skip_blank(dm)
if mseg[m]b<>'M' then
    {
    print bios "Missing Menu Descriptions"
    beep:stop
    }

;take over the DOS interupt for entering strings, ie: the DOS command line
dos 35(21):dosseg=reg es:dosoff=reg bx
poke oldseg,dosseg:poke oldoff,dosoff
poke 0|21h*4,set_signal:poke 0|21h*4+2,reg cs

;install MUX handler
dos 35h(2fh)			;get interupt vector
poke multi_off,reg bx
poke multi_seg,reg es

reg dx=mux_start,es=reg cs
dos 25(2fh)			;multiplexer handler

fillb max_menus from menuy with 0
fillb max_menus from menu_no with 0

;== screen saver setup and routines ===========================================

const ss_boxes=8
ss_mem ? ss_boxes*4	    ;x,y,circle

fill ss_boxes*2 from ss_mem with 0

proc screen_saver
    {
    colour 7:cls:cursor 0,0
    counter=0

    while not keyscan
	{
	m=ss_mem
	for box=1 to ss_boxes
	    x=peekb m
	    y=peekb (m+1)
	    c=peekb (m+2)
	    i=peekb (m+3)

	    if x=0 then
		{
		if (rnd and 63)=10 then
		    {
		    x=1+rnd mod 79
		    y=rnd mod 25
		    i=1+rnd and 7
		    }
		}

	    if x then
		{
		locate y,x
		if box and 1 then colour 7 else colour 15
		if (rnd and 63)=20 then x=0:s=' '
		else
		    {
		    c+=i
		    s=peekb (circle+((c/8) and 3))
		    }
		print chr s;
		}

	    pokeb m,x:pokeb m+1,y:pokeb m+2,c:pokeb m+3,i
	    m+=4
	next box

	repeat 200 repeat 200 { }
	counter++

	locate 0,0
	if counter and 256
	then print "(screen saver)";
	else print "              ";
	}
    return

    circle: datab '|/-\'
    }

;== procedures ================================================================

function get_time
    {
    dos 2c(0)
    hour=high reg cx
    minute=low reg cx
    return hour*60+minute
    }

function print_name(m,len)
    {
    while mseg[m]b<>13
	{
	if len then print chr mseg[m]b;:len--
	m++
	}
    while len print " ";:len--
    return skip_blank(m)
    }

proc setup_screen
    {
    colour back_colour:cls
    fill 2000 from video|0 with 177+back_colour*256
    colour top_colour
    fill 80*4 from video|0 with ' '+top_colour*256
    locate 1,1:print "FBMENU v1.02";
    locate 2,1:print "(c) The FastBase Development Company Ltd";
    colour top_colour2

    if menu_security=9 then locate 2,55:print "ESC=Exit";

    dm=0:functions=0
    locate 24,0

    next_function:
    m=get_item(dm,functions,'F')
    if m then
	{
	functions++
	colour f1_colour
	print " F";functions;":";
	m++
	while mseg[m]b<>13 print chr mseg[m]b;:m++
	print " ";
	goto next_function
	}
    if functions then
	{
	while locpos<4000 print " ";
	}
    }

proc display_menu(menul,menun)
    {
    dm=get_item(0,menun,'M')
    m=skip_blank(dm)
    m++
    menu_colour=get_number(m)
    menu_colour=default_menu

    x=5+menul*3
    y=menul+6

    locate y,x:print "ÉÍ";
    repeat menu_width print "Í";
    print "Í»";
    locate y+1,x:print "º ";
    repeat menu_width print " ";
    print " º";
    locate y+1,x+2
    dm=print_name(m,menu_width)
    locate y+2,x:print "ÌÍ";
    repeat menu_width print "Í";
    print "Í¹";
    y+=3
    menu_options=0

    display_next_item:
    m=get_item(dm,menu_options,'I')
    if m then
	{
	locate y,x:print "º ";
	print chr('A'+menu_options);"  ";
	m=print_name(m+1,menu_width-3):print " º";
	y++:menu_options++
	goto display_next_item
	}

    end_display:
    locate y,x:print "ÈÍ";
    repeat menu_width print "Í";
    print "Í¼";
    }

proc show_bar(sby,sbc)
    {
    locate y+sy+3,x+1
    loctocur
    repeat menu_width+2
	{
	video[locpos+1]b=sbc
	locpos+=2
	}
    }

function select_menu(sm)
    {
    x=5+menu_level*3
    y=menu_level+6
    sy=peekb (menuy+menu_level)

    forever
	{
	show_bar(sy,scroll_colour)
	key_start:
	start_time=get_time

	key_wait:
	#long
	if not keypressed then
	    {
	    #short
	    repeat 10000 {}
	    end_time=get_time
	    colour top_colour2
	    locate 2,68
	    am=1
	    if hour>=12 then
		{
		if hour>12 then hour-=12
		am=0
		}
	    if hour<10 then print "0";
	    print hour;":";
	    if minute<10 then print "0";
	    print minute;" ";
	    if am then print "am"; else print "pm";

	    if (end_time-start_time)>=time_out then
		{
		screen_saver
		return -2
		}
	    goto key_wait
	    }

	show_bar(sy,menu_colour)

	ks=keyscan
	k=lcase low ks:s=high ks
	if (s=1) or (s=78) then return -1

	if ks=7936 then
	    {
	    screen_saver
	    return -2
	    }

	if ks=11520 then
	    {
	    moveb 5 from old_signal to set_signal	;reset DOS int 21h
	    moveb 5 from mux_resume to mux_start	;reset MUX int 2fh
	    return -1
	    }

	if s=71 then sy=0
	if s=72 then sy--
	if s=73 then sy=0
	if s=79 then sy=99
	if s=80 then sy++
	if s=81 then sy=99

	if (k>='a') and (k<='z') then
	    {
	    sy=k-'a'
	    if sy>=menu_options then sy=menu_options-1
	    else goto enter_option
	    }

	if (s>=59) and (s<=(58+functions)) then
	    {
	    pokeb menuy+menu_level,sy	    ;store current y position
	    return 100+(s-59)
	    }

	if sy<0 then sy=0
	if sy>=menu_options then sy=menu_options-1

	if k=13 then
	    {
	    enter_option:
	    pokeb menuy+menu_level,sy
	    return sy
	    }
	}
    }

function get_response
    {
    len=get_number(option+1)
    option=m

    colour 48
    for x=21 to 77
	locate 19,x:print "Í";
	locate 20,x:print " ";
	locate 21,x:print "Í";
    next x

    locate 19,20:print "É";:locate 19,78:print "»";
    locate 20,20:print "º";:locate 20,78:print "º";
    locate 21,20:print "È";:locate 21,78:print "¼";

    locate 20,22
    while mseg[option]b<>13 print chr mseg[option]b;:option++
    print " : ";
    loctocur
    pokeb get_string,len+1
    inputs get_string
    moveb 50 from get_string+1 to response+(50*responses)
    responses++

    if peekb (get_string+1)=0 then beep:return 0
    return option
    }

function process_commands(option)
    {
    while mseg[option]b<>13 option++
    option=skip_blank(option)
    c=mseg[option]b
    if c='X' then return 1
    if c='G' then
	{
	do_something=0	    ;not a dos command line
	return get_response
	}
    if c='S' then
	{
	if (menu_level+1)<max_menus then
	    {
	    menu_level++
	    pokeb menu_no+menu_level,get_number(option+1)
	    pokeb menuy+menu_level,0
	    }
	return 0
	}
    if c<>'D' then return 0
    if cls_screen then
	{
	colour 7:scroll 0,4,79,24,0:cursor 5,0
	cls_screen=0
	}
    option++
    return option
    }

;== initialise variables ======================================================

menu_level=0
fbmenu_flag=1	    ;FBMENU = active

stop resident	    ;everything has been initialised now

;== main program ==============================================================

proc fbmenu_option
    {
    fbmenu_start:
    setup_screen

    menul=0
    while menul<=menu_level
	{
	menu_number=peekb (menu_no+menul)
	display_menu(menul,menu_number)
	menul++
	}

    cmd=select_menu(menu_pos)
    if cmd=-2 then goto fbmenu_start
    if cmd=-1 then
	{
	if menu_level then menu_level--
	else
	    {
	    if menu_security=9 then cls:return 1
	    }
	goto fbmenu_start
	}

    if cmd>=100 then	    ;function key?
	{
	option=get_item(0,cmd-100,'F')
	}
    else
	{
	option=get_item(dm,cmd,'I')
	}

    cls_screen=1
    responses=0
    option=process_commands(option)
    if option then return option
    goto fbmenu_start
    }

;== DOS command line control routines =========================================

;fbmenu_flag = 0 : get a command line
;fbmenu_flag = 1 : select a menu option
;fbmenu_flag = 2 : pass menu command lines to command line

set_signal:
inline 9ch
inline 80h,0fch,0ah
inline 74h,6
inline 9dh
old_signal:
inline 0eah
oldoff:
data 0
oldseg:
data 0

inline 0fbh,9dh
pushall

push reg ds
reg ds=reg cs
offset=reg dx
pop segment

fbmenu_loop:

if fbmenu_flag=0 then
    {
    popall
    goto old_signal
    }

if fbmenu_flag=1 then
    {
    do_something=1
    option=fbmenu_option
    if option then
	{
	fbmenu_flag=2
	if do_something then goto return_command
	goto fbmenu_loop
	}
    }

if fbmenu_flag=2 then
    {
    option=process_commands(option)
    if option then goto return_command
    else fbmenu_flag=1
    }

goto fbmenu_loop

return_command:
typed=0
#long
if option=1 then
    {
    fbmenu_flag=0
    }
else
    {
    em=option
    x=offset+2
    while mseg[em]b<>13
	{
	c=mseg[em]b:em++
	if c='%' then
	    {
	    r=mseg[em]b-'1':em++
	    m=response+50*r
	    moveb peekb m from m+1 to segment|x
	    x+=peekb m
	    typed+=peekb m
	    }
	else segment[x]b=c:x++:typed++
	}
    }
#short
segment[offset+2+typed]b=13
segment[offset+1]b=typed
popall
iret

;== mux handler ===============================================================

mux_start:
inline 3dh,0ffh,0c7h		;cmp ax,c7ffh
inline 075h,04			;jne mux_check
inline 0b8h,0,0c7h		;mov ax,c700h
iret				;iret

inline 3dh,0fbh,0c7h		;cmp ax,c7fbh
inline 75h,8			;jne mux_resume
inline 2eh			;cs:
fbmenu_flag=1			;fbmenu_flag=1
iret				;iret

mux_resume:
inline 0eah			;jmpf seg|off

multi_off: data 0
multi_seg: data 0

get_string: string 50
