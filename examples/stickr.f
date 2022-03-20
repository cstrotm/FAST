
;STICK - for want of a better name

;==============================================================================
; OPEN PC-PC COMMUNICATIONS PROGRAM (c) Peter Campbell Software 1994.
;==============================================================================

const resident_tsr=1	    ;TSR - host system

#include stick.fi
#window memory 1000

const keys_size=50
var keys_head,keys_tail
var indo,inds
var dtao,dtas
var stick_ss,stick_sp

keys_buffer ? keys_size*3

const mux_code=0f1h
const mux_init=0f2h
const mux_ok=  0f3h

;==============================================================================

proc clear_support_screen
    {
    fill 2000 from screen_seg|0 with 0
    }

proc setup_connection
    {
    ;send stick message then version and keyboard LED status
    open window window_connect
    repeat 6
	{
	wait_seconds(1)
	send_string("*STICKER*")
	send_string("*STICKER*")
	send_string("*STICKER*")
	}
    close window

    clear_support_screen
    cursor_x=-1
    cursor_y=-1
    update_row=0
    connected=1
    dsz_mode=0
    }

;==============================================================================

function send_buffer(len)
    {
    m=buffer
    while len
	{
	send_character(peekb m)
	m++:len--
	}
    }

;==============================================================================

function check_bytes
    {
    if serial_tail>serial_head then count=serial_head+serial_size-serial_tail
    else count=serial_head-serial_tail

    return count    ;number of characters in buffer
    }

;==============================================================================

;keyboard interupt
;if keys_head<>keys_tail then use character in buffer
;else return to old keyboard interupt.

goto skip_this_inline

new_int16:
enable interupts
pushall

push reg ax
reg ds=reg cs
pop ff
ff=high ff	;reg AH

if (ff=0) or (ff=10h) then
    {
;   kstat=peekb 0040h|0017h
    while keys_head=keys_tail	;wait for modem keypress
	{
	if not connected then goto skip_waiting
;	if kstat<>peekb 0040h|0017h then goto skip_waiting

	nextk=peek 0040h|001ah	    ;user keypress
	lastk=peek 0040h|001ch
	if nextk<>lastk then goto skip_waiting

;	if serial_head<>serial_tail then goto skip_waiting
	if dsz_mode then goto skip_waiting
	}
    skip_waiting:
    }

if keys_head<>keys_tail then	;key in buffer?
    {
    if (ff=0) or (ff=10h) or (ff=1) or (ff=11h) then
	{
	m=keys_buffer+keys_tail*3
	kstat=peekb m
	kkeyscan=peek (m+1)
	force_update=2		;force entire update of screen (approx)

	pokeb 0040h|0017h,kstat

	if (ff and 1)=0 then	;remove key from buffer
	    {
	    keys_tail++:if keys_tail>=keys_size then keys_tail=0
	    }
	else	;test only, set not-zero flag
	    {
	    reg ax=kstat+5000	;not zero
	    }

	popall
	inline 2eh	    ;CS:
	reg ax=kkeyscan     ;MOV AX,CS:[KKEYSCAN]
	inline 0cah,2,0     ;RETF 0002
	}
    }

popall
inline 0eah	;JMPF interupt_16
int16_off:
data 0
int16_seg:
data 0

skip_this_inline:

;==============================================================================

function update_screen(us_row)
    {
    locpos=us_row*fscreen_cols2
    len=0

    update_line=compare fscreen_cols1 at video|locpos with screen_seg|locpos
    if not update_line then
	{
	;compare at 0 can't work
	if video[locpos]<>screen_seg[locpos] then update_line=1
	}

    if update_line then
	{
	;compress buffer and attach it to the output
	m=buffer+5:len=5    ;include header - set at end
	x=0:locend=0

	while locend<2
	    {
	    c=video[locpos]b

	    if c=255 then
		{
		pokeb m,c:m++:len++
		pokeb m,c:m++:len++
		locpos+=2:x++
		}
	    else
		{
		l2=1
		if x<76 then	;check repeat?
		    {
		    locpos2=locpos+2
		    x2=x+1
		    while (x2<fscreen_cols1) and (video[locpos2]b=c)
			{
			locpos2+=2
			x2++
			l2++
			}
		    }
		if l2>3 then
		    {
		    pokeb m,255
		    pokeb m+1,c
		    pokeb m+2,l2
		    m+=3:len+=3
		    locpos=locpos2:x=x2
		    }
		else
		    {
		    pokeb m,c:m++:len++
		    locpos+=2:x++
		    }
		}

	    if x>=80 then
		{
		locpos=us_row*fscreen_cols2+1:x=0
		locend++
		}
	    }

	poke buffer,0fbfbh
	poke buffer+2,len
	pokeb buffer+4,us_row

	send_buffer(len)

	;update screen copy
	locpos=us_row*fscreen_cols2
	move fscreen_cols1 from video|locpos to screen_seg|locpos
	}

    return len
    }

;==============================================================================

proc update_supporter	    ;interupt driven
    {
    if serial_status and 128 then
	{
	if not connected then
	    {
	    if check_bytes<7 then return
	    c=read_character
	    if c<>'C' then return
	    m=connect_response+1
	    repeat 6
		{
		if read_character<>peekb m then return
		m++
		}
	    setup_connection
	    }
	}
    else
	{
	if connected then
	    {
	    connected=0
	    dsz_mode=0
	    open window window_disconnect
	    hang_up
	    close window
	    }
	return
	}

    ;send some screen?
    lines=0:transmit_len=0

    waiting++
    if (waiting and 31)=0 then force_update++

    ;if keys in buffer don't send screen
    if keys_head<>keys_tail then goto skip_screen_update

    if force_update then
	{
	while (lines<25) and (transmit_len<160)     ;160 is a rough guess for time
	    {
	    transmit_len+=update_screen(update_row)
	    update_row++:if update_row>24 then update_row=0
	    lines++
	    }
	force_update--
	}

    ;update cursor position
    x=low curpos
    y=high curpos
    if (cursor_x<>x) or (cursor_y<>y) then
	{
	poke buffer,0fbfbh
	poke buffer+2,7 	;length
	pokeb buffer+4,255
	pokeb buffer+5,x
	pokeb buffer+6,y

	send_buffer(7)
	cursor_x=x
	cursor_y=y
	}

    skip_screen_update:

    ;receive key strokes?
    if check_bytes<5 then return

    c=read_character:if c<>0fch then return
    c=read_character:if c<>0fch then return

    kstat=read_character
    kkey=read_character
    kscan=read_character

    if (kstat=255) and (kkey=255) then
	{
	;file transfer?
	;set dsz_mode - then wait for not using DOS (on int, on idle)
	if kscan=255 then
	    {
	    dsz_mode='R'    ;receive file
	    return
	    }
	if kscan=254 then
	    {
	    dsz_mode='S'    ;send file
	    m=dsz_filename:len=0

	    read_name:
	    c=read_character
	    if c>=' ' then
		{
		pokeb m,c:m++
		len++
		if len<40 then goto read_name
		}
	    pokeb m,13
	    return
	    }
	}

    ;receive another keystroke from support person?
    ;assume file transfer was deactivated
    dsz_mode=0

    ;put into buffer for next int 16h read
    m=keys_buffer+keys_head*3
    pokeb m,kstat
    pokeb m+1,kkey
    pokeb m+2,kscan

    pokeb 0040h|0017h,kstat

    keys_head++:if keys_head>=keys_size then keys_head=0
    }

;==============================================================================

proc file_transfer
    {
    old_ss=reg ss
    old_sp=reg sp

    disable interupts
    reg ss=stick_ss
    reg sp=stick_sp
    enable interupts

    old_psp=psp:psp reg cs
    old_dtao=dta offset:old_dtas=dta segment

    ;set old DTA
    reg dx=dtao:reg ds=dtas:dos 1ah
    reg ds=reg cs

    old_mode=dsz_mode
    dsz_mode=0		;reset now

    stop int 1		;stop screen updates etc

    reg dx=peek int16_off
    reg ds=peek int16_seg:dos 25(16)	;restore keyboard interupt
    reg ds=reg cs

    ;save screen image
    move 2000 from video|0 to screen_seg|0

    if old_mode='S' then        ;send file to remote site?
	{
	;shell 'dsz.com port n sz filename';
	len=10:dm=dsz_send+11:fm=dsz_filename
	while peekb fm<>13
	    {
	    pokeb dm,peekb fm
	    dm++:fm++:len++
	    }
	pokeb dm,13

	pokeb dsz_send,len
	pokeb dsz_send+6,com_port+'1'
	locate 24,74:print "SND";
	execute_shell(dsz_send)
	}
    else			;receive file from site?
	{
	;shell 'dsz.com port n rz -y';
	pokeb dsz_receive+6,com_port+'1'
	locate 24,74:print "RCV";
	execute_shell(dsz_receive)
	}

    ;restore screen image
    move 2000 from screen_seg|0 to video|0
    clear_support_screen

    on int 1		;restore screen updates
	{
	update_supporter
	if (dsz_mode<>0) and (inds[indo]b=0) then
	    {
	    locate 24,70:print "DSZ";
	    file_transfer
	    }
	}

    reg dx=new_int16:dos 25(16)     ;set stick keyboard routines

    ;set program's DTA
    reg dx=old_dtao:reg ds=old_dtas:dos 1ah
    reg ds=reg cs

    psp old_psp

    disable interupts
    reg ss=old_ss
    reg sp=old_sp
    enable interupts
    }

;==============================================================================

proc initialise_stickr
    {
    connected=0
    dsz_mode=0
    force_update=0

    keys_head=0
    keys_tail=0

    enable serial,enable_number

    repeat 5
	{
	for y=500 to 1500
	noise 100,y+(rnd and 255)
	next y
	}
    noise off
    }

;==============================================================================

goto skip_multiplexer

new_multi:
pushall
enable interupts
push reg ax
reg ds=reg cs
pop ax

if ax=(mux_code*256+mux_init) then
    {
    initialise_stickr
    popall
    reg ax=mux_ok
    iret
    }

popall
inline 0eah	;JMPF multiplexer
multi_off:
data 0
multi_seg:
data 0

skip_multiplexer:

;==============================================================================

initialise_modem
send_string("ats0=2"):wait_for_data(2,ok_msg)

;detect if stickr.com already loaded?
;if so then activate it with the multiplexer function and stop

reg ax=mux_code*256+mux_init
int 2fh
ax=reg ax
if (low ax)=mux_ok then
    {
    print bios
    print bios "STICK already loaded - please wait for support call."
    stop
    }

;setup multiplexer interupt 2fh
dos 35(2f)
poke multi_seg,reg es:poke multi_off,reg bx
reg dx=new_multi:dos 25(2f)

screen_seg=allocate 4000/16

indo=indoso:inds=indoss
dtao=dta offset:dtas=dta segment

stick_ss=reg ss
stick_sp=reg sp

on int 1
    {
    update_supporter
    if (dsz_mode<>0) and (inds[indo]b=0) then
	{
	locate 24,70:print "DSZ";
	file_transfer
	}
    }
on idle
    {
    if dsz_mode then
	{
	locate 24,70:print "DSZ";
	file_transfer
	}
    }

initialise_stickr

dos 35(16)
poke int16_seg,reg es:poke int16_off,reg bx
reg dx=new_int16:dos 25(16)		;initialise keyboard routines

print bios
print bios "STICK has loaded and initialised correctly."
print bios "When support personnel connect to your computer a message will appear."

stop resident

;==============================================================================

window_connect:
datab 0,0,20,9,60,15,79
datab 22,5,3,'A support person has connected.'
datab 26

window_disconnect:
datab 0,0,18,9,63,15,31
datab 22,5,3,'The support person has disconnected.'
datab 26

connect_response: datab 'CONNECT'
