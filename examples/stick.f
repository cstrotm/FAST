
;STICK - for want of a better name (stick to another computer maybe?)

;==============================================================================
; OPEN PC-PC COMMUNICATIONS PROGRAM (c) Peter Campbell Software 1994.
;==============================================================================

;23-01-96 If using b/w screen and site you are connected to is colour then
;	  weird things appear on screen (sometimes flashing or invisible)
;	  STICK now converts colours to mono equivalents
;
;

const resident_tsr=0	    ;1=tsr

#include cmd_line.fi
#include stick.fi
#window memory 1000

;==============================================================================

proc establish_connection
    {
    ;dial!!!
    wait_seconds(1)
    print bios "Dialing ";

    len=cmd_len(1)
    moveb len from cmd_add(1) to dial_number+4
    pokeb dial_number+4+len,0

    send_string(dial_number)
    wait_seconds(1)

    if not wait_for_data(99,"CONNECT") then abort_error("Failed to connect.")

    if wait_for_data(10,"*STICKER*") then goto talking_to_stick

    abort_error("Failed to communicate correctly - setup?")

    talking_to_stick:

    print bios "Established a connection with STICKER."
    print bios
    print bios "To abort the connection: Press Left Shift - Ctrl - ESC"
    print bios "To start file transfer: Press Left Shift - Ctrl - F1"
    print bios
    return

    dial_number:
    datab 'atdt'
    space 128
    }

;==============================================================================

proc translate_colour(tc)
    {
    if mono then
	{
	if tc>=192 then return 240
	else if tc>=128 then return 135
	else if tc>=64 then return 112
	return (tc and 8)+7
	}
    else return tc
    }

;==============================================================================

proc process_buffer(len)
    {
    ;buffer contains row number (1-24) then the data, RLL compressed
    ;data represents 80 characters then 80 colours
    ;number 255 followed by 255 represents one 255
    ;number 255 followed by a character then repeat value (4-80)

    x=0
    y=peekb buffer

    if y=255 then			    ;row 255 = cursor position
	{
	cursor peekb (buffer+2),peekb (buffer+1)
	return
	}

    if (y<0) or (y>24) then beep:y=24	    ;invalid data

    locpos=y*fscreen_cols2

    m=buffer+1:len-=5

    while len>0
	{
	c=peekb m:m++:len--
	count=1

	if c=255 then
	    {
	    c=peekb m:m++:len--
	    if c<>255 then count=peekb m:m++:len--
	    }

	repeat count
	    {
	    video[locpos]b=c
	    locpos+=2:x++
	    }

	if x>=fscreen_cols1 then
	    {
	    locpos=y*fscreen_cols2+1:x=0
	    }
	}
    }

;==============================================================================

proc file_transfer
    {
    open window window_transfer
    cursor 11,35
    wait for keypressed
    dsz_mode=ucase key

    if (dsz_mode<>'R') and (dsz_mode<>'S') then
	{
	beep
	close window
	return
	}
    print bios chr dsz_mode;

    locate 13,18:colour 79
    print "File Name : ";:loctocur
    #inpend=13
    inputs dsz_filename

    close window

    if peekb (dsz_filename+2)=13 then return

    if dsz_mode='S' then        ;send file to remote site?
	{
	send_character(0fch)
	send_character(0fch)
	send_character(255)
	send_character(255)
	send_character(255)

	;shell 'dsz.com port n sz filename';
	len=10:dm=dsz_send+11:fm=dsz_filename+2
	while peekb fm<>13
	    {
	    pokeb dm,peekb fm
	    dm++:fm++:len++
	    }
	pokeb dm,13
	wait_seconds(1)

	pokeb dsz_send,len
	pokeb dsz_send+6,com_port+'1'
	execute_shell(dsz_send)
	}
    else			;receive file from site?
	{
	send_character(0fch)
	send_character(0fch)
	send_character(255)
	send_character(255)
	send_character(254)

	m=dsz_filename+2
	while peekb m<>13
	    {
	    send_character(peekb m)
	    m++
	    }
	send_character(13)
	wait_seconds(1)

	;shell 'dsz.com port n rz -y';
	pokeb dsz_receive+6,com_port+'1'
	execute_shell(dsz_receive)
	}
    }

;== The Start =================================================================

initialise_modem

;ask for number to dial - use directory from config file
establish_connection

code_fb=0

forever
    {
    ;wait for response - input into buffer, display screen
    ;all messages from host require a header: 0FB, 0FB, length (lo,hi)
    c=read_character
    if got then
	{
	if code_fb=0 then	;header is always 0fb, 0fb, length(2), data
	    {
	    if c=0fbh then code_fb=1
	    }
	else if code_fb=1 then
	    {
	    if c=0fbh then code_fb=2
	    else code_fb=0
	    }
	else if code_fb=2 then
	    {
	    code_low=c:code_fb=3
	    }
	else if code_fb=3 then
	    {
	    code_len=c*256+code_low
	    if (code_len>=2) and (code_len<=321) then
		{
		waitm=buffer:len=4  ;received 4 so far
		code_fb=4	    ;ok, set for reading data
		}
	    else code_fb=0
	    }
	else
	    {
	    pokeb waitm,c:waitm++:len++
	    if len>=code_len then
		{
		process_buffer(len)	;display line
		code_fb=0		;reset codes
		}
	    }
	}

    ;if a key is pressed then send to host (0FC, 0FC, STATUS, KEY, SCAN)
    if keypressed then
	{
	ks=keyscan
	kstat=peekb 0040h|0017h

	if ks=283 then if (kstat and 6)=6 then abort_error("User Aborted")
	if ks=24064 then if (kstat and 6)=6 then
	    {
	    file_transfer
	    goto skip_send_char
	    }

	send_character(0fch)
	send_character(0fch)
	send_character(kstat)
	send_character(low ks)
	send_character(high ks)

	skip_send_char:
	}
    }

;==============================================================================

window_transfer:
datab 0,0,15,9,65,15,79
datab 22,3,2,'Send or Receive?'
datab 26
