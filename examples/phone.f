
;== Terminal Emulator : Capture and X/Y/Z MODEM for future =================

;03/08/91  Reset colours when clearing screen (ansi: ESC [ J )
;01/09/93  Provide option in source code for either COM1 or COM2

colour 7

;Fast now has 4 new variables com_port, com_base, com_irq & com_enable
;The default is for COM1
;COM1: port=0, base=03f8h, irq=12, enable=0efh
;COM2: port=1, base=02f8h, irq=11, enable=0f7h

;temporary - these are system variables used by Fast internally
;com_port=1
;com_base=02f8h     ;COM2 parameters
;com_irq=11
;com_enable=0f7h

#window memory 3000
#include extinput.fi
#include fsort.fi
const p32_bios=0
#include fast32.fi
#inpend=0
#short

ext_inpend=0
const phones=42,cfglen=8100
var echo,capture,echo,bufsize
var escnum,cfs
var32 size,topscr,newscr,chars
unsigned cap_off,cap_seg,base,buffer,gwl
unsigned serial_tail,cap_size,o,f
work ? 1600
escaping ? 20
simage ? 4000
lasts=1
escape=0
chars=0
save_pos=0  ;Saved position for ansi.

const soh=01,stx=02,eot=04,ack=06,nak=15h,can=18h

proc abort
    {
    disable serial
    cursor 24,0:print bios
    stop
    }

on error
    {
    cursor 24,0:print bios
    print bios:error msg "\dos.err":print bios "!"
    abort
    }
on break error 999

;== Procedures/Functions ===================================================

proc save_screen
    {
    olocpos=locpos
    move 2000 from video|0 to simage
    cls:locate 0,0
    }

proc load_screen
    {
    move 2000 from simage to video|0
    locpos=olocpos
    }

function ansin(adef)
    {
    an=0:any=0:as=1
    if peekb escnum='+' then escnum++
    if peekb escnum='-' then as=-1:escnum++
    while (peekb escnum>='0') and (peekb escnum<='9')
	{
	d=peekb escnum-'0'
	an=an*10+d
	escnum++
	any=1
	}
    if peekb escnum=';' then escnum++
    if any=0 then return adef
    return an*as
    }

proc cap_write
    {
    if cap_off then write #2,cap_off from cap_seg|0:cap_off=0
    }

proc scr_put(r)
    {
    if capture=0 then return
    if cap_off>=cap_size then cap_write

    r*=160:sr=r
    sl=cap_off:len=0
    repeat 80
	{
	if video[r]b<>' ' then len=(r-sr)/2+1
	cap_seg[sl]b=video[r]b:r+=2:sl++
	}
    cap_seg[cap_off+len]=0a0dh:cap_off+=len+2
    }

proc scr_page for dcy=0 to 23:scr_put(dcy):next dcy

proc send_block(sbl)
    {
    m=work
    repeat sbl l=serial_send peekb m:m++
    }

proc print_to(ptm,ptn)
    {
    pokeb ptm,(ptn/10)+'0'
    pokeb ptm+1,(ptn mod 10)+'0'
    }

proc display(dc)
    {
    #long
    if escape then
	{
    #short
	if escape=escaping then
	    {
	    if dc<>'[' then
		{
		push dc:dc=27:gosub print_dc
		pop dc
		escape=0
		goto not_ansi
		}
	    }
	pokeb escape,dc:escape++
	if escape>=(escaping+20) then goto un_ansi

	if (dc='H') or (dc='j') or (dc='f') then
	    {
	    y=ansin(1):x=ansin(1)
	    locate (y-1),(x-1)
	    goto done_ansi
	    }
	if dc='s' then save_pos=locpos:goto done_ansi
	if dc='u' then locpos=save_pos:goto done_ansi
	if dc='A' then
	    {
	    locpos-=ansin(1)*160
	    goto done_ansi
	    }
	if dc='B' then
	    {
	    locpos+=ansin(1)*160
	    goto done_ansi
	    }
	if dc='C' then
	    {
	    locpos+=ansin(1)*2
	    goto done_ansi
	    }
	if dc='D' then
	    {
	    locpos-=ansin(1)*2
	    goto done_ansi
	    }
	if dc='K' then
	    {
	    x=(locpos mod 160)/2:xl=locpos
	    repeat 80-x video[xl]=0720h:xl+=2
	    goto done_ansi
	    }
	#long
	if dc='n' then
	    {
	#short
	    if peekb escnum<>'6' then goto done_ansi
	    y=locpos/160+1
	    x=(locpos-y*160)/2+1
	    pokeb work,27
	    pokeb work+1,'['
	    print_to(work+2,y)
	    pokeb work+4,';'
	    print_to(work+5,x)
	    poke work+7,'R'
	    send_block(8)
	    goto done_ansi
	    }
	if dc='J' then
	    {
	    scr_page
	    cls:locpos=0
	    colour 7	    ;reset colours too
	    goto done_ansi
	    }
	#long
	if dc='m' then
	    {
	#short
	    ans_sgr:
	    if peekb escnum='m' then goto done_ansi
	    laste=escnum:m=ansin(0):if escnum=laste then goto done_ansi
	    if m=0 then colour 7
	    if m=1 then bright 1
	    if (m=2) or (m=8) then colour 6
;	    if (m=5) or (m=6) then flash 1
	    if m=7 then
		{
		p=(colour and 112)/16
		i=colour and 7
		colour (colour and 72)+p+i*16
		}
;	    if (m>=30) and (m<=37) then ink m-30
;	    if (m>=40) and (m<=47) then paper m-40
	    goto ans_sgr
	    }
	if dc='h' then goto ans_sgr ;why change screen res?
	if dc='P' then
	    {
	    ;do what?
	    goto done_ansi
	    }
	return

	done_ansi: escape=0:goto check_pos

	un_ansi:
	printhb "*";dc;"*";
	dc=27:gosub print_dc
	m=escaping
	while m<escape
	    {
	    dc=peekb m:m++
	    gosub print_dc
	    }
	escape=0
	return
	}

    not_ansi:
    if dc=10 then locpos+=160:goto check_pos
    if dc=13 then locpos-=locpos mod 160:return
    if dc=8 then locpos-=2:goto check_pos
    if dc=9 then
	{
	x=(locpos mod 160)/2:nx=(x and 248)+8
	repeat nx-x print chr ' ';:gosub check_pos
	return
	}

    if dc=27 then
	{
	escape=escaping
	escnum=escape+1
	fillb 20 from escaping with 0
	return
	}

    print_dc:  print chr dc;
    check_pos:
    if locpos<0 then locpos=0
    while locpos>=3840
	{
	scr_put(0)
	scroll 0,0,79,23,1
	locpos-=160
	}
    }

proc send_newline l=serial_send(13):l=serial_send(10)

function get
    {
    c=0:got=0
    if serial_tail<>serial_head then
	{
	c=serial_seg[serial_tail]b:got=1
	chars++
	serial_tail++:if serial_tail>=serial_size then serial_tail=0
	}
    if cfs[8034]b=4 then c=c and 127
    return c
    }

function confirm(text,def)
    {
    do_what:
    escaped=0
    open window confirmw
    locate 20,10:prints text,0
    print "? y/n [";chr('n'+def*11);"] ";
    loctocur
    wait for keypressed:kt=lcase key
    close window
    if kt=27 then escaped=1:return 0
    if kt='n' then return 0
    if kt=13 then return def
    if kt='y' then return 1
    goto do_what

    confirmw:
    datab 0,0,6,18,40,22,15,26
    }

proc backline(ln)
    {
    newscr=topscr-1600:m=work+1599
    sig=high newscr
    if sig<0 then newscr=0:m=work+(low topscr)-1
    seek #1,newscr
    l=read #1,1599 to work:pokeb work+l,1ah
    ln++
    while (ln>0) and (m>work)
	{
	m--
	if peekb m=13 then ln--
	}
    if m<=work then topscr=0:return
    if peekb m=13 then m++
    if peekb m=10 then m++
    mw=m-work:topscr=newscr+mw
    }

proc forwardline(ln)
    {
    newscr=topscr
    m=work
    while (ln>0) and (m<(work+1599))
	{
	if peekb m=13 then ln--
	m++
	}
    if peekb m=10 then m++
    mw=m-work:topscr=newscr+mw
    }

proc find_me(fnew)
    {
    if fnew then
	{
	open window find_string
	locate 22,12:print "Find: ";
	l=ext_string(fstr,30)
	close window
	if l<=1 then return
	if ext_esc then return
	}

    forwardline(1)
    newscr=topscr

    forever
	{
	seek #1,newscr:flen=read #1,bufsize-1 to buffer|1
	if flen=0 then
	    {
	    beep:backline(1)
	    return
	    }
	if flen>(bufsize-40) then flen=bufsize-40
	f=1
	while f<flen
	    {
	    f=searchb flen-f from buffer|f for peekb fstr
	    #long
	    if f then
		{
		c=compareb 30 at fstr with buffer|f
		if peekb c=0 then
		    {
		    topscr=newscr+f-1
		    colour 15
		    return
		    }
		f++
		}
	    else goto next_disk
	    #short
	    }
	next_disk:
	newscr+=flen
	}
    }

proc readme(hfile,hwhat)
    {
    save_screen
    colour 15
    repeat 80 print chr 196;
    print "FAST PHONE -->  Reading ";:prints hfile,0
    locate 1,64:print "F=Find, ^F=next.";
    repeat 80 print chr 196;
    colour 7
    #errors off:open #1,hfile:#errors on
    if error then
	{
	loctocur:error msg "\dos.err":print "! press ESCape"
	wait for key=27
	goto exit_read
	}
    topscr=0
    if hwhat=-1 then goto rm_eof

    forever
	{
	seek #1,topscr
	l=read #1,1599 to work:pokeb work+l,1ah
	m=work
	for y=3 to 23
	  locate y,0
	  m=printm m,81     ;Don't fuck up 80 character lines?
	  colour 7
	next y

	wait for keypressed:ks=keyscan:s=high ks

	if s=1 then goto finish_read
	if s=71 then topscr=0
	if s=72 then backline(1)
	if s=73 then backline(22)
	if s=79 then
	    {
	    rm_eof:
	    seek #1,eof:rax=reg ax:rdx=reg dx:topscr=rax+rdx*65536
	    backline(21)
	    }
	if s=80 then forwardline(1)
	if s=81 then forwardline(22)
	if ks=8454 then find_me(0)
	    else if s=33 then find_me(1)
	}

    finish_read: close #1
    exit_read:	 load_screen
    }

proc intro
    {
    cls:locate 0,0
    colour 120
    repeat 80 print " ";
    print "      FAST PHONE v1.30        Written in FAST by Peter Campbell. 10/09/90       ";
    repeat 80 print " ";
    print
    colour 7
    print "F1	 - PHONEs OnLine(ish) Help."
    print "ALT-C - view Capture buffer (scroll back)."
    print "ALT-D - Dial Directory."
    print "ALT-H - Hangup."
    print "ALT-S - Show all options, change them?"
    print "ALT-X - Exit to DOS  ** Can use DOS then re-enter without losing carrier **"
    print "PgDn  - Download file(s) from BBS."
    print "PgUp  - Upload file(s) to BBS."
    print "ALT-F10 - Pauses display, still captures data." cr
    }

proc send_string(sss,ssm)
    {
    while sss[ssm]b
	{
	l=serial_send sss[ssm]b
	ssm++
	}
    }

proc lpstr(lpm)
    {
    while cfs[lpm]b
	{
	lprint chr cfs[lpm]b;:lpm++
	}
    lprint
    }

proc dial_bar(y,c)
    {
    m=3*160+y*80+1
    repeat 39 video[m]b=c:m+=2
    }

function speed(ds)
    {
    pokeb spedw+1,0:open window spedw:pokeb spedw+1,5
    if (ds<0) or (ds>4) then ds=0
    s=select spedw,ds+1:close window
    if s=0 then return ds
    return s-1
    }

function edit_number
    {
    move 40 from cfs|base to work
    locate 10,18:prints work,0
    locate 11,18:prints work+18,0
    locate 12,18:prints work+36,0
    locate 14,18:prints speeds+peekb (work+79)*15,0

    locate 10,18:if not ext_string(work,18) then return 0
    locate 11,18:if not ext_string(work+18,18) then return 0
    locate 12,18:l=ext_string(work+36,50)
    locate 14,18:pokeb work+79,speed(peekb (work+79))
    move 40 from work to cfs|base

    save_config=1
    return 1
    }

proc dial
    {
    num=0
    save_screen
    colour 120:print " PHONE DIRECTORY ";
    colour 15:print "  ENTER";:colour 7:print "=dial";
    colour 15:print "  A";:colour 7:print "dd";
    colour 15:print "  C";:colour 7:print "hange";
    colour 15:print "  D";:colour 7:print "elete";
    colour 15:print "  S";:colour 7:print "ort";
    colour 15:print "  P";:colour 7:print "rint";
    colour 15:print "  ESC";:colour 7:print "=exit";
    locate 1,0:print "Name		  Number";
    locate 1,40:print "Name		   Number";
    locate 2,0:repeat 80 print chr 196;

    show_numbers:
    scroll 0,3,79,24,0

    m=0:y=0
    repeat phones
	{
	locpos=3*160+y*80:l=locpos
	prints cfs|m,0
	if cfs[m]b then
	    {
	    locpos=l+40
	    prints cfs|m+18,0
	    }
	m+=80:y++
	}

    get_number:
    dial_bar(num,120)
    wait for keypressed:ks=keyscan:kt=lcase low ks:s=high ks
    dial_bar(num,7)
    base=num*80
    if s=1 then goto exit_dial
    if s=71 then num=0
    if s=72 then num-=2
    if s=80 then num+=2
    if s=79 then num=phones-1
    if s=75 then num=num and 0feh
    if s=77 then num=num or 1
    if num>=phones then num=phones-1
    if num<0 then num=0

    #long
    if kt=13 then
	{
	load_screen
	if locpos>=3840 then scr_put(0):scroll 0,0,79,23,1
	print "Calling... ";:prints cfs|base,0:print "  ";
	if cfs[base+36]b then print "Note: ";:prints cfs|base+36,0
	print

	enable serial,peekb (spbyte+cfs[base+79]b)

	send_string(cfs,8000)
	send_string(cfs,base+18)
	send_newline
	return
	}
    if kt='a' then
	{
	open window wdial_add
	base=0:nnum=0
	repeat phones
	    {
	    if cfs[base]b=0 then goto add_ok
	    base+=80:nnum+=1
	    }
	beep:close window
	goto next_number

	add_ok:
	fill 40 from cfs|base with 0
	if edit_number then num=nnum
	close window
	goto show_numbers
	}
    #short
    if kt='c' then
	{
	open window wdial_add
	edit_number
	close window
	goto show_numbers
	}
    if kt='d' then
	{
	if confirm(c_del,0) then
	    {
	    cfs[base]b=0
	    save_config=1
	    goto show_numbers
	    }
	}
    #long
    if kt='s' then
	{
	base=0
	repeat phones
	    {
	    if cfs[base]b=0 then cfs[base]b=255 ;nulls come last.
	    base+=80
	    }
	s=sort(cfs,0,80,36)
	base=0
	repeat phones
	    {
	    if cfs[base]b=255 then cfs[base]b=0
	    base+=80
	    }
	save_config=1
	goto show_numbers
	}
    if kt='p' then
	{
	if confirm(c_print,0) then
	    {
	    lprint "PHONE DIRECTORY"
	    lprint "---------------"
	    lprint
	    m=0
	    repeat phones
		{
		if peekb m then
		    {
		    lprint "Name:    ";:lpstr(m)
		    lprint "Number:  ";:lpstr(m+18)
		    lprint "Note:    ";:lpstr(m+36)

		    lprint
		    }
		m+=80
		}
	    repeat 4 lprint
	    }
	}
    #short

    next_number:
    goto get_number

    exit_dial:
    load_screen
    }

proc close_capture cap_write:close #2

proc open_capture
    {
    cap_off=0
    move 20 from cfs|8010 to work
    #errors off:open #2,work:#errors on
    if error=2 then create #2,work
    if error then beep:capture=0:return
    seek #2,eof
    }

proc setup
    {
    olocpos=locpos
    pokeb wsetup+1,0
    open window wsetup

    forever
	{
	scroll 12,8,49,13,0
	locate	8,12:print "Dial command: ";:prints cfs|8000,0
	locate	9,12:print "Capture file: ";:prints cfs|8010,0
	locate 10,12:print "Capture is ";
	if capture then print "on"; else print "off";
	locate 11,12:if echo then print "Half duplex (echo on)";
	    else print "Full duplex (echo off)";
	locate 12,12:print "MODEM ";:prints speeds+(cfs[8034]b)*15,0
	locate 13,12:print "MODEM init:   ";:prints cfs|8035,0
	pokeb wsetup+1,6
	lasts=select wsetup,lasts
	if lasts=0 then lasts=1:close window:locpos=olocpos:return
	if lasts=1 then
	    {
	    moveb 10 from cfs|8000 to work
	    locate 8,26:l=ext_string(work,10)
	    moveb 10 from work to cfs|8000
	    }
	if lasts=2 then
	    {
	    moveb 10 from cfs|8010 to work
	    locate 9,26:l=ext_string(work,20)
	    moveb 20 from work to cfs|8010
	    }
	if lasts=3 then
	    {
	    capture=not capture:cfs[8030]b=capture
	    if capture then open_capture else close_capture
	    }
	if lasts=4 then echo=not echo:cfs[8031]b=echo
	if lasts=5 then cfs[8034]b=speed(cfs[8034]b)
	if lasts=6 then
	    {
	    moveb 20 from cfs|8035 to work
	    locate 13,26:l=ext_string(work,20)
	    moveb 20 from work to cfs|8035
	    }
	save_config=1
	}
    }

proc print_blocks(bs)
    {
    seek #1,eof
    ax=reg ax:dx=reg dx
    size=dx*65536+ax
    size+=bs-1
    size/=bs
    print "Uploading ";low size;" blocks..."
    seek #1,0
    }

proc fload(sr)
    {
    pokeb cline+8,sr
    x=menu wload:close window:if x=0 then return
    c='z'
    if x=1 then c='x'
    if x=2 then c='b'
    pokeb cline+9,c

    pokeb cline+6,('1'+com_port)

    push locpos
    open window cwind
    locate 20,32:print "File: ";:l=ext_string(cfile,30)
    close window
    pop locpos
    if l=0 then return
    moveb l from cfile to cline+11:pokeb cline+10+l,13
    pokeb cline,l+2
    print:loctocur

    poke exed+4,reg cs
    poke exed+8,reg cs
    poke exed+12,reg cs

    execute fdsz,exed
    curtoloc:print
    if error then print "File Transfer Problem?":beep
    print "Ok."
    }

proc show_status
    {
    push locpos,colour
    locate 24,0:colour 120
    b=serial_tail
    if serial_tail>serial_head then b-=64000
    a=serial_head-b
    print "  PHONE   waiting characters ";a;"      ";
    locate 24,36
    print "Total characters read = ";
    n32=chars
    print32(0)
    while locpos<4000 video[locpos+1]b=120:locpos+=2
    colour 7
    pop colour,locpos
    }

;== Start here... ==========================================================

intro

cfs=allocate cfglen/16+1
serial_size=64000:serial_seg=allocate serial_size/16+1
bufsize=20480:buffer=allocate bufsize/16
cap_size=32000:cap_seg=allocate cap_size/16+6

serial_head=0:serial_tail=0

save_config=0
#errors off
load "phone.cfg",cfs|0,cfglen
#errors on
if error then
    {
    if error<>2 then error
    fillb cfglen from cfs|0 with 0
    }
capture = cfs[8030]b:if capture then open_capture
echo	= cfs[8031]b

enable serial,peekb (spbyte+cfs[8034]b)
if (serial_status and 128)=0 then send_string(cfs,8035):send_newline

forever
    {
    loctocur
    repeat 20
	{
	c=get:if got=0 then goto stop_get
	display(c)
	}

    stop_get:
    #long
    show_status
    if keypressed then
	{
    #short
	phks=keyscan:phk=low phks:phs=high phks

	#long
	if phks=11520 then
	    {
	#short
	    if save_config then
		{
		x=confirm(c_save,1)
		if escaped then goto next_key
		if x then save "phone.cfg",cfs|0,cfglen
		}
	    if capture then scr_page:close_capture
	    abort
	    }
	if phks=28928 then
	    {
	    while not keyscan show_status
	    goto next_key
	    }
	if phs=59 then readme(fhelp,0):goto next_key
	if phks=7936 then setup:goto next_key
	if phks=8192 then dial:goto next_key
	if phks=20736 then fload('r'):goto next_key
	if phks=18688 then fload('s'):goto next_key

	if phks=8960 then
	    {
	    print "Hanging up..."
	    send_string(reg cs,hplus)
	    repeat 10 repeat 65000 {}
	    send_string(reg cs,hath)
	    send_newline
	    goto next_key
	    }

	if phks=11776 then
	    {
	    if capture then cap_write:close_capture
	    move 20 from cfs|8010 to work
	    readme(work,-1)
	    if capture then open_capture
	    goto next_key
	    }

	l=serial_send phk:if phk then if echo then display(phk)
	if phk=0 then l=serial_send phs
	}
    next_key:
    }

;== Phones tables and windows ==============================================

c_del:	 fname 'Delete entry'
c_save:  fname 'Save PHONE.CFG'
c_print: fname 'Print Directory'

wdial_add:
datab 0,0,7,7,73,16,7
datab 22,2,1,20,120,'Add Directory Listing',20,15
datab 22,2,3,'Name:'
datab 22,2,4,'Number:'
datab 22,2,5,'Note:'
datab 22,2,7,'Setup:'
datab 26

wsetup:
datab 1,0,10,5,50,14,7
datab 22,2,1,'SETUP: Select item to change'
datab 26

wload:
datab 1,3,55,9,74,15,15
datab 22,2,1,'Protocol?'
datab 22,2,3,'Xmodem'
datab 22,2,4,'Ymodem'
datab 22,2,5,'Zmodem'
datab 26

spedw:
datab 1,5,20,14,40,22,15
datab 22,2,1,'MODEM Setting?'
datab 22,2,3,'2400 8N1'
datab 22,2,4,'300  8N1'
datab 22,2,5,'1200 8N1'
datab 22,2,6,'9600 8N1'
datab 22,2,7,'2400 7E1'
datab 26

speeds:
fname '2400 8N1      '
fname '300  8N1      '
fname '1200 8N1      '
fname '9600 8N1      '
fname '2400 7E1      '

spbyte:
datab 10100011b
datab 01000011b
datab 10000011b
datab 11100011b
datab 10111010b

fhelp: fname 'phone.doc'

find_string: datab 0,0,10,20,60,24,15,26
fstr:	     space 30

fdsz:  fname 'dsz.com'
cline: datab '.port 1 rz f                               '
cfile: space 30

exed:
data 0
data cline,0
data 5ch,0
data 6ch,0

cwind: datab 0,0,30,18,76,22,15,26

hplus: fname '+++'
hath : fname 'ATH'
