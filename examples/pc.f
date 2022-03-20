;PC - Doucument reporter. (C) PC 16/5/1988

;03/08/91  Default to reading *.* files, not *.doc


const maxd=1024,blen=512	   ;BLEN >= 256
var cd,m,mad,loc,wordstar,ds
var32 topscr,botscr,fseek,secondline,fad,atemp
fbuf ? blen

#include fsort.fi

on error
    {
    print bios
    error msg "\dos.err"
    print bios "!"
    terminate
    }

proc clear_bot
    {
    colour 7
    scroll 0,20,79,24,0
    cursor 20,0
    locate 20,0
    }

proc show_ws
    {
    colour 7
    locate 2,70:print " WS ";
    if wordstar=255 then print "OFF "; else print "ON  ";
    }

function fill_buffer
    {
    fill blen/2 from fbuf with 1a1ah
    read_len=read #1,blen to fbuf
    return fbuf
    }

proc get_eof
    {
    seek #1,eof
    rax=reg ax:rdx=reg dx
    topscr=rax+rdx*65536 ;Current address for TOPSCR.
    }

proc backline(clines)
    {
    const backlen=160
    repeat clines
	{
	fad=topscr-backlen
	if (high fad)<0 then topscr=0:return
	seek #1,fad
	fill backlen/2+1 from fbuf with 1a1ah
	read_len=read #1,backlen to fbuf
	m=fbuf

	atemp=fad
	next_line13:
	col=0
	back_loop:
	c=peekb m:m++
	if c>31 then
	    {
	    back_print:
	    col++
	    if col<80 then goto back_loop
	    c=13
	    }
	if c=26 then goto next_backline
	if c=13 then
	    {
	    ad32=m-fbuf
	    topscr=atemp
	    atemp=fad+ad32
	    goto next_line13
	    }
	if c<>9 then goto back_print
	col=(col and 248)+8
	goto back_loop

	next_backline:
	}
    }

proc position(addr)
    {
    px=addr mod 6
    py=addr/6
    locate 3+py,px*13+1
    }

proc pos_colour(colc)
    {
    repeat 2 video[locpos+1]b=colc:locpos+=2
    while video[locpos-2]b<>' ' video[locpos+1]b=colc:locpos+=2
    cursor 3+py,px*13+2
    }

function get_char
    {
    if m>=(fbuf+blen) then
	{
	m=fill_buffer
	}
    else m++
    return peekb m
    }

proc get_files
    {
    xd=ffind
    while peekb	xd print bios chr peekb	xd;:xd++
    print bios " ";
#errors	off
    find first ffind
    goto entry

    while cd<maxd
	{
	find next
#errors	on
	entry:
	if error then return
	moveb 13 from dta segment|dta offset+30 to ds|mad
	mad+=13:cd++
	}
    }

proc open_file
    {
    moveb 13 from ds|loc*13 to fbuf
    open #1,fbuf
    clear_bot
    colour 70h:locate 2,2
    pm=fbuf
    while peekb pm print chr peekb pm;:pm++
    show_ws
    }

proc display_file
    {
    open_file
    topscr=0

    forever
	{
	seek #1,topscr
	eof_flag=0:pagelen=0
	m=fbuf+blen
	for y=3 to 18
	ad32=m-fbuf
	if y=4 then secondline=topscr+ad32

	locate y,0
	col=0
	loop_prt:
	if m>=(fbuf+blen) then m=fill_buffer
	c=peekb m:m++:pagelen++
	if c>31 then
	    {
	    print_char:
	    col++:print chr c and wordstar;
	    if col<80 then goto loop_prt
	    c=13 ; Start new line.
	    }
	if c=26 then print "<EOF>";:eof_flag=1:goto fill_end
	if c=13 then
	    {
	    if col<80 then fill (80-col) from video|locpos with 0720h
	    goto next_line
	    }
	if c=10 then goto loop_prt ; Skip printing this character.
	if c<>9 then goto print_char
	ncol=(col and 248)+8
	repeat ncol-col print " ";
	col=ncol
	goto loop_prt

	next_line:
	next y

	fill_end:
	fl=19*160-locpos
	if fl>0 then fill fl/2 from video|locpos with 0720h

	botscr=topscr+pagelen
	wait_eof:
	wait for keypressed
	s=scan

	if s=45 then goto abort ; X to abort.
	if s=1 then goto finish_display
	if s=17 then
	    {
	    wordstar=wordstar xor 128
	    show_ws
	    goto next_display
	    }

	if s=71 then topscr=0:goto next_display
	if s=72 then backline(1):goto next_display
	if s=73 then backline(16):goto next_display

	if eof_flag then goto wait_eof

	if s=79 then get_eof:backline(15):goto next_display
	if s=80 then topscr=secondline:goto next_display
	;if s=81 then do nothing, default is view next page.

	topscr=botscr
	next_display:
	}

    finish_display:
    close #1
    }

proc print_file
    {
    open_file
    cursor 20,0:print bios "Press ESC to abort printing. "
    m=fbuf+blen

    print_line:
    col=0
    print_loop:
    if m>=(fbuf+blen) then m=fill_buffer
    c=peekb m:m++
    if c>31 then
	{
	lprint_char:
	col++:lprint chr c and wordstar;
	goto print_loop
	}
    if c=26 then lprint ff;:goto print_end
    if c=13 then
	{
	lprint
	if key=27 then goto print_end
	goto print_line
	}
    if c=10 then goto print_loop ; Skip printing this character.
    if c<>9 then goto lprint_char
    ncol=(col and 248)+8
    repeat ncol-col lprint " ";
    col=ncol
    goto print_loop

    print_end:
    close #1
    }

proc ddir(mad)
    {
    mad*=13
    mn=0

    while (mn<96) and ((mad/13)<cd)
	{
	position(mn)
	locpos+=2
	a=mad
	print chr ucase ds[a]b;
	a++:l=12
	while ds[a]b print chr lcase ds[a]b;:a++:l--
	mad+=13
	mn++
	while l print " ";:l--
	}
    while mn<96 position(mn):mn++:print "             ";
    }


wordstar=255
ds=allocate (maxd*13)/16+15

mad=0
cd=0
print bios "Reading files... ";
#errors	off
open #1,"\PC.FIL":if error then	get_files:goto start_display
#errors	on

m=fbuf+blen
loopf:
c=get_char
loopf2:
if c=26 then goto start_display
if c<=' ' then goto loopf
a=ffind
while c>' '
    {
    pokeb a,ucase c
    a++
    if a>=str_end then error 13
    c=get_char
    }

gf_execute:
pokeb a,0
get_files
if cd<maxd then goto loopf2

start_display:
close #1
drawloc=0:loc=0

if not cd then error 18
if not sort(ds,0,13,cd)	then error 1

display:
cls
colour 71h:locate 0,7
print " PC - Document reporter. Written by Peter Campbell, version 1.2 "
colour 6:locate 1,0
print " Use ARROWS to select file, ENTER to display file on screen or P to print file."
colour 7
repeat 80 print chr 196;
locate 19,0
repeat 80 print chr 196;
show_ws
colour 7
locate 19,2:print " ";cd;" Files. ";
goto redis2

redis:
drawloc=(loc/6)*6
redis2:
ddir(drawloc)

forever
    {
    clear_bot
    position(loc-drawloc)
    pos_colour(70h)
    wait for keypressed
    ks=keyscan:s=high ks:k=lcase low ks

    position(loc-drawloc)
    pos_colour(7)
    if k=27 then goto abort
    if k='p' then print_file:goto display
    if k=13 then display_file:goto display
    loc-=s=75
    loc+=s=77
    if s=72 then loc-=6
    if s=80 then loc+=6
    if s=71 then loc=0
    if s=79 then loc=cd-1
    if s=73 then loc-=96
    if s=81 then loc+=96
    if loc<0 then loc=0
    if loc>cd-1 then loc=cd-1
    if (loc<drawloc) or (loc>(drawloc+95)) then goto redis
    }

abort:
clear_bot
terminate

ffind:
fname '*.*'
space 29    ;Total length=32
str_end:
