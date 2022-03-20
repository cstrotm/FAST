
;==============================================================================
; FastBase Show utility - display file to screen
;==============================================================================

;syntax: fbshow filename.ext

;20-09-93 increase time out delay
;25-01-94 enable Trident VGA mode (56h) 132*60
;28-06-95 change printing to use BIOS int 17h, modify time-out procedure
;23-01-96 provide much needed "find" option (F, enter string)
;

#short

const blen=132*60
fbuf ? blen+2
var32 topscr,botscr,secondline,fad,atemp,n32
var32 findlast
var32 end_of_file,end_of_file23,end_of_file48,file_size,end_of_file58
var column,col,c
unsigned marky

video_lines=23
video_width=80
video_fsize=video_lines*video_width

;== set colours ===============================================================

if mono then
    {
    col_back=15
    col_find=112
    col_top1=120
    col_top2=120
    col_top3=120
    col_scroll=112
    col_place=15
    col_page=7
    }
else
    {
    col_back=31
    col_find=79
    col_top1=116
    col_top2=112
    col_top3=113
    col_scroll=48
    col_place=68
    col_page=30
    }

#include cmd_line.fi
#include extinput.fi

file_name ? 64
ext_inpend=0

if cmd_nos<1 then
    {
    print bios "FBSHOW: no file name specified, aborting."
    beep:stop
    }

file_add=cmd_add(1)
file_len=cmd_len(1)

moveb file_len from file_add to file_name
pokeb file_name+file_len,0

on error
    {
    error msg "\dos.err"
    stop
    }

function fill_buffer
    {
    read_len=read #1,video_fsize to fbuf
    pokeb fbuf+read_len,1ah
    return fbuf
    }

proc backline(clines)
    {
    maxlen=video_fsize

    fad=topscr-video_fsize:n=high fad
    if n<0 then fad=0:maxlen=low topscr
    seek #1,fad
    read_len=read #1,video_fsize to fbuf
    pokeb fbuf+read_len,1ah

    if maxlen>read_len then maxlen=read_len
    m=fbuf+maxlen:if maxlen then m--

    repeat clines
	{
	if m>=fbuf then if clines then m--
	while (m>=fbuf) and (peekb m<>13) m--
	}

    m++
    m-=fbuf
    topscr=fad+m
    }

;==============================================================================

proc get_eof
    {
    seek #1,eof
    rax=reg ax:rdx=reg dx
    file_size=rax+rdx*65536
    topscr=file_size
    backline(22)
    end_of_file23=topscr
    backline(22)
    end_of_file48=topscr
    backline(9)
    end_of_file58=topscr
    }

proc open_file
    {
    #open 01000000b
    open #1,file_name
    }

;==============================================================================

function fill_end(fe)
    {
    if col<0 then col=0
    if col<(video_width-1) then fill ((video_width-1)-col) from video|locpos with fe
    }

;==============================================================================

proc set_video(svm,svc,svr)
    {
    video_lines=svr-2
    video_width=svc
    video_fsize=video_lines*video_width

    if svr=50 then
	{
	reg bx=0
	reg ax=1112h
	}
    else reg ax=svm

    int 10h
    }

proc vlocate(vy,vx)
    {
    locpos=vy*video_width*2+vx*2
    }

;==============================================================================

proc setup_page
    {
    colour col_back:cls

    vlocate(0,0):colour col_top1
    print "FastBase Screen Reports v1.05";
    colour col_top2
    print "                  Escape=Exit  P=Print  V=Vga 50*80";
    vlocate(1,0):colour col_top3
    print "Use the Arrow keys to view different parts of the report.       F=Find  T=132*60";
    cursor 1,video_width-1

    for y=2 to video_lines+1
    vlocate(y,video_width-1):video[locpos]=col_scroll*256+20h
    next y
    }

;==============================================================================

proc print_file
    {
    colour 32
    vlocate(9,30):print "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿";
    vlocate(10,30):print "³ Printing :   0%  ³";
    vlocate(11,30):print "³ Status   :       ³";
    vlocate(12,30):print "³                  ³";
    vlocate(13,30):print "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ";

    seek #1,0

    #open 01000010b
    open #10,"lpt1"
    fad=0

    print_buffer:
    if key=27 then goto exit_print
    read_len=read #1,video_fsize to fbuf
    write #10,read_len from fbuf

    fad+=read_len
    n32=fad*100
    n32/=file_size
    n=low n32
    d=digits n
    vlocate(10,43)
    while d<3 print " ";:d++
    print n;

    if read_len=video_fsize then goto print_buffer

    exit_print:
    close #10
    }

;==============================================================================

proc find_string(fs_manual)
    {
    if fs_manual then
	{
	open window window_find
	vlocate(12,12)
	findl=ext_string(findstr,40)
	if findl<2 then close window:return
	}
    if peekb findstr<' ' then goto find_exit

    m=findstr
    findl=0
    while (peekb m<>0) and (findl<40)
	{
	pokeb m,lcase peekb m
	m++:findl++
	}

    ;search from current point, middle of the screen
    fad=secondline
    findlast=fad

    find_next_block:
    if key=27 then beep:goto find_exit

    ;show search cursor
    vlocate(marky,video_width-1):video[locpos]=col_scroll*256+20h
    n32=(video_lines-1)*fad+(video_lines-2)
    if (high end_of_file) or low end_of_file then n32/=end_of_file
    else n32=0
    marky=2+low n32:if marky>(video_lines+1) then marky=video_lines+1
    vlocate(marky,video_width-1):video[locpos]=col_place*256+178

    seek #1,fad
    read_len=read #1,video_fsize to fbuf
    if read_len=0 then beep:goto find_exit
    fsm=0

    while fsm<read_len
	{
	fsc=peekb (fbuf+fsm)
	if (fsc=10) or (fsc=13) then findlast=fad+1

	len=findl:fsx=findstr:fsz=fbuf+fsm
	while len
	    {
	    c=lcase peekb fsz
	    if c<>peekb fsx then goto find_skip_char
	    fsz++:len--:fsx++
	    }

	;found string
	topscr=findlast
	goto find_exit

	find_skip_char:
	fsm++:fad++

	;if buffer is full load next buffer before getting to end
	;this is to avoid stuffing up searches over buffer boundaries
	if read_len=video_fsize then if (fsm+50)>read_len then goto find_next_block
	}
    goto find_next_block

    find_exit:
    if fs_manual then close window
    }

;==============================================================================

proc display_file
    {
    open_file
    get_eof
    topscr=0
    marky=2
    column=0

    redisplay:
    if video_lines=23 then end_of_file=end_of_file23
    else if video_lines=48 then end_of_file=end_of_file48
    else end_of_file=end_of_file58

    n32=end_of_file-topscr
    n=high n32
    if n<0 then topscr=end_of_file

    vlocate(marky,video_width-1):video[locpos]=col_scroll*256+20h
    n32=(video_lines-1)*topscr+(video_lines-2)
    if (high end_of_file) or low end_of_file then n32/=end_of_file
    else n32=0
    marky=2+low n32:if marky>(video_lines+1) then marky=video_lines+1
    vlocate(marky,video_width-1):video[locpos]=col_place*256+219
    colour col_back

    seek #1,topscr
    n32=topscr
    m=fbuf+video_fsize

    ;check if current find string?
    findchk=lcase peekb findstr

    for y=2 to video_lines+1
    if y=3 then secondline=n32
    findhighlight=0

    vlocate(y,0)
    col=0-column

    loop_prt:
    if m>=(fbuf+video_fsize) then m=fill_buffer
    c=peekb m:if c<>26 then m++:n32++
    #long
    if c>31 then
	{
	#short
	if findchk then if not findhighlight then
	    {
	    fslen=findl:fsx=findstr:fsz=m-1
	    while fslen
		{
		fsc=lcase peekb fsz
		if fsc<>peekb fsx then goto print_char
		fsz++:fslen--:fsx++
		}
	    findhighlight=findl
	    }

	print_char:
	if (col>=0) and (col<(video_width-1)) then
	    {
	    if findhighlight then
		{
		video[locpos]=col_find*256+c
		findhighlight--
		}
	    else video[locpos]=col_back*256+c
	    locpos+=2
	    }
	col++
	goto loop_prt
	}

    if c=26 then
	{
	c=13		;fill end of line for all lines
	}
    if c=13 then
	{
	fill_end(col_back*256+20h)
	goto next_line
	}
    if c=10 then goto loop_prt ; Skip printing this character.
    if c=12 then
	{
	fill_end(col_page*256+0cdh)
	goto next_line
	}
    if c<>9 then goto print_char
    ncol=(col and 0fff8h)+8
    if col<0 then col=ncol
    else
	{
	repeat ncol-col
	    {
	    if col<(video_width-1) then video[locpos]=col_back*256+' ':locpos+=2
	    col++
	    }
	}
    goto loop_prt

    next_line:
    next y

    botscr=n32

;==============================================================================

    same_page:

    wait for keypressed
    ks=keyscan:s=high ks:k=low ks

    if (s=1) or (s=78) then goto finish_display

    if s=71 then topscr=0:column=0:goto redisplay
    if s=72 then backline(1):goto redisplay
    if s=73 then backline(video_lines):goto redisplay

    if s=79 then
	{
	topscr=end_of_file
	column=0
	goto redisplay
	}
    if s=80 then topscr=secondline:goto redisplay
    if searchb 3 from next_page_keys for s then topscr=botscr:goto redisplay

    if (s=75) or (ks=3840) then if column>7 then
	{
	column-=8
	goto redisplay
	}
    if (s=77) or (ks=3849) then if column<1000 then
	{
	column+=8
	goto redisplay
	}
    if s=115 then if column>0 then
	{
	column-=80:if column<0 then column=0
	goto redisplay
	}
    if s=116 then if column<920 then
	{
	column+=80
	goto redisplay
	}

    if s=25 then
	{
	print_file	       ;p for print?
	goto redisplay
	}

    if s=47 then if not mono then
	{
	if video_lines<>48 then
	    {
	    set_video(12h,80,50)
	    }
	else
	    {
	    set_video(3,80,25)
	    }
	setup_page
	goto redisplay
	}

    if s=20 then if not mono then
	{
	if video_lines<>58 then
	    {
	    set_video(56h,132,60)
	    }
	else
	    {
	    set_video(3,80,25)
	    }
	setup_page
	goto redisplay
	}

    if s=33 then	;find? 23-01-96
	{
	if k=6 then find_string(0)
	else find_string(1)
	goto redisplay
	}

    goto same_page

    finish_display:
    close #1
    }

;==============================================================================

setup_page
display_file

if video_lines<>23 then colour 7:screen 3
cls
stop

;==============================================================================

next_page_keys: datab 28,81,57		;enter, pgdn, space

window_find:
datab 0,0,3,10,53,14,113
datab 22,2,2,'Find :',26

findstr: space 41
