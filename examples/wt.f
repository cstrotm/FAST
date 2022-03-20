
;The word processor written entirely in FAST.
;(c) Peter Campbell Software; (original wt.com from 24-09-87 to ... 11-03-97)

#errors off
#window memory 4000
#include extinput.fi
#include fsort.fi
#inpend=0
ext_inpend=0

const colour_editing=1	    ;colour editing?

const max_buffers=40
const info_len=70	    ;file buffers; see put_details, get_details
const info_size=info_len*max_buffers

const name_len=40

name_bak     ? name_len+6
buffer	     ? 260
sbuffer      ? 260
ebuffer      ? 260
block_type   ? 2
work	     ? 7700	    ;used for storing procedures and other stuff (350)
startup_files ? 20

;global variables
var text_sizek,text_seg
var line_seg
var current_line,current_column,xpos,ypos
var line_line,next_line,last_line,last_column,end_line
var block_start,block_end,origin_blockl
var edit_line,insert
var current_buffer,buffers

unsigned buffer_lines

var32 n32
var32 text_size,text_free,text_addr
var32 file_start,file_end

;==============================================================================

mono_override=0
m=81h
while peekb m<>13
    {
    c=peekb m:m++
    if c='#' then mono_override=-1
    }

if mono or mono_override then
    {
    colour_background=15
    colour_select1=112
    colour_select2=15
    colour_help=15
    colour_message=112
    colour_block=112
    colour_top=112
    colour_name=112
    colour_modified=15
    colour_changed=15

    colour_number=15
    colour_upper=15
    colour_normal=7
    colour_directive=9
    colour_word=15
    colour_error=9
    colour_fuck=143
    colour_comment=15

    pokeb win_select_buffer+6,7
    pokeb files_menu+6,7
    pokeb default_menu+6,7
    pokeb select_procedures+6,112
    pokeb direct_window+6,112
    }
else
    {
    colour_background=31
    colour_select1=79
    colour_select2=27
    colour_help=23
    colour_block=78
    colour_top=48
    colour_name=63
    colour_modified=79
    colour_changed=29

    colour_number=27
    colour_upper=30
    colour_normal=23
    colour_directive=26
    colour_word=31
    colour_error=28
    colour_fuck=158
    colour_comment=63
    }

;== screen details ============================================================

proc set_video(width,depth,set_screen_mode)
    {
    lineb=width*2
    width1=width-1
    smiddle=depth/2-1

    if depth=25 then
	{
	if set_screen_mode then
	    {
	    if mono then screen 7
	    else screen 3
	    }
	}
    else if depth=50 then
	{
	reg bx=0
	reg ax=1112h
	int 10h
	}
    else
	{
	screen 56h
	}

    fscreen_cols1=width
    fscreen_cols2=width*2
    fscreen_rows1=depth
    fscreen_size=fscreen_rows1*fscreen_cols2
    }

proc slocate(sly,slx)
    {
    locpos=sly*lineb+slx*2
    }

;comment out two of these lines, leaving desired video mode active

;set_video(132,60,1)	  ;see change_video (alt-v)
;set_video(80,50,1)
set_video(80,25,0)	    ;don't set "screen" - causes annoying flicker?

;==============================================================================

#if colour_editing

word_buffer ? 10
const max_words=600

word_seg=allocate (max_words*10)/16+1
word_seg2=allocate (max_words*10)/16+1

fbc_words=0
fill max_words*5 from word_seg|0 with 2020h
fill max_words*5 from word_seg2|0 with 1a1ah

;#errors off
load "wt.lan",word_seg2|1,max_words*10
if error then
    {
    load "..\wt.lan",word_seg2|1,max_words*10
    if error then goto no_words
    }

m=1

find_next_word:
c=word_seg2[m]b
mo=m
m=searchb 200 from word_seg2|m for 13
if (c>='a') and (c<='z') then
    {
    if fbc_words>=max_words then beep:goto no_words
    moveb m-mo from word_seg2|mo to word_seg|fbc_words*10
    fbc_words++
    }
if m then
    {
    m++
    if word_seg2[m]b=10 then m++
    goto find_next_word
    }

sort(word_seg,0,10,fbc_words)

no_words:
deallocate word_seg2
#endif

;==============================================================================

goto skip_undef

seperators:
datab ' !^*()-=+\|.,<>/?',39,'":;[]'
sept_end:
const septs=sept_end-seperators

skip_undef:

proc print_to(ptm,ptn)
    {
    pokeb ptm,(ptn/10)+'0'
    pokeb ptm+1,(ptn mod 10)+'0'
    }

proc write_log(wlf)
    {
    #open 01000010b
    open #10,logfile	    ;read/write
    if error then
	{
	if error<>2 then return
	create #10,logfile
	}
    else
	{
	reg bx=handle #10,cx=-1,dx=-1:dos 42h(2)
	n=read #10,1 to work:if peekb work=26
	    then reg bx=handle #10,cx=-1,dx=-1:dos 42h(2)
	}

    m=wlf
    n=logline+15
    repeat 19
	{
	c=ucase peekb m
	if c then m++ else c=' '
	pokeb n,c:n++
	}

    dos 2ah
    rday=low reg dx
    rmonth=high reg dx

    ryear=reg cx-1900
    while ryear>=100 ryear-=100

    print_to(logline,rday)
    print_to(logline+3,rmonth)
    print_to(logline+6,ryear)
    dos 2ch
    rhour=high reg cx
    rminute=low reg cx
    print_to(logline+9,rhour)
    print_to(logline+12,rminute)

    write #10,35 from logline
;   write #10,firstlen from firstline
    write #10,2 from logeof
    close #10
    }

proc bottoms
    {
    cursor depth-1,0
    print bios
    }

procedure abort
    {
    if fscreen_rows1 <> 25 then set_video(80,25,1)
    bottoms
    stop
    }

proc input_message(imc)
    {
    if mono or mono_override then imc=colour_message
    pokeb input_window+6,imc
    open window input_window
    colour imc
    slocate(20,6)
    }

on error
    {
    bottoms
    error msg "\dos.err"
    print bios "!"
    stop
    }

proc wt_error(we)	;not severe DOS errors also
    {
    open window severe	;*windows sets error#
    colour peekb (severe+6)
    slocate(13,12)
    if we<1000 then
	{
	loctocur
	error msg "\dos.err",we
	print bios "!"
	}
    else prints we,0
    beep
    wait for key=27
    close window
    return
    }

proc inx  xpos+=current_column:current_column=0

;==============================================================================

function yesno(default)
    {
    forever
	{
	c=ucase key
	if c='Y' then print bios "Y";:return 1
	if c='N' then print bios "N";:return 0
	if c=27  then return 2
	if c=13  then print bios chr 'N'+default*11;:return default
	}
    }

function confirm(conf_win)
    {
    open window conf_win
    cursor peekb (conf_win+3)+2,peekb (conf_win+4)-2
    beep
    conf=yesno(0)
    close window
    if conf=1 then return 1
    return 0
    }

;==============================================================================

proc get_memory
    {
    info_seg=allocate info_size/16+1
    fillb info_size from info_seg|0 with 0

    x=allocate 64000			;how much free memory?
    text_sizek=(reg bx/64)-100		;all memory less 100K
    if text_sizek<10 then error 8	;insufficient memory

    text_seg=allocate text_sizek*64
    text_size=text_sizek*1024
    text_free=0
    }

;==============================================================================

function read_config(rc_def,rc_min,rc_max,rc_word)
    {
    rc_value=rc_def

    rc_len=0:m=rc_word
    while peekb m rc_len++:m++

    ;search 'work' for rc_word, find? get value
    m=work
    while peekb m<>1ah
	{
	if peekb m=peekb rc_word then
	    {
	    if not compareb rc_len at m with rc_word then goto read_value
	    }
	m++
	}
    goto skip_value

    read_value:
    m+=rc_len
    if peekb m<>'=' then goto skip_value
    m++

    if rc_def=-1 then	    ;string (-1,length,address,"word")
	{
	rc_len=rc_min
	while (rc_len>0) and (peekb m>' ')
	    {
	    pokeb rc_max,ucase peekb m
	    rc_len--
	    rc_max++:m++
	    }
	pokeb rc_max,0
	return 1
	}

    rc=lcase peekb m
    if rc='y' then pokeb m,'1'  ;substitute Y for 1
    if rc='n' then pokeb m,'0'  ;substitute N for 0

    rc_value=0
    while (peekb m>='0') and (peekb m<='9')
	{
	rc_value=rc_value*10+(peekb m-'0')
	m++
	}

    skip_value:
    if rc_def=-1 then return 0	    ;string

    ;check limits
    if rc_value<rc_min then rc_value=rc_min
    if rc_value>rc_max then rc_value=rc_max
    return rc_value
    }

proc get_config
    {
    fill 1024 from work with 1a1ah
    load "wt.cfg",work,2000
    pokeb startup_files,0

    if (peekb work<>1ah) and (peekb (work+10)=1ah) then
	{
	backups=peekb work
	stab=peekb (work+1)
	split_enter=peekb (work+2)
	comments=peekb (work+6)
	keep_tab=peekb (work+7)
	make_log=peekb (work+8)
	moveb 3 from work+3 to default_ext+2

	find_default=0
	}
    else
	{
	backups=read_config(1,0,1,"backups")
	make_log=read_config(1,0,1,"create_log")
	keep_tab=read_config(1,0,1,"keep_tabs")
	split_enter=read_config(0,0,1,"line_split")
	find_default=read_config(1,0,1,"find_all")

	stab=read_config(8,1,32,"tab_size")
	comments=read_config(1,0,3,"comments")

	;default extension = 3 chars, 0 end marker
	if read_config(-1,3,work,"extension")
	then moveb 4 from work to default_ext+2

	if read_config(-1,10,work,"startup_files")
	then moveb 11 from work to startup_files
	}
    }

procedure print_name
    {
    colour colour_name
    slocate(1,2)
    repeat 40 print " ";
    slocate(1,2)
    nm=name+2	 ;Print name of file in ucase case with extension.
    while peekb nm
	{
	pokeb nm,ucase peekb nm
	print chr peekb nm;:nm++
	}
    }

;==============================================================================

proc change_video
    {
    if fscreen_rows1=25 then set_video(80,50,1)
    else if fscreen_rows1=50 then set_video(132,60,1)
    else set_video(80,25,1)
    }

;==============================================================================

function end_of_line
    {
    start=buffer+254
    while start>=buffer
	{
	#short
	if peekb start<>' ' then goto end_found
	#long
	start--
	}
    end_found:
    return 1+start-buffer
    }

function compress_buffer
    {
    cb_kt=keep_tab
    est=end_of_line+buffer
    st=buffer
    f=sbuffer
    x=0
    while st<est
	{
	b=peekb st
	if cb_kt and (b=' ') then
	    {
	    bl=(x and 248)+8
	    if (bl-x)=1 then goto spput
	    flag=1
	    for a=1 to bl-x
	      if peekb (st+a-1)<>' ' then goto spput
	    next a
	    b=9:st+=a-2:x=bl-1
	    }
	if (b=''') or (b='"') then cb_kt=0
	spput:
	pokeb f,b:f++
	x++:st++
	}
    poke f,0a0dh
    pokeb f+2,1ah
    return f+2-(sbuffer)
    }

;==============================================================================

;line_seg is a structure; line_seg[buffer_lines][4]
; the 4 bytes are 3 bytes for the text_address and one for the line length

;find_line = line number (0...)

;returns length and text_addr = address (if length <> 0)

function find_text(find_line)
    {
    fl=find_line*4
    ft1=line_seg[fl]b
    ft2=line_seg[fl+1]
    text_addr=(ft1*65536)+ft2

    return line_seg[fl+3]b	;line length
    }

proc increase_buffer_lines
    {
    if buffer_lines>=16000 then
	{
	wt_error(e_lines)
	return 0
	}

    old_seg=line_seg:old_lines=buffer_lines

    buffer_lines+=1000
    line_seg=allocate buffer_lines/4
    if error then
	{
	wt_error(error)
	line_seg=old_seg	;restore old values
	buffer_lines=old_lines
	return 0
	}

    fill buffer_lines*2 from line_seg|0 with 0	;clear line_seg
    move old_lines*2 from old_seg|0 to line_seg|0

    deallocate old_seg

    return buffer_lines
    }

;==============================================================================

;store_line    = line number (0...)
;store_pointer = pointer to 32 bit address, 0 means make new address
;store_length  = line length (inclusive)

;return 0 if unable to store text (out of memory)

function store_text(store_line,store_pointer,store_length)
    {
    ;first ensure there is enough lines allocate (line_seg[buffer_lines])
    while store_line>=buffer_lines
	{
	if not increase_buffer_lines then return 0
	}

    text_addr=peek store_pointer

    sl=store_line*4
    sllen=line_seg[sl+3]b	;line length; 0?

    if store_length>sllen then
	{
	;check enough memory?
	n32=text_free+store_length
	if32 n32>=text_size then
	    {
	    wt_error(e_memory)
	    return 0
	    }

	;calculate address and move data to text segment
	n32=text_free/16
	seg=text_seg+low n32
	off=(low text_free) and 15
	moveb store_length from sbuffer to seg|off

	;update text line pointer
	line_seg[sl]b=high text_free
	line_seg[sl+1]=low text_free
	line_seg[sl+3]b=store_length
	text_free+=store_length
	}
    else
	{
	;retrieve line address and calculate seg|off
	sl1=line_seg[sl]b
	sl2=line_seg[sl+1]
	text_addr=(sl1*65536)+sl2
	n32=text_addr/16
	seg=text_seg+low n32
	off=(low text_addr) and 15

	;store line and update new length
	moveb store_length from sbuffer to seg|off
	line_seg[sl+3]b=store_length
	}

    return 1
    }

;==============================================================================

proc insert_lines(ins_line,ins_count)
    {
    while (end_line+ins_count)>=buffer_lines
	{
	if not increase_buffer_lines then return
	}

    if end_line>ins_line then
    move (end_line-ins_line)*2 from line_seg|ins_line*4 to line_seg|(ins_line+ins_count)*4
    fill ins_count*2 from line_seg|ins_line*4 with 0
    end_line+=ins_count

    if block_end>=ins_line then block_end+=ins_count
    if origin_blockl>=ins_line then origin_blockl+=ins_count

    last_line=-1
    }

;==============================================================================

proc put_line
    {
    if (line_line<>-1) and edit_line then
	{
	line_len=compress_buffer

	find_text(line_line)	    ;find current text_addr
	store_text(line_line,.text_addr,line_len)

	edit_file=1
	edit_line=0
	}
    }

procedure put_details
    {
    put_line
    pos=current_buffer*info_len

    info_seg[pos]b=xpos
    info_seg[pos+1]b=ypos
    info_seg[pos+2]=current_line
    info_seg[pos+4]=current_column
    info_seg[pos+6]=end_line
    info_seg[pos+8]=line_seg
    info_seg[pos+10]=buffer_lines
    info_seg[pos+12]b=edit_file

    moveb 55 from name+2 to info_seg|pos+13
    }

procedure get_details
    {
    if not buffers then return

    pos=current_buffer*info_len

    xpos=info_seg[pos]b
    ypos=info_seg[pos+1]b
    current_line=info_seg[pos+2]
    current_column=info_seg[pos+4]
    end_line=info_seg[pos+6]
    line_seg=info_seg[pos+8]
    buffer_lines=info_seg[pos+10]
    edit_file=info_seg[pos+12]b

    moveb 55 from info_seg|pos+13 to name+2

    last_line=-1
    line_line=-1
    print_name
    }

;==============================================================================

proc word_default
    {
    pokeb default_menu+1,0
    opt=1
    open window default_menu
    wd_back:
    colour peekb (default_menu+6)
    slocate(4,58):m=default_ext+2:while peekb m print chr ucase peek m;:m++
    print "  ";
    slocate(5,58):if backups then print "ON "; else print "OFF";
    slocate(6,58):if split_enter then print "ON "; else print "OFF";
    slocate(7,58):printb stab;"  ";
    slocate(8,56)
    if comments=0 then print ";=======";
    else if comments=1 then print "/*====*/";
    else if comments=2 then print "// =====";
    else if comments=3 then print "#======#";
    slocate(9,58):if keep_tab then print "YES"; else print "NO ";
    slocate(10,58):if make_log then print "YES"; else print "NO ";
    slocate(11,58):if find_default then print "YES"; else print "NO ";
    pokeb default_menu+1,8

    opt=select default_menu,opt
    if not opt then close window:return
    if opt=1 then
	{
	open window def_input
	slocate(10,69):colour 60h
	l=ext_input(default_ext)
	close window
	}
    if opt=2 then backups=not backups
    if opt=3 then split_enter=not split_enter
    if opt=4 then
	{
	open window tab_input
	push stab
	cursor 7,67
	stab=inputb
	if not stab then pop stab else pop x
	if stab>20 then stab=20
	if stab<1 then stab=1
	close window
	}
    if opt=5 then comments++:if comments>3 then comments=0
    if opt=6 then keep_tab=not keep_tab
    if opt=7 then make_log=not make_log
    if opt=8 then find_default=not find_default
    goto wd_back
    }

function first_nonblank
    {
    fbx=0
    while (fbx<255) and (peekb (buffer+fbx)=' ') fbx++
    if fbx=255 then fbx=0
    return fbx
    }

proc reset_block
    {
    column_block=0
    line_block=0
    last_line=-1	;force page re-display (colours)
    }

proc mark_columns(mcx)
    {
    put_line
    reset_block
    column_block=1
    origin_blockl=line_line
    origin_blockx=mcx
    }

proc home  current_column=0:xpos=0

proc top_of_file  current_line=0:ypos=0:home

proc put_char(pchr)
    {
    pcx=current_column+xpos
    if pcx<255 then
	{
	if insert then moveb 255-pcx from pcx+buffer to pcx+1+buffer
	pokeb buffer+pcx,pchr
	xpos++
	edit_line=1
	}
    }

function sept(sx)
    {
    sbyte=peekb (buffer+sx)
    return searchb septs from seperators for sbyte
    }

function word_left(wx)
    {
    x=wx
    if x then x--
    if sept(x) then
	{
	while x>=0
	    {
	    if not sept(x) then goto wl2
	    x--
	    }
	return 0
	}

    wl2:
    while x>=0
	{
	if sept(x) then return x+1
	x--
	}
    return 0
    }

function word_right(wx)
    {
    x=wx
    while x<256
	{
	if sept(x) then goto wr2
	x++
	}
    goto wr_end

    wr2:
    while x<256
	{
	if not sept(x) then return x
	x++
	}

    wr_end:
    if x>=end_of_line then return end_of_line
    return wx
    }


proc del(xd) moveb 255-xd from buffer+xd+1 to buffer+xd:edit_line=1

proc back(xd)
    {
    if xd then
	{
	if insert then moveb 256-xd from buffer+xd to buffer-1+xd
	else pokeb buffer+xd-1,' '
	xpos--
	}
    edit_line=1
    }

proc clear_line fill 130 from buffer with 2020h:home:edit_line=1

proc clear_eol
    {
    pcx=current_column+xpos
    fillb 256-pcx from buffer+pcx with 20h
    edit_line=1
    }

procedure screen_display
    {
    colour colour_background:cls:slocate(0,0)
    colour colour_top
    print " WT v2.51 (c) Peter Campbell Software 1996.      F1-Help   Files   Defaults     ";
    slocate(1,0):repeat width print " ";
    }

proc parameters
    {
    colour colour_top
    slocate(1,45):repeat 30 print " ";
    slocate(1,45):print "x";1+current_column+xpos;
    slocate(1,50):print "Line ";current_line+ypos+1;" of ";end_line;

    slocate(1,70)
    n32=text_size-text_free
    n32/=1024
    kfree=low n32;
    if kfree<100 then print " ";
    if kfree<10 then print " ";
    print kfree;"K free";
    }

;==============================================================================

proc split_line(splitx,splits)
    {
    fill 128 from ebuffer with 2020h
    moveb 256-splitx from buffer+splitx to ebuffer+splits
    clear_eol
    put_line

    line_line++
    insert_lines(line_line,1)

    move 128 from ebuffer to buffer
    edit_line=1
    }

proc centre_line
    {
    x=(current_line+ypos)-smiddle
    if x>0 then current_line=x:ypos=smiddle
    }

;==============================================================================

proc get_line(gl)
    {
    if line_line=gl then return
    put_line

    line_line=gl

    ;get line address (data_seg, data_off, data_len)
    len=find_text(line_line)
    n32=text_addr/16
    seg=text_seg+low n32
    off=(low text_addr) and 15

    fill 130 from buffer with 2020h
    col=0
    edit_line=0

    while (col<255) and (len>0)
	{
	byte=seg[off]b
	off++:len--

	if (byte=13) or (byte=26) then return
	if byte=9 then col=(col and 248)+8
	else
	    {
	    pokeb buffer+col,byte
	    col++
	    }
	}

    ;lines can't get truncated anymore, only split automatically
    }

;==============================================================================

proc get_date
    {
    dos 2ah(0)
    dt=low reg ax
    current_day=low reg dx
    current_month=high reg dx
    current_year=reg cx
    }

proc print_two(number)
    {
    n=(number/10):if n>=10 then n+=7
    put_char(n+'0')

    n=number mod 10:if n>=10 then n+=7
    put_char(n+'0')
    }

;==============================================================================

;find all "proc"'s and display names for selection
;select a proc and goto it

proc select_procedure(spq)
    {
    if spq then goto item_selected

    sel_proc1:
    open window select_lookup
    m=txt_proc
    for y=0 to 9
	slocate(y+4,7)
	m++
	colour 47
	print chr ucase peekb m;" = ";
	m++
	colour 32
	repeat 7 print chr peekb m;:m++
    next y

    wait for keypressed
    kk=keyscan:spq=ucase low kk
    close window

    if (kk=17408) or (kk=4096) then goto sel_proc1

    item_selected:
    mselect=txt_proc
    repeat 10
	{
	if spq=peekb (mselect+1) then goto item_selected2
	mselect+=9
	}
    return

    item_selected2:
    last_proc=spq
    mlen=peekb mselect:mselect+=2

    mprocs=work:sprocs=0

    if spq='A' then     ;** Anystr **
	{
	input_message(15)
	print "Lines starting: ";
	startchrsl=ext_input(startchrs)
	c=0:if startchrsl then c=peekb(startchrs+2)
	startchrsl--
	close window
	if c=0 then return 0
	mselect=startchrs+2:mlen=startchrsl
	}

    put_line

    proc_line=0
    while proc_line<end_line
	{
	get_line(proc_line)
	sskip=buffer

	while (peekb sskip=' ') or (peekb sskip=9) sskip++

	if spq='X' then     ;** Labels **
	    {
	    ;if assembly langauge then a label starts at column 0, no colon
	    f=searchb 50 from name+2 for '.'
	    spa=ucase peekb (f+1)
	    sps=ucase peekb (f+2)
	    spm=ucase peekb (f+3)
	    if spa='A' then if sps='S' then if spm='M' then
		{
		;is first character on line a letter?
		c=ucase peekb buffer
		if (c<'A') or (c>'Z') then goto nxt_item
		goto item_found
		}

	    kp=sskip
	    while (peekb kp>' ') kp++
	    if (peekb (kp-1)=':') then goto item_found
	    sskip=kp:goto nxt_item
	    }

	if not compareb mlen at mselect with sskip then
	    {
	    sskip+=mlen
	    if not spq='A' then while peekb sskip>' ' sskip++

	    item_found:
	    while (peekb sskip=' ') or (peekb sskip=9) sskip++
	    move 10 from sskip to mprocs
	    poke mprocs+20,proc_line
	    mprocs+=22:sprocs++
	    if sprocs>=350 then goto show_procedures
	    }

	nxt_item:
	proc_line++
	}

    show_procedures:
    if sprocs=0 then beep:return

    sort(reg ds,work,22,sprocs)
    open window select_procedures
    if spq='A' then
	{
	locate 5,17
	colour 48
	m=mselect:while peekb m>=' ' print chr peekb m;:m++
	}
    else locate 5,26:print chr peekb (mselect-1)
    sitem=0

    select_loop:
    sb=(sitem/15)*15
    m=work+sb*22
    for y=0 to 14
	slocate(y+6,17)
	a=m
	repeat 20
	    {
	    if sb=sitem then colour colour_select2 else colour colour_top
	    c=peekb a
	    if (c>' ') and (sb<sprocs) then print chr c;:a++
	    else print " ";:if (spq='A') and (c<>13) then a++
	    }
	if (a=m) and (sb<sprocs) then locate y+6,17:print "end_of_line";
	m+=22:sb++
    next y

    wait for keypressed
    ks=keyscan:s=high ks:k=lcase low ks
    if (ks=17408) or (ks=4096) then
      {
      close window
      goto sel_proc1
      }
    if s=1 then close window:return

    if s=71 then sitem=0
    if s=72 then sitem--
    if s=73 then sitem-=15
    if s=79 then sitem=sprocs-1
    if s=80 then sitem++
    if s=81 then sitem+=15
    if sitem<0 then sitem=0
    if sitem>=sprocs then sitem=sprocs-1

    if (k>='a') and (k<='z') then
	{
	repeat sprocs
	    {
	    sitem++
	    if sitem>=sprocs then sitem=0
	    mprocs=work+sitem*22
	    if (lcase peekb mprocs)=k then goto select_loop
	    }
	}

    if s=28 then
	{
	current_line=peek (work+sitem*22+20)
	close window
	current_column=0:xpos=0
	ypos=0
	if (spq='A') or (spq='X') then centre_line
	return
	}

    goto select_loop

    txt_proc:	datab 4,'Pproc   '
    txt_func:	datab 4,'Ffunc   '
    txt_record: datab 6,'Rrecord '
    txt_index:	datab 5,'Iindex  '
    txt_local:	datab 5,'Llocal  '
    txt_call:	datab 4,'Ccall   '
    txt_goto:	datab 4,'Ggoto   '
    txt_cmd:	datab 7,'Ocommand'
    txt_any:	datab 6,'Aanystr '
    txt_lbl:	datab 6,'Xlabels '
    }

;==============================================================================

proc delete_block(dbstart,dbend)	;line numbers
    {
    put_line
    if dbstart>dbend then swap dbstart,dbend
    del_lines=(dbend-dbstart)+1

    if dbstart>=end_line then beep:return
    if dbend>end_line then beep:return

    move (end_line-dbend)*2 from line_seg|(dbend+1)*4 to line_seg|dbstart*4

    end_line-=del_lines
    fill del_lines*2 from line_seg|end_line*4 with 0

    if dbstart<origin_blockl then
	origin_blockl-=del_lines:if carry then reset_block

    if current_line>dbstart then
	current_line-=del_lines:if carry then current_line=0

    last_line=-1
    line_line=-1:edit_file=1
    }

function scrap(do)
    {
    input_message(49):print "Saving block...";

    if line_block or column_block then
	{
	block_start=origin_blockl
	block_end=line_line
	if block_start>block_end then swap block_start,block_end
	}
    else
	{
	block_start=line_line
	block_end=line_line
	}

    create #1,block_name:if error then wt_error(error):return

    if column_block then    ;write header; 2 bytes = 0,width
	{
	x1=origin_blockx
	xc=xpos+current_column
	if xc<x1 then swap x1,xc
	scrap_len=1+xc-x1
	pokeb block_type,0:pokeb block_type+1,scrap_len
	write #1,2 from block_type
	}

    block_line=block_start
    while block_line<=block_end
	{
	get_line(block_line)

	if column_block then
	    {
	    write #1,scrap_len from buffer+x1
	    if error then wt_error(error):close #1:return
	    if do then moveb 256-x1-scrap_len
		from buffer+x1+scrap_len to buffer+x1:edit_line=1
	    }
	else
	    {
	    line_len=compress_buffer
	    write #1,line_len from sbuffer
	    if error then close #1:wt_error(error):return
	    }

	block_line++
	}

    if not column_block then if do then
	{
	delete_block(block_start,block_end)
	}

    close #1
    close window
    reset_block
    return 1
    }

proc join_line(xj)
    {
    line_number=line_line
    x=end_of_line
    if xj<x then xj=x
    move 128 from buffer to ebuffer
    get_line(line_number+1)
    moveb 256-xj from buffer to ebuffer+xj
    delete_block(line_number+1,line_number+1)
    move 128 from ebuffer to buffer
    line_line=line_number

    edit_line=1
    last_line=-1
    }

proc shift_left
    {
    current_column=0
    xpos=first_nonblank
    xn=(xpos/stab)*stab-stab
    if xn<0 then xn=0
    if xpos then
	{
	moveb 256-xpos from buffer+xpos to buffer+xn
	edit_line=1
	}
    }

proc shift_right
    {
    current_column=0
    xpos=first_nonblank
    xn=((xpos+stab)/stab)*stab
    moveb 256-xn from buffer+xpos to buffer+xn
    fillb xn-xpos from buffer+xpos with ' '
    edit_line=1
    }

;==============================================================================

function check_if_comment(po)
    {
    if comments=0 then if c=';' then return -1
    if comments=1 then if (c='/') and (peekb po='*') then return -1
    if comments=2 then if (c='/') and (peekb po='/') then return -1
    if comments=3 then return 0     ;leave comments with "#" in green
    return 0
    }

;==============================================================================

proc print_text_line
    {
    po=buffer+current_column	    ;print offset

    #if colour_editing
    ecover=0
    ecomment=0	;comments like ; or /* ... */ or // ...
    equote1=0	;quote 'text'
    equote2=0	;quote "text"
    spx=0	;skip pointer for next word

    ;print_change flag determines if line has been edited/changed
    print_change=0

    if print_change then ec_normal=colour_changed
    else ec_normal=colour_normal

    for px=0 to width-1
	c=peekb po
	if searchb 4 from eol_characters for c then c=' '
	else po++:md++

	if c=9 then
	    {
	    pn=(px and 248)+8
	    ec=colour_background
	    if ecomment>1 then ec=colour_comment
	    while px<pn
		{
		video[locpos]=' '+ec*256
		locpos+=2:px++
		}
	    px--
	    }
	else
	    {
	    ;check for quotes
	    if ecomment then goto skip_quotes
	    if ((c=''') and (peekb po<>'s')) or (equote1<>0) then
		{
		if c=''' then equote1=not equote1
		ec=ec_normal
		goto display_c_colour
		}
	    else if (c='"') or (equote2<>0) then
		{
		if c='"' then equote2=not equote2
		ec=ec_normal
		goto display_c_colour
		}

	    skip_quotes:
	    ;check for comments - colour comments 15-03-96
	    if ecomment=0 then
		{
		;determine if comment?
		ecomment=check_if_comment(po)
		}

	    if ecomment then
		{
		;determine if end of comment?
		if comments=1 then
		    {
		    if (c='*') and (peekb po='/') then ecomment=3
		    }
		ecomment--
		if ecomment then
		    {
		    ec=colour_comment
		    goto display_c_colour
		    }
		}

	    if ecover=0 then
		{
		if c='#' then ecover=colour_directive
		else if c<>' ' then ecover=1
		}
	    if ecover>1 then ec=ecover
	    else
		{
		if (c>='0') and (c<='9') then ec=colour_number
		else if (c>='A') and (c<='Z') and (peekb po<'a') then ec=colour_upper
		else if (c>='a') and (c<='z') and (px>=spx) then
		    {
		    ppl=0:ppm=po-1:ppw=word_buffer
		    spx=px
		    while ppl<10
			{
			ppc=peekb ppm
			if ((ppc>='a') and (ppc<='z')) or (ppc='_') then ppm++:spx++
			else ppc=' '
			pokeb ppw,ppc
			ppw++:ppl++
			}
		    fc=findbin(word_buffer,word_seg,0,fbc_words,10,10)
		    if fc then
			{
			ec=colour_word
			if peek word_buffer='r'*256+'e' then
			    if peek (word_buffer+2)='o'*256+'r' then
				ec=colour_error
			if peek word_buffer='u'*256+'f' then
			    if peek (word_buffer+2)='k'*256+'c' then
				ec=colour_fuck
			}
		    else ec=ec_normal
		    }
		else if px>=spx then ec=ec_normal
		}

	    display_c_colour:
	    video[locpos]=c+ec*256

	    locpos+=2
	    }
    next px

    c=peekb po:po++:md++
    if searchb 4 from eol_characters for c then
	{
	if c=26 then md--
	else if peekb po=10 then md++
	}

    if print_change then    ;set last character on line = purple
	{
	c=(video[locpos-1]b and 15)+80
	video[locpos-1]b=c
	}

    locpos-=lineb

    #else

    if width=80 then md=printm po,80,0
    else md=printm po,132,0

    #endif

    if line_block or column_block then
	{
	;does line need some colour?
	newy1=origin_blockl
	newy2=current_line+ypos
	if newy1>newy2 then swap newy1,newy2

	if line_block then
	    {
	    newx1=0
	    newx2=width1
	    }
	else
	    {
	    newx1=origin_blockx-current_column
	    newx2=xpos
	    if newx1<0 then newx1=0
	    if newx1>width1 then newx1=width1
	    if newx1>newx2 then swap newx1,newx2
	    }

	if line_line<newy1 then return
	if line_line>newy2 then return

	locpos+=newx1*2
	while newx1<=newx2
	    {
	    video[locpos+1]b=colour_block
	    locpos+=2
	    newx1++
	    }
	}
    }

;==============================================================================

procedure page_display
    {
    colour colour_background

    for yp=2 to depth-1

    if (current_line+yp-2)>=buffer_lines then
	{
	fill 128 from buffer with 2020h
	line_line=-1
	}
    else
	{
	get_line(current_line+yp-2)
	}

    slocate(yp,0)
    print_text_line

    next yp

    last_line=current_line	    ;set_old
    last_column=current_column
    }

procedure print_line(py)
    {
    slocate(2+py,0)
    get_line(current_line+py)
    last_ypos=py
    print_text_line
    }

;== start/load buffer =========================================================

;load_name; asciiz file
;load_action; 1=normal, 2=scrap

function load_buffer(load_name,load_action)
    {
    if load_action=1 then
    if buffers>=max_buffers then wt_error(e_maxfiles):return 0

    #open 01000000b
    open #1,load_name	    ;read only
    if error then
	{
	if error<>2 then wt_error(error)
	if confirm(win_newfile) then
	    {
	    create #1,load_name
	    if error then
		{
		wt_error(error)
		return 0
		}
	    }
	else return 0
	}

    file_start=text_free	;start address for line store
    file_end=text_free

    load_loop:
    ;check enough memory to load 20K chunk?
    n32=file_end+20256		;256 extra bytes
    if32 n32>=text_size then
	{
	wt_error(e_memory)
	;temporary - load what we have so far 22-01-97
	if (high file_end) or (low file_end) then goto load_continue
	close #1:return 0
	}

    ;setup segment/offset and load 20K
    n32=file_end/16
    seg=text_seg+low n32
    off=(low file_end) and 15
    r=read #1,20000 to seg|off
    if error then wt_error(error):close #1:return 0
    file_end+=r
    if r=20000 then goto load_loop

    load_continue:

    close #1
    fill 128 from seg|off+r with ' '    ;extra clear space for "search" below

    ;now store/count lines...
    buffer_lines=1000	    ;minimum lines, goes up in increments of 1000?
    line_seg=allocate buffer_lines/4
    if error then wt_error(error):close #1:return 0
    fill buffer_lines*2 from line_seg|0 with 0	;clear line_seg

    end_line=0
    store_buffer_lines:
    if32 file_start<file_end then
	{
	;find line length
	len=0
	n32=file_start/16
	seg=text_seg+low n32
	off=(low file_start) and 15

	x=searchb 255 from seg|off for 13
	if x then
	    {
	    x++
	    if seg[x]b=10 then x++
	    len=x-off
	    }
	else
	    {
	    x=searchb 255 from seg|off for 10
	    if x=0 then
		{
		;default max line, no end of line marker
		n32=file_end-file_start
		if32 n32>255 then len=255
		else len=low n32
		}
	    else
		{
		x++
		len=x-off
		}
	    }

	moveb len from seg|off to sbuffer   ;move to/from
	if store_text(end_line,.file_start,len) then
	    {
	    end_line++
	    file_start+=len
	    goto store_buffer_lines
	    }
	}

    if load_action=2 then return 1

    current_buffer=buffers
    buffers++

    ;start_buffer
    xpos=0:ypos=0
    current_line=0
    current_column=0

    edit_file=0:edit_line=0
    last_line=-1:line_line=-1
    put_details:print_name
    line_block=0:column_block=0
    last_proc=0

    return 1
    }

;==============================================================================

function get_directory(load_auto)
    {
    dir_seg=allocate 1024:if error then wt_error(error):return 0
    dir name+2,dir_seg|0:files=dir_seg[0]
    if not files then
	{
	none_dir:
	wt_error(2)

	exit_dir:
	deallocate dir_seg
	return 0
	}

    if not sort(dir_seg,2,13,files) then goto none_dir
    x=0:lx=-1

    open window direct_window
    colour peekb (direct_window+6)
    slocate(23,3):print " ";files;" file(s) ";

    forever
	{
	if (x/60)<>lx then
	    {
	    start=(x/60)*60
	    lx=x/60
	    px=2:py=11

	    while py<23
		{
		slocate(py,px):print "            ";:slocate(py,px)
		if start<files then
		    {
		    st=(start*13)+2
		    while dir_seg[st]b print chr dir_seg[st]b;:st++
		    start++
		    }
		px+=16:if px>75 then px=2:py++
		}
	    }

	if load_auto then goto dir_load_auto

	py=(x mod 60)/5:px=x mod 5
	slocate(py+11,px*16+2):st=locpos+1:old=st
	repeat 12 video[st]b=colour_select2:st+=2

	wait for keypressed:ks=keyscan:s=high ks:k=low ks
	if s=1 then close window:goto exit_dir
	if s=72 then x-=5
	if s=80 then x+=5
	if s=75 then x--
	if (s=77) or (s=15) then x++
	if s=73 then x-=60
	if s=81 then x+=60
	if (s=71) or (x<0) then x=0
	if (s=79) or (x>=files) then x=files-1

	st=old:repeat 12 video[st]b=peekb (direct_window+6):st+=2

	if k=13 then
	    {
	    ;if directory is "C:\FBASE\AS\*.F" then put name after last "\"
	    nm=name+2:nd=nm
	    while peekb nm
		{
		if (peekb nm=':') or (peekb nm='\') then nd=nm+1
		nm++
		}

	    move 7 from dir_seg|x*13+2 to nd
	    close window
	    deallocate dir_seg
	    return load_buffer(name+2,1)
	    }
	else if ks=7178 then	;ctrl-enter? load all selected files
	    {
	    dir_load_auto:
	    nm=name+2:nd=nm
	    while peekb nm
		{
		if (peekb nm=':') or (peekb nm='\') then nd=nm+1
		nm++
		}

	    for dirx=0 to files-1
		move 7 from dir_seg|dirx*13+2 to nd
		if not load_buffer(name+2,1) then goto stop_loading
	    next dirx

	    stop_loading:
	    close window
	    deallocate dir_seg
	    current_buffer=0	;display first buffer
	    return buffers
	    }
	}
    }

proc load_file(use_filename,load_auto)
    {
    retry_load:
    open window input_load_name
    colour peekb (input_load_name+6)
    slocate(13,62):print "[.";
    st=default_ext+2
    while peekb st print chr(ucase peek st);:st++
    print "]";

    slocate(13,13)

    if use_filename then
	{
	l=1	;dummy length, not 0
	}
    else
	{
	l=ext_clean_input(name)
	}

    if (peekb (name+2)=0) or (l=0) then
	{
	close window
	get_details
	return 0
	}

    nm=name+2:cd=0
    while peekb nm cd+=peekb nm='.':nm++
    if not cd then
	{
	pokeb nm,'.'
	move 2 from default_ext+2 to nm+1
	}

    slocate(13,13):nm=name+2
    while peekb nm print chr(ucase peek nm);:nm++

    wild=0
    nm=name+2
    while peekb nm
	{
	b=peekb nm
	if (b='*') or (b='?') then wild=1
	nm++
	}
    if wild then
	{
	close window
	x=get_directory(load_auto)
	get_details
	if x then return 1  ;load 1 or multiple buffers?
	use_filename=0
	goto retry_load
	}

    x=load_buffer(name+2,1)
    close window
    if not buffers then abort
    get_details
    return x
    }

;==============================================================================

proc display_buffers(db_y)
    {
    db_y=(db_y/15)*15
    start=db_y*info_len
    db_row=6

    repeat 15
	{
	slocate(db_row,7)

	last_char=' '
	if db_y>=buffers then colour peekb (win_select_buffer+6)
	else
	    {
	    if info_seg[start+12]b then last_char='*'
	    if db_y=y then colour colour_select2:cursor db_row,7
	    else if info_seg[start+12]b then colour colour_modified
	    else colour peekb (win_select_buffer+6)
	    }

	pos=start+13
	print " ";
	repeat 29
	    {
	    db_c=info_seg[pos]b
	    if db_c then pos++ else db_c=' '
	    if db_y>=buffers then db_c=' '

	    print chr lcase db_c;
	    }
	print chr last_char;" ";

	start+=info_len
	db_row++:db_y++
	}
    }

function select_buffer
    {
    open window win_select_buffer
    y=current_buffer

    forever
	{
	display_buffers(y)

	wait for keypressed
	db_ks=keyscan:db_s=high db_ks:db_k=low db_ks

	if db_k=27 then close window:return 0
	if db_k=13 then
	    {
	    close window
	    current_buffer=y
	    get_details
	    return 1
	    }

	if db_s=72 then y--
	if db_s=80 then y++
	if db_s=73 then y-=15
	if db_s=81 then y+=15
	if db_s=71 then y=0
	if db_s=79 then y=buffers-1

	if y>=buffers then y=buffers-1
	if y<0 then y=0
	}
    }

;==============================================================================

proc set_positions
    {
    ;re-do this crap logic
    while xpos<0
	{
	xpos+=8
	current_column-=8
	if current_column<0 then current_column=0:xpos=0
	}
    while xpos>width1
	{
	xpos-=8
	current_column+=8
	if current_column>176 then current_column=176:xpos=width1
	}
    if ypos<0 then
	{
	ypos=0
	current_line--
	}
    if ypos>(depth-3) then ypos=depth-3:current_line++
    if current_column<0 then current_column=0
    if current_column>176 then current_column=176
    if current_line>=end_line then current_line=end_line-1
    if (current_line+ypos)>=end_line then ypos=(end_line-1)-current_line
    if current_line<0 then current_line=0

    if column_block or line_block then goto page_all

    if (current_line<>last_line) or (current_column<>last_column) then
	{
	if keypressed then
	    {
	    last_line=-1
	    goto exe_key
	    }
	colour colour_background
	if (current_line+1)=last_line then
	    {
	    scroll down 0,2,width1,depth-1,1
	    print_line(0)
	    goto edit_page
	    }
	if (current_line-1)=last_line then if last_line<>-1 then
	    {
	    scroll 0,2,width1,depth-1,1
	    print_line(depth-3)
	    goto edit_page
	    }

	page_all:
	page_display

	edit_page:
	last_line=current_line	    ;set_old
	last_column=current_column
	}

    exe_key:

    get_line(current_line+ypos)     ;only if different
    last_ypos=ypos

    slocate(ypos+2,0)
    print_text_line
    parameters
    if mono or mono_override
	then cursor size 12-(insert*4),13
	else cursor size 6-(insert*2),7
    cursor ypos+2,xpos
    }

;==============================================================================

proc read_columns
    {
    inx
    scrap_len=peekb (block_type+1)
    scrap_line=line_line
    if (scrap_len+xpos)>255 then wt_error(e_scrapwide):return

    forever
	{
	get_line(scrap_line)
	moveb 256-xpos-scrap_len from xpos+buffer to xpos+scrap_len+buffer
	rlen=read #1,scrap_len to buffer+xpos:if error then return
	if rlen<>scrap_len then return
	edit_line=1
	put_line
	scrap_line++
	}
    }

proc input_block_file
    {
    put_line	;save line before inserting, otherwise causes stuff up

    input_message(49):print "Inputting block..."
    #open 01000010b
    open #1,block_name:if error then goto cs_error  ;read/write

    two=read #1,2 to block_type
    if (two<>2) or (error<>0) then goto cs_error

    if not peekb block_type then
	{
	read_columns
	close #1
	}
    else
	{
	;load buffer "wt.ins"
	;insert the new line_seg[] into the current lines buffer
	close #1
	push line_seg,buffer_lines,end_line
	ins_status=load_buffer(block_name,2)
	scrap_seg=line_seg
	scrap_end=end_line
	pop end_line,buffer_lines,line_seg

	if ins_status then
	    {
	    insert_lines(line_line,scrap_end)
	    move scrap_end*2 from scrap_seg|0 to line_seg|line_line*4
	    deallocate scrap_seg
	    }
	}

    close window
    reset_block
    edit_file=1
    return

    cs_error:
    wt_error(error)
    close window
    return
    }

;==============================================================================

function finds(fsx)	;column number offset in "buffer"
    {
    fsx+=buffer
    fsend=buffer+end_of_line
    while fsx<fsend
	{
	fsx=searchb fsend-fsx from fsx for peekb (findstr+2)
	if fsx then
	    {
	    m=findstr+3
	    f2=fsx
	    while peekb m
		{
		#short
		c=peekb m:m++:f2++
		if c='?' then goto fnchar
		if c<>peekb f2 then goto fnext
		fnchar:
		#long
		}
	    current_column=0
	    xpos=fsx-buffer
	    return 1

	    fnext:
	    fsx++
	    }
	else return 0
	}
    return 0
    }

function find_string(find_multi)
    {
    find_buffer=current_buffer

    find_next_buffer:

    if finds(current_column+xpos) then return 1

    find_line=current_line+ypos+1
    while find_line<end_line
	{
	get_line(find_line)
	if finds(0) then
	    {
	    current_line=find_line
	    ypos=0:centre_line
	    return 1
	    }
	find_line++
	}

    ;searched to end, check next file?
    if find_multi then
	{
	put_details
	current_buffer++
	if current_buffer>=buffers then current_buffer=0
	get_details
	if current_buffer<>find_buffer then
	    {
	    ;reset pointers to look from start of buffer
	    top_of_file
	    get_line(current_line)
	    goto find_next_buffer
	    }
	}

    return 0
    }

proc input_find(find_first)
    {
    if find_first then
	{
	input_message(15)
	print "Find: ";
	findl=ext_input(findstr)
	c=0:if findl then c=peekb(findstr+2)
	if c=0 then close window:return 0
	findl--
	}
    else xpos++

    if find_string(find_default) then	;search all buffers? find_default
	{
	if find_first then close window
	return 1
	}
    if find_first then close window

    input_message(12)
    print "Text Not Found";
    if find_default then print " - Searched All Buffers";
    print "!";
    wait for keyscan
    close window
    if not find_first then xpos--
    return 0
    }

proc input_replace(find_first)
    {
    ir=input_find(find_first):if ir=0 then return
    if find_first then
	{
	input_message(14)
	print "Replace: ";
	replacel=ext_input(replacestr)
	c=0:if replacel then c=peekb(replacestr+2)
	close window

	if replacel=0 then return 0	;allow replace to null string

	replacel--
	}

    replace_again:
    if ir then
	{
	last_line=-1:set_positions
	input_message(14)
	print "Replace Yes/No/All or ESC? ";
	wait for keypressed:rk=lcase key:print chr rk;
	replace=0		  ;Default: replace none.
	if rk='y' then replace=1     ;One only.
	if rk='a' then replace=32767    ;Max.
	close window
	if rk=27 then return

	if replace=0 then xpos++:ir=find_string(0)
	while (replace<>0) and (ir<>0)
	    {
	    xx=current_column+xpos	;replace beyond col 80? 11-03-97
	    moveb 256-xx-findl from buffer+xx+findl to buffer+xx+replacel
	    moveb replacel from replacestr+2 to buffer+xx
	    edit_line=1
	    replace--
	    xpos+=replacel:ir=find_string(0)
	    if (replace<>0) and (ir<>0) then set_positions
	    }
	if replace then last_line=-1:set_positions
	goto replace_again
	}
    }

proc print_file
    {
    input_message(49):loctocur
    print bios "Print current Scrap or File? (s/f) ";
    wait for keypressed
    pk=lcase key
    print bios chr pk;" ... ESC aborts.";

    if pk='s' then
	{
	#if 0
	fuck
	lps=allocate 4096:if error then wt_error(8):goto end_lprint
	fill 32768 from lps|0 with 1a1ah
	load block_name,lps|0,65535
	if error then wt_error(error):goto lp_deall
	if lps[0]b then
	    {
	    m=0
	    while lps[m]b<>1ah
		{
		lprint chr lps[m]b;:m++
		if key=27 then goto lp_deall
		}
	    }
	else
	    {
	    scrap_len=lps[1]b
	    m=2:c=scrap_len
	    while lps[m]b<>1ah
		{
		lprint chr lps[m]b;
		m++
		c--
		if c=0 then lprint:c=scrap_len
		}
	    }
	lp_deall:
	deallocate lps
	#endif
	}
    else if pk='f' then
	{
	#open 01000010b
	open #1,"lpt1"      ;this is the same as "save" read/write
	prt_line=0
	while (prt_line<end_line) and (key<>27)
	    {
	    len=find_text(prt_line)
	    if len then
		{
		n32=text_addr/16
		seg=text_seg+low n32
		off=(low text_addr) and 15
		write #1,len from seg|off
		}
	    prt_line++
	    }
	close #1
	}

    end_lprint:
    close window
    }

proc save_file(new_nameq)
    {
    put_line
    if new_nameq then
	{
	input_message(79)
	print "Save file as: ";
	l=ext_input(name)
	close window
	if not l then return
	print_name
	}

    moveb name_len+6 from name+2 to name_bak
    f=searchb name_len+6 from name_bak for '.'
    if not f then f=searchb name_len+6 from name_bak for 0
    if f then moveb 5 from bak_extension to f

    if backups then
	{
	delete name_bak     ;If error then assume doesn't exist.
	rename name+2 to name_bak
	if error>3 then     ;Novell gets "Path Not Found" if doesn't exist?
	    {
	    wt_error(error):return
	    }
	}

    create #1,name+2
    if error then
	{
	wt_error(error):return
	}

    save_line=0
    while save_line<end_line
	{
	len=find_text(save_line)
	if len then
	    {
	    n32=text_addr/16
	    seg=text_seg+low n32
	    off=(low text_addr) and 15
	    write #1,len from seg|off	    ;needs to be faster!!!
	    if error then wt_error(error):close #1:return
	    }
	save_line++
	}

    close #1

    if make_log then write_log(name+2)

    edit_file=0:edit_line=0
    put_details
    }

proc save_alter
    {
    if edit_file then
	{
	open window save_altered_file
	retry_yesno:
	cursor 12,51
	wait_yesno:
	byte=lcase key
	if byte=27 then close window:return
	if (byte<>'y') and (byte<>'n') then goto wait_yesno
	close window
	if byte='y' then save_file(0)
	if byte='n' then edit_file=0
	}
   }

proc exit_file(flag)
    {
    if flag then
	{
	if not select_buffer then return
	save_alter
	if edit_file then return
	}

    parameters
    deallocate line_seg     ;text_seg will remain used!

    if current_buffer<>(buffers-1) then moveb ((buffers-1)-current_buffer)*info_len
       from info_seg|(1+current_buffer)*info_len to info_seg|current_buffer*info_len

    buffers--
    current_buffer=0
    get_details
    if not buffers then
	{
	load_file(0,0)
	if not buffers then abort
	}
    }

proc exit_wordq
    {
    original_buffer=current_buffer

    exit_test:
    eflag=0

    ;count all changed buffers
    for current_buffer=0 to buffers-1
    get_details
    if edit_file then eflag++
    next current_buffer
    if eflag>15 then eflag=15	    ;maximum to display in window
    if eflag=0 then abort	    ;no changed files

    pokeb wind_files+5,eflag+6
    open window wind_files:y=6

    for current_buffer=0 to buffers-1
    get_details
    if edit_file then
	{
	colour colour_modified
	slocate(y,8)
	len=30
	nm=name+2
	while len
	    {
	    c=peekb nm:if c then nm++ else c=' '
	    print chr lcase c;
	    len--
	    }
	y++
	}
    next current_buffer

    beep
    slocate(4,31)
    loctocur
    no_count=0

    no_againq:
    wait for keypressed
    r=ucase key
    if r='N' then
	{
	if no_count then abort
	no_count++:beep:goto no_againq
	}
    close window
    if r<>'Y' then
	{
	current_buffer=original_buffer	;restore original buffer
	get_details
	return
	}

    for current_buffer=0 to buffers-1
    get_details
    if edit_file then save_file(0)
    next current_buffer

    goto exit_test	;re-check all buffers

    exit_abort:
    abort
    }

proc help_me
    {
    colour colour_help
    scroll 0,2,width-1,depth-1,0
    slocate(3,0)
    print "            WT HELP  --  WORD TURBO WRITTEN BY PETER CAMPBELL 1988."
    print
    print " FUNCTION KEYS    ALT-S  Split line               ALT-N  Edit next file"
    print "ÚÄÄÄÄÄÄÂÄÄÄÄÄÄ¿   ALT-J  Join lines               ALT-X  Exit WT (save?)"
    print "³  F1  ³  F2  ³   ALT-U  Undo changed line"
    print "³ HELP ³ EDIT ³   ALT-E  Erase line               ALT-B  Start block mark"
    print "ÃÄÄÄÄÄÄÅÄÄÄÄÄÄ´   ALT-K  Erase to end of line     ALT-A  Mark from beginning"
    print "³  F3  ³  F4  ³   ALT-G  Goto line                ALT-Z  Mark to end"
    print "³ SAVE ³ LOAD ³   ALT-P  Goto Procedure           ALT-W  Mark word"
    print "ÃÄÄÄÄÄÄÅÄÄÄÄÄÄ´   ALT-Q  Goto 'label'             ALT-L  Mark line"
    print "³  F5  ³  F6  ³   ALT-T  Join line (remove gap)   ALT-R  Repeat line"
    print "³ FIND ³ REPL ³"
    print "ÃÄÄÄÄÄÄÅÄÄÄÄÄÄ´   Shft-F5  Find next string       Shft-Minus  Delete to scrap"
    print "³  F7  ³  F8  ³   Shft-F6  Replace next string    Shft-Plus   Copy to scrap"
    print "³ LEFT ³ RGHT ³                                   Shft-Insert Insert scrap"
    print "ÃÄÄÄÄÄÄÅÄÄÄÄÄÄ´   Shft-F7  Move left then down"
    print "³  F9  ³ F10  ³   Shft-F8  Move right then down   CTRL-T    Top of screen"
    print "³ PRNT ³ AltQ ³                                   CTRL-C    Centre screen"
    print "ÀÄÄÄÄÄÄÁÄÄÄÄÄÄÙ   ALF-F3   Save & remove file     CTRL-B    Bottom screen"
    print
    print "CTRL-U Uppercase  CTRL-E   Expand buffer (64K)    CTRL-Y    Delete line"
    print "CTRL-I Lowercase  ALT-Spc  Insert space           CTRL-Bksp Delete Line";

    last_line=-1
    wait for keyscan
    return
    }

proc word_files
    {
    word_start:
    put_details
    sw=menu files_menu:goto word_files2
    forever
	{
	sw=select files_menu,sw
	word_files2:
	if not sw then close window:return
	if sw=1 then exit_file(1):if not buffers then abort
	if sw=2 then exit_wordq
	}
    }

;- main entry ---------------------------------------------------------------

get_memory
get_config
buffers=0
screen_display
insert=1

nm=81h
while peekb nm<>13
    {
    if peekb nm>' ' then
	{
	moveb name_len from nm to name+2
	m=searchb name_len from name+2 for 13
	if m then pokeb m,0
	if not load_file(1,0) then abort
	goto display_entry
	}
    nm++
    }

;startup_files (wt.cfg) command line?
if peekb startup_files then
    {
    moveb 20 from startup_files to name+2
    if not load_file(1,1) then abort
    }
else if not load_file(0,0) then abort

display_entry:
screen_display
page_display
print_name
last_line=-1
line_line=-1

edit:
set_positions

wait for keypressed
ks=keyscan
k=low ks:s=high ks

;Handle keypad cursor movement.
if ks=18688 then current_line-=depth-2:goto edit
if ks=20736 then current_line+=depth-2:goto edit
if peekb 0|417h and 16 then
    {
    if ks=18432 then current_line--:goto edit
    if ks=20480 then current_line++:goto edit
    if ks=19200 then current_column-=stab:goto edit
    if ks=19712 then current_column+=stab:goto edit
    }
else
    {
    if ks=18432 then ypos--
    if ks=20480 then ypos++
    if ks=19200 then xpos--
    if ks=19712 then xpos++
    }
if ks=18176 then home
if ks=20224 then current_column=0:xpos=end_of_line
if ks=30464 then ypos=0
if ks=29952 then ypos=depth-3
if ks=33792 then top_of_file
if ks=30208 then
    {
    current_line=end_line-(depth-2)
    ypos=depth-3:home
    }
if ks=18231 then xpos=first_nonblank:goto edit
if ks=29440 then inx:xpos=word_left(xpos)
if ks=29696 then inx:xpos=word_right(xpos)

;Special character movement functions.
if ks=3840 then xpos=(xpos/stab)*stab-stab
if ks=3849 then
    {
    newx=((xpos+stab)/stab)*stab
    if insert then
	{
	if (newx+current_column)>255 then newx=0
	while newx>xpos
	    {
	    put_char(' ')
	    }
	}
    else xpos=newx
    goto edit
    }
if ks=20992 then insert=not insert
if ks=21248 then del(current_column+xpos)
if ks=3592 then back(current_column+xpos):goto edit
if ks=4608 then clear_line
if ks=9472 then clear_eol

;Line functions.
if k=13 then
    {
    enter_line:
    enx=first_nonblank
    if not split_enter then current_column=0:xpos=end_of_line
    split_line(xpos+current_column,enx)
    ypos++:xpos=enx:current_column=0
    goto edit
    }
if ((peek 0|417h and 8)=8) and (ks=14624) then
    {
    push insert 		;Alt Space bar
    insert=1
    put_char(' '):xpos--
    pop insert
    goto edit
    }
if ks=5632 then 		;ctrl-U; undo changed line
    {
    line_line=-1
    home
    }
if (ks=5401) or (ks=3711) then
    {
    delete_block(line_line,line_line)
    goto edit
    }
if ks=7936 then
    {
    split_line(xpos+current_column,0)
    ypos++:home
    }
if ks=9216 then join_line(xpos+current_column)
if ks=5120 then
    {
    atx=end_of_line+1
    join_line(atx)
    repeat 254
	{
	if peekb(buffer+atx)<>' ' then goto exit_at
	del(atx)
	}
    exit_at:
    }
if ks=16640 then shift_left
if ks=16896 then shift_right
if ks=23040 then shift_left:ypos++:last_line=-1
if ks=23296 then shift_right:ypos++:last_line=-1
if ks=27648 then
    {
    home
    push insert
    insert=1
    if comments=0 then
	{
	put_char(';')
	repeat 78 put_char('=')
	}
    else if comments=1 then
	{
	put_char('/'):put_char('*')
	repeat 75 put_char('=')
	put_char('*'):put_char('/')
	}
    else if comments=2 then
	{
	put_char('/'):put_char('/'):put_char(' ')
	repeat 76 put_char('=')
	}
    else if comments=3 then
	{
	put_char('#')
	repeat 68 put_char('=')
	put_char('#')
	}
    pop insert
    split_line(xpos+current_column,0)
    ypos++:home
    }

;Pop-up menu keys.
if ks=8448 then word_files
if ks=8192 then word_default
if ks=6400 then select_procedure('P'):goto edit         ;ALT-P
if ks=4096 then select_procedure(0):goto edit		;ALT-Q
if ks=17408 then select_procedure(last_proc):goto edit	;F10

;Special ALT/CTRL-key functions.
if (ks=12544) and (buffers>1) then
    {
    put_details
    current_buffer++
    if current_buffer=buffers then current_buffer=0
    get_details
    }
if ks=8704 then     ;alt-g "goto line"
    {
    input_message(113):loctocur
    print bios "Goto line: ";
    curtoloc
    gline=input
    close window
    if gline then
	{
	current_line=gline-1
	ypos=0
	centre_line	;try and centre so line is in middle of screen
	}
    }
if ks=5140 then 	;ctrl-t (top)
    {
    current_line+=ypos
    ypos=0:goto edit
    }
if ks=12290 then	;ctrl-b (bottom)
    {
    current_line-=(depth-3)-ypos
    ypos=depth-3:goto edit
    }
if ks=11779 then	;ctrl-c (centre)
    {
    centre_line
    goto edit
    }

if ks=5653 then
    {
    for m=buffer to buffer+255
    pokeb m,ucase peekb m
    next m
    edit_line=1
    goto edit
    }
if ks=5897 then
    {
    for m=buffer to buffer+255
    pokeb m,lcase peekb m
    next m
    edit_line=1
    goto edit
    }

;File option keys.
if ks=15104 then help_me
if ks=15360 then put_details:b=select_buffer
if ks=15616 then save_file(1)
if ks=15872 then put_details:load_file(0,0):goto edit
if ks=11520 then put_details:exit_wordq
if ks=17152 then put_details:print_file:last_line=-1:line_line=-1
if ks=27136 then
    {
    put_details
    if edit_file then save_file(0)
    exit_file(0)
    goto edit
    }

;Find/replace options.
if ks=16128 then input_find(1)
if ks=16384 then input_replace(1)
if ks=22528 then input_find(0)
if ks=22784 then input_replace(0)

;Blocks
if ks=9728 then
    {
    if line_block then reset_block
    else
	{
	put_line
	reset_block
	line_block=1
	origin_blockx=0
	origin_blockl=line_line
	}
    }
if ks=12288 then
    {
    if column_block then reset_block
    else mark_columns(xpos+current_column)
    }
if ks=20011 then
    {
    if peekb 0|417h and 3 then n=scrap(0):goto edit
    }
if ks=18989 then
    {
    if peekb 0|417h and 3 then n=scrap(1):goto edit
    }

if ks=4864 then     ;alt-r; repeat line
    {
    put_line
    line_line++:ypos++
    insert_lines(line_line,1)
    edit_line=1
    }

if ks=8196 then
    {
    get_date
    print_two(current_day)
    put_char('-')
    print_two(current_month)
    put_char('-')
    print_two(current_year/100)
    print_two(current_year mod 100)
    goto edit
    }

if ks=21040 then input_block_file:goto edit

if ks=12032 then	;alt-v (change video mode)
    {
    change_video
    screen_display
    last_line=-1
    goto edit
    }
if ks=7680 then mark_columns(0)
if ks=11264 then mark_columns(end_of_line)
if ks=4352 then
    {
    inx:x=word_right(xpos):x=word_left(x):xpos=x:mark_columns(x)
    while xpos<256
	{
	if sept(xpos) then xpos--:goto edit
	xpos++
	}
    }

if k then
    {
    if (k<>26) and (k<>10) then put_char(k)
    }
goto edit

;== WT data ================================================================

block_name:	fname 'wt.ins'
logfile:	fname 'wt.log'
bak_extension:	fname '.BAK'

comp_input:	string 50
itext:		string 30
block_name_i:	string 20

eol_code:	datab 13
default_ext:	datab 4,0,'F',0,0,0

direct_window:
datab 0,0,0,10,79,23,63,26

severe:
datab 0,0,10,11,70,16,4fh
datab 22,16,4,'Press ESC'
datab 26

name:
data name_len
space name_len+20

input_load_name:
datab 0,0,11,9,69,16,112
datab 22,2,2,'Enter exact file name or use wild cards for directory.'
datab 26

win_newfile:
datab 0,0,15,14,36,18,79
datab 22,2,2,'New File? (yN)'
datab 26

save_altered_file:
datab 0,0,24,10,55,14,1001111b
datab 22,2,2,'Save changed file? (Y/N)'
datab 26

save_errorw:
datab 0,0,16,7,65,12,1001110b
datab 22,16,2,'Error saving file!'
datab 22,2,3,'Press escape to abort, any other key to retry'
datab 26

files_menu:
datab 1,2,30,2,51,7,32
datab 22,6,1,	  'Files Menu'
datab 22,2,3,'Remove file'
datab 22,2,4,'Exit editor  ALT-X'
datab 26

win_select_buffer:
datab 1,0,6,3,39,21,63
datab 22,10,1,'Select a File'
datab 26

def_input:
datab 0,0,52,8,75,12,60h
datab 22,3,2,'New default : '
datab 26

tab_input:
datab 0,0,55,5,74,9,15
datab 22,2,2,'New tab = '
datab 26

default_menu:
datab 1,0,45,1,70,12,47
datab 22,2,1,'WT Defaults'
datab 22,2,3,'Extension:'
datab 22,2,4,'Backups'
datab 22,2,5,'Line split'
datab 22,2,6,'Tab step = '
datab 22,2,7,'Comments'
datab 22,2,8,'Keeps tabs'
datab 22,2,9,'Create Log'
datab 22,2,10,'Find ALL'
datab 26

input_window:
datab 0,0,4,18,75,22,120
datab 26

config_defaults:
datab 1,4,0,'F',0,0
datab 0,1,0,0

findstr: string 30
replacestr: string 30
startchrs: string 20
space 1

wind_files:
datab 1,0,6,3,39,21,79
datab 22,7,1,'Save Files? (Y/N)'
datab 26

logline: datab '??/??/?? ??:??                     '
logeof:  datab 13,10

e_maxsize:   fname 'Maximum file size exceeded, CTRL-Expand!' ;do automatic!
e_scrapwide: fname 'Scrap to wide for line!'
e_maxfiles:  fname 'Maximum files (40) already loaded!'
e_memory:    fname 'Memory is full, can',39,'t store new text!'
e_lines:     fname 'Buffer contains more than 16000 lines, file truncated!'

select_procedures:
datab 0,0,15,5,38,21,48
datab 26

select_lookup:
datab 0,0,5,3,19,14,32
datab 26

eol_characters:
datab 26,13,10,0
