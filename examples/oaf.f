;(s16.66H
;Compress ascii file with lots of repetition and blank lines.
;This is ideal for accounting system branch transfer files (see FastBase).

;(c) Peter Campbell.

;Create .OAF files using the following compression.
;Read in each line (cyclic table of 110 lines).

;As read each line - can it be found in existing table?
;  no -> store in new file, store in cyclic table.
;  yes -> store 130+n to represent line n.

;Use extra testing for blank lines (ie: cr,lf), store 240+r to represent
;from 1-15 blank lines.

;05-12-1994 Oaf automatically overwrites the existing file when expanding
;	    oaf.log file is created in current directory listing all work
;

#errors off
#short
#include cmd_line.fi

const oaf_version=1	;version 1-255

const max_lines=110	;maximum
const max_width=100
const max_word=20,max_words=256

var line_no,line_nos,line_start
var oaf_compress

buffer ? 256
buffer_lines ? max_lines*2
buffer_data ? max_lines*max_width   ;maximum line length to store
work1 ? 10

;var word_start,word_nos
;word_table ? max_word*max_words

var nlen,blank_count
var32 n32,file_size,file_read,file_write

unsigned input_count,input_size,bufferm
const input_max=4096
input_area ? input_max

unsigned output_count
const output_max=4096
output_area ? output_max

const oaf_zero=127	;"00"
const oaf_char=128	;character follows >=127
const oaf_part=129	;number of line follows, then length
const oaf_line=130	;line number 0-109 (130-239)
const oaf_crlf=240	;cr/lf count (240-255)

;== routines ==================================================================

proc print_to(ptm,ptn)
    {
    pokeb ptm,(ptn/10)+'0'
    pokeb ptm+1,(ptn mod 10)+'0'
    }

proc write_log(wlf)
    {
    open #10,"oaf.log"
    if error then
	{
	if error<>2 then return
	create #10,"oaf.log"
	}
    else
	{
	seek #10,eof
	}

    m=command_line:n=logline+16:l=20
    while l
	{
	c=peekb m
	if c<>13 then m++ else c=' '
	pokeb n,c
	n++:l--
	}

    m=wlf:n=logline+36:l=30
    while l
	{
	c=peekb m
	if c then m++ else c=' '
	pokeb n,c
	n++:l--
	}

    dos 2ah
    rday=low reg dx
    rmonth=high reg dx
    ryear=reg cx-1900
    print_to(logline,rday)
    print_to(logline+3,rmonth)
    print_to(logline+6,ryear)

    dos 2ch
    rhour=high reg cx
    rminute=low reg cx
    print_to(logline+9,rhour)
    print_to(logline+12,rminute)

    write #10,68 from logline
    close #10
    }

;==============================================================================

function setup_oafname
    {
    pokeb filename+nlen,0
    moveb nlen from filename to oafname
    s=searchb nlen from oafname for '.'
    if not s then s=oafname+nlen
    moveb 5 from oaf_ext to s
    }

function print_asciiz(m)
    {
    while peekb m print bios chr peekb m;:m++
    }

proc abort(abort_message)
    {
    print bios
    print bios "ABORT ERROR = ";
    print_asciiz(abort_message)
    print bios
    beep
    write_log(abort_message)
    stop
    }

;==============================================================================

proc clear_lines	;setup work area for lines
    {
    line_nos=0:line_start=0
    input_count=input_max
    input_size=input_max
    output_count=0

    word_nos=0:word_start=0
    }

;==============================================================================

function getc
    {
    if input_count>=input_size then
	{
	input_size=read #1,input_max to input_area
	file_read+=input_size
	input_count=0
	}

    if input_count>=input_size then return -1

    c=peekb (input_area+input_count)
    input_count++
    return c
    }

;==============================================================================

function put_output(output_count)
    {
    write #2,output_count from output_area
    file_write+=output_count
    output_count=0
    }

function putc(c)
    {
    if output_count>=output_max then put_output(output_count)

    pokeb output_area+output_count,c
    output_count++
    }

;==============================================================================

function read_line	;read next line into "buffer", blen - return blen ok?
    {
    blen=0
    bufferm=buffer

    forever
	{
	c=getc
	if c=-1 then return blen

	#long
	if c<>10 then
	    {
	    if oaf_compress then
		{
		if c>=127 then pokeb bufferm,oaf_char:bufferm++:blen++
		if c='0' then
		    {
		    if blen then if peekb (bufferm-1)='0' then
			{
			pokeb bufferm-1,oaf_zero
			goto skip_zeros
			}
		    }
		}
	    else
		{
		if c=oaf_char then
		    {
		    c=getc	;next character (>=127)
		    }
		else if c=oaf_zero then
		    {
		    pokeb bufferm,'0':bufferm++:blen++
		    c='0'
		    }
		}
	    pokeb bufferm,c:bufferm++
	    blen++
	    skip_zeros:
	    }
	#short

	if c=13 then return blen
	if c=26 then return blen
	}
    }

;==============================================================================

function find_line(flm,fll)
    {
    line_compare=0:line_clen=4
    line_no=0
    line_s=line_start
    if line_nos=0 then return 0

    repeat line_nos
	{
	len=peek (buffer_lines+line_s*2)

	area=buffer_data+line_s*max_width
	fc=compareb len at flm with area

	if not fc then
	    {
	    if len=fll then return line_no+1	;found!
	    fc=len
	    }
	else fc-=flm

	if fc>line_clen then
	    {
	    line_clen=fc
	    ll=0	;find true expansion length
	    while fc
		{
		c=peekb area:area++:fc--
		if c=oaf_zero then ll++
		else if c=oaf_char then
		    {
		    area++:fc--
		    }
		ll++
		}
	    line_compare=line_no+1
	    line_clen2=ll
	    }

	line_no++
	line_s++:if line_s>=line_nos then line_s=0
	}

    return 0
    }

;==============================================================================

function store_line(flm,fll)
    {
    line_s=line_start+line_nos
    if line_s>=max_lines then
	{
	line_s-=max_lines
	line_start++
	if line_start>=max_lines then line_start-=max_lines
	}

    if line_nos<max_lines then line_nos++

    #long
    if (line_s<0) or (line_s>=max_lines) then
	{
	#short
	print bios
	print bios "OAF STORE ERROR!"
	print bios "<status> line_s=";line_s;"=";line_start;"+";line_nos
	print bios
	print bios "Contact FastBase - severe error!"
	print bios "--> press 'X' to exit ... ";
	beep
	wait for lcase key='x'
	abort("line_s")
	}

    poke buffer_lines+line_s*2,fll
    moveb blen from flm to buffer_data+line_s*max_width
    }

;==============================================================================

function expand_line(elc,ell)
    {
    line_s=line_start+elc
    if line_s>=max_lines then line_s-=max_lines

    fl=peek (buffer_lines+line_s*2)
    fm=buffer_data+line_s*max_width

    if fl>ell then fl=ell
    while fl
	{
	putc(peekb fm)
	fm++:fl--
	}
    }

;==============================================================================

function store_blanks
    {
    putc(oaf_crlf+blank_count-1)
    blank_count=0
    }

;==============================================================================

function compress_filename
    {
    oaf_compress=1
    setup_oafname
    print bios "compress ";:print_asciiz(filename)

    open #1,filename
    if error then abort("Can't open source file.")

    seek #1,eof:rax=reg ax:rdx=reg dx
    file_size=(rdx*65536)+rax
    seek #1,0

    n32=(file_size+1023)/1024
    print bios " (";low n32;"K) to ";

    print_asciiz(oafname)

    create #2,oafname
    if error then abort("Can't create 'oaf' file.")

    clear_lines
    blank_count=0
    file_read=0
    file_write=0

    putc('O')
    putc(oaf_version)	    ;store version number

    curtoloc:locpos+=2

    while read_line
	{
	#long
	if (blen=1) and (peekb buffer=13) then
	    {
	    if blank_count>=16 then store_blanks
	    blank_count++
	    }
	else
	    {
	    if blank_count then store_blanks

	    ;find in buffer?
	    fline=find_line(buffer,blen)
	    if fline then
		{
		;yes - store table pointer
		fline--
		putc(oaf_line+fline)
		}
	    else
		{
		;no - store buffer in file and table
		if line_compare then
		    {
		    putc(oaf_part)
		    putc(line_compare-1)
		    putc(line_clen2)
;		    print bios "lc=";line_compare;" len=";line_clen;" blen=";blen
		    bufferm=buffer+line_clen
		    blen-=line_clen
		    }
		else
		    {
		    if blen<=max_width then store_line(buffer,blen)
		    bufferm=buffer
		    }
		while blen
		    {
		    putc(peekb bufferm)
		    bufferm++:blen--
		    }
		}
	    }
	#short
	}

    if blank_count then store_blanks
    put_output(output_count)	    ;write output buffer
    close #1,#2

    ;print summary
    n32=(file_write+1023)/1024
    print bios " (";low n32;"K) = ";
    n32=(file_write*100)/file_size
    print bios low n32;"%"

    write_log("COMPRESS OK")
    }

;==============================================================================

function expand_filename
    {
    oaf_compress=0
    setup_oafname
    print bios "expand ";:print_asciiz(oafname)
    print bios " --> ";:print_asciiz(filename)
    print bios

    open #1,oafname
    if error then abort("Can't open 'oaf' file.")

    ;create file - overwrite if it exists
    create #2,filename
    if error then abort("Can't create file for expansion.")

    clear_lines
    blank_count=0
    file_read=0
    file_write=0

    c=getc:if c<>'O' then abort("Invalid File Type")
    c=getc:if c<>oaf_version then abort("Wrong OAF Version")

    curtoloc:locpos+=2

    read_oaf:
    c=getc

    #long
    if c=-1 then goto expand_end
    if c=oaf_part then
	{
	lc=getc:ll=getc
	expand_line(lc,ll)
	save_line=0
	goto read_eol
	}
    else if c>=oaf_crlf then
	{
	repeat 1+c-oaf_crlf
	    {
	    putc(13)
	    putc(10)
	    }
	}
    else if c>=oaf_line then
	{
	expand_line(c-oaf_line,999)
	}
    else
	{
	save_line=1	;get line - store in table
	input_count--

	read_eol:
	read_line
	if peekb (buffer+blen-1)=13 then pokeb buffer+blen,10:blen++

	if save_line then store_line(buffer,blen)
	bufferm=buffer
	while blen
	    {
	    putc(peekb bufferm)
	    bufferm++:blen--
	    }
	}
    #short

    goto read_oaf

    expand_end:
    put_output(output_count)	    ;write output buffer

    close #1,#2
    write_log("EXPAND OK")
    }

;== main ======================================================================

print bios "OAF: Optimised ASCII File compression - v1.10 (c) Peter Campbell Software 1994"
print bios

move 32 from 81h to command_line

#long
if cmd_nos<>2 then
    {
    #short
    bad_options:
    write_log("Command error")
    print bios "command error: oaf filename c/e"
    print bios
    print bios "   oaf filename.ext c  - compress filename.ext to filename.oaf"
    print bios "   oaf filename.ext e  - expand filename.oaf to filename.ext"
    beep:stop
    }

nlen=cmd_len(1)
moveb nlen from cmd_add(1) to filename

if cmd_len(2)<>1 then goto bad_options
option=lcase peekb cmd_add(2)

if option='c' then compress_filename
else if option='e' then expand_filename
else goto bad_options

stop

;== data ======================================================================

filename: space 125
oafname: space 125
oaf_ext: fname '.oaf'

logline:
datab 'dd/mm/yy hh:mm  command line        Message                       ',13,10

command_line: space 80
