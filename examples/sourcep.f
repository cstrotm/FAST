
;source print program
;syntax: sourcep filenames > output_file

;generates output with filename, line number, data

#include cmd_line.fi

buffer=allocate 32000/16
var line_no

proc print_line_info
    {
    plm=name
    pll=0
    while peekb plm pll++:plm++

    pl2=pll:while pl2<12 print bios " ";:pl2++

    plm=name
    while peekb plm print bios chr lcase(peekb plm);:plm++

    print bios " ";
    if line_no<1000 then print bios "0";
    if line_no<100  then print bios "0";
    if line_no<10   then print bios "0";
    print bios line_no;"  ";
    }

if cmd_nos then
    {
    l=cmd_len(1)
    moveb l from cmd_add(1) to name+2
    pokeb name+2+l,0			;end marker
    }
else
    {
    print bios "Syntax: sourcep filename > output"
    print bios
    stop
    }

find first name+2
goto entry

forever
    {
    #errors off
    find next
    if error then stop
    #errors on
    entry:
    moveb 13 from dta segment|dta offset+30 to name

    #open 01000000b ;read only, other users can do all
    open #1,name
    line_no=1

    print_line_info
    thirteens=0

    file_loop:
    len=read #1,32000 to buffer|0
    m=0
    while m<len
	{
	c=buffer[m]b:m++
	if (c=13) or ((c=10) and (thirteens=0))  then
	    {
	    if c=13 then thirteens=1
	    line_no++
	    print bios
	    print_line_info
	    }
	else if c>31 then print bios chr c;
	}

    getn:
    if len=32000 then goto file_loop
    close #1

    print bios
    print bios

    test break
    }

;==============================================================================

name:
string 64

