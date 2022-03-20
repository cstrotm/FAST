;
; Print files with special control characters.
;
;08/06/91 Prints multiple files and sorts them (eg *.HLP)
;	  The whole file is printed now - not selected pages.
;	  Added options ^H ^F ^# ^l

; ^#	= prints the page no
; ^B	= bold
; ^b	= bold OFF
; ^D	= draft
; ^F	= footer, any text after the ^F becomes the footer
; ^H	= header, any text after the ^H becomes the header
; ^I	= italics
; ^i	= italics OFF
; ^Lnn	= Lines per page, eg L66
; ^lnn	= Printed lines per page, eg l55 (default=55)
; ^N	= near letter quality
; ^P	= page length = 15 inches
; ^U	= underline
; ^u	= underline OFF
; ^W	= double width
; ^^	= ^
;
; nb: - Options are reset at end of each line except ^N ^D ^F ^H
;     - ^H should be set in the first place you want it cos it will
;	be printed when defined and at every page break.
;     - ^F can be set anywhere before the first page break, the footer is only
;	printed when a page exceeds 55 lines of text or a page break's printed.
;

unsigned m,header,footer,last_footer,cp,pline,lines
#inpend=0
#include fsort.fi

proc print_header
    {
    if header=-1 then return
    push m
    m=header
    printing_hf=1
    gosub print_hf_data
    lprint cr lf
    pop m
    printing_hf=0
    }

proc print_footer(pf)
    {
    if last_footer=cp then return

    if footer<>-1 then
	{
	while pline<=lines lprint:pline++
	push m
	m=footer
	printing_hf=1
	gosub print_hf_data
	pop m
	printing_hf=0
	}

    if pf then
	{
	last_footer=cp
	pline=0
	cp++:print bios cp;" ";
	}
    lprint ff;
    if pf then print_header
    }

print bios
print bios "        PANPRT Multifile printing program."
print bios "Prints ASCII text files with embedded control codes."
print bios
bseg=allocate 4096
fmem ? 1536*13

print bios "File(s) to print? ";
inputs filen
print bios
if peekb (filen+2)=0 then stop

lines=55
totlines=66

dir filen+2,fmem
files=peek fmem
print bios cr lf files;" file(s) to print..." cr lf
if files=0 then stop

fm=fmem+2
while files
    {
    cp=1
    fill 32768 from bseg|0 with 1a1ah
    load fm,bseg|0
    m=fm:while peekb m print bios chr peekb m;:m++
    print bios ", page 1 ";
    header=-1
    footer=-1:last_footer=0
    printing_hf=0
    pline=1

    m=0
    forever
	{
	print_hf_data:
	wait_busy:
	if key=27 then
	    {
	    print bios cr lf lf "Aborted!"
	    stop
	    }
	reg dx=0,ax=200h:int 17h
	st=high reg ax
	if st<127 then goto wait_busy

	c=bseg[m]b:m++
	if printing_hf then if (c=13) or (c=12) then return
	if c=10 then goto next_char
	if c=26 then goto complete
	if c=12 then print_footer(1):goto next_char
	if c='^' then
	    {
	    o=bseg[m]b:m++
	    if o=26 then goto complete
	    if o='B' then lprint chr 27;chr 'E';
	    if o='b' then lprint chr 27;chr 'F';
	    if o='D' then lprint chr 27;chr 'P';
	    if o='I' then lprint chr 27;chr '4';
	    if o='i' then lprint chr 27;chr '5';
	    if o='N' then lprint chr 27;chr 'n';
	    if o='P' then lprint chr 27;chr 'C';chr 0;chr 15;
	    if o='U' then lprint chr 27;chr '-';chr 1;
	    if o='u' then lprint chr 27;chr '-';chr 0;
	    if o='W' then lprint chr 14;
	    if o='H' then header=m
	    if o='F' then footer=m:m=1+searchb 200 from bseg|m for 13
	    if o='#' then lprint cp;
	    if o='L' then
		{
		o=bseg[m]b:m++
		n=10*(o-'0')
		o=bseg[m]b:m++
		n+=o-'0'
		lprint chr 27;chr 67;chr n;
		totlines=n
		if lines>(totlines-4) then lines=totlines-4
		}
	    if o='l' then
		{
		o=bseg[m]b:m++
		n=10*(o-'0')
		o=bseg[m]b:m++
		n+=o-'0'
		if n>=5 then lines=n
		if lines>(totlines-4) then lines=totlines-4
		}
	    if o='^' then lprint "^";
	    }
	else
	    {
	    if c=13 then
		{
		lprint chr 27;chr '-';chr 0;
		lprint chr 27;chr 'F';
		pline++
		if pline>lines then print_footer(1)
		else lprint
		}
	    else lprint chr c;
	    }
	next_char:
	}

    complete:
    print_footer(0)

    end_print:
    print bios "ok."
    fm+=13
    files--
    }

print bios
print bios "Printing is complete."
stop

;===========================================================================

filen: string 30
