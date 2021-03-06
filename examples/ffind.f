;Find program for multiple file search.

;one space in the "look for" string will be matched as any number of spaces or
;tabs in the input files. eg: find "mov ax,bx" will find "mov     ax,bx".

;===========================================================================
;ver 5 : modified 25th April 1991
;
;command line: FFIND [-cld] [files] [string]
;-c = case insensitive
;-l = display line before, equal and after each occurrence
;-s = search sub directories - not implemented yet
;
;FFIND now searches the whole file - even greater than 64K
;
;11/03/92   Bug: often skipped lines after finding an occurrence.
;	    Bug: the -l option would loop forever if reporting first or last.
;
;05/09/94   Add -f option: list file names only which have the string
;	    Add -d option: prints date/time of file (previously did always)
;
;13-11-98   When opening files set the mode to "read only" non-exclusive
;===========================================================================

var ok,day,month,year,hour,minute
unsigned lno,slno,m,f

name ? 15
#inpend=0
#short

proc print_two(number)
    {
    number=(number mod 10)+(number/10)*16
    printhb bios number;
    }

proc print_date
    {
    print_two(day)
    print bios "-";
    print_two(month)
    print bios "-";
    print_two(year);
    }

proc print_time
    {
    print_two(hour)
    print bios ":";
    print_two(minute)
    }

on error
    {
    print bios
    error msg "\dos.err"
    print bios "!"
    stop
    }

function print_line(pln,word)
    {
    test break
    if m[word]b=26 then return word
    print bios "  ";pln;" ";
    col=0
    while (m[word]b<>13) and (m[word]b<>10)
	{
	test break	;fuck
	if m[word]b=9 then
	    {
	    xx=(col and 248)+8
	    repeat xx-col print bios " ";
	    col=xx
	    }
	else print bios chr m[word];:col++
	word++
	}
    print bios
    return word
    }

curtoloc
print
print "FFIND v5.3 (c) Peter Campbell Software, PO Box 54-180 Mana, New Zealand."
print
print "FFIND [-cdfl] [files] [string] [>outfile]"
print "  -c = case insensitive."
print "  -d = print date/time of each file."
print "  -f = display file names only."
print "  -l = display lines before & after."
print
loctocur

m=allocate 4096

ocase=0     ;options default
oline=0
find_only=0
date_print=0

n=81h:p=0:s=0
while peekb n<>13
    {
    c=peekb n:n++
    #long
    if c='-' then
	{
	#short
	next_opt:
	c=lcase peekb n
	if c=13 then goto end_of_cmdline
	if c=' ' then n++:goto next_c

	if c='c' then ocase=1:goto opt_ok
	if c='l' then oline=1:goto opt_ok
	if c='f' then find_only=1:goto opt_ok
	if c='d' then date_print=1:goto opt_ok

	print "Invalid command line option: -";chr c
	error 999

	opt_ok:
	n++
	goto next_opt
	}
    #long
    if c>' ' then
	{
	#short
	if p=0 then dest=path+2:p=1 else dest=table+2:s=1
	pokeb dest,c:dest++
	forever
	    {
	    c=peekb n:n++
	    if c=13 then pokeb dest,0:goto end_of_cmdline
	    if (s=0) and (c=' ') then pokeb dest,0:goto next_c
	    pokeb dest,c:dest++
	    }
	}
    next_c:
    }

end_of_cmdline:
if p=0 then
    {
    print "Which files: ";
    loctocur
    inputs path
    if peekb (path+2)=0 then error 999
    print
    }

if s=0 then
    {
    print "Look for: ";
    loctocur
    inputs table
    if peekb (table+2)=0 then error 999
    print
    }

if ocase then	    ;set to lowercase if case insensitive
    {
    n=table+2
    while peekb n pokeb n,lcase peekb n:n++
    }

;== start program here =====================================================

word_found=0

find first path+2
goto entry

loop:
#errors off
find next
#errors on
if error=18 then
    {
    if word_found=0 then
	{
	print bios "Match not found for : ";
	m=table+2
	while peekb m print bios chr peekb m;:m++
	print bios
	stop
	}
    print bios "Ok"
    stop
    }

entry:
moveb 15 from dta segment|dta offset+30 to name
time=peek dta segment|(dta offset+16h)
date=peek dta segment|(dta offset+18h)
curtoloc
print "File: ";
prints name,0:print "        ";

#open 01000000b ;read only, other users can do all
open #1,name:re=read #1,65530 to m|0
m[re]b=26

x=0:first=1:lno=1:slno=0:oslno=0

look:
f=x
while m[f]b<>26
    {
    if f>65000 then	    ;load next chunk of file???
	{
	moveb 65535-slno from m|f to m|0
	f-=slno
	slno=0
	x=searchb 600 from m|0 for 26
	re=x+read #1,65530-x to m|x
	m[re]b=26
	}
    c=m[f]b:if ocase then c=lcase c
    if c=13 then lno++:oslno=slno:slno=f+2
    if c=peekb (table+2) then goto look_found
    f++
    }

finished_file:
close #1
test break
goto next_one

look_found:
ss=f
gosub look_for_name
#long
if ok then
    {
    word_found=1
    if first then
	{
	#short
	print bios "File: ";
	mx=name:while peekb mx print bios chr peek mx;:mx++

	if date_print then
	    {
	    day=date and 31
	    month=(date/32) and 15
	    year=(date/512)+80
	    hour=(time/2048)
	    minute=(time/32) and 63
	    print bios "  ";:print_date
	    print bios "  ";:print_time
	    }

	first=0

	if find_only then goto finished_file	;file names only?

	print bios
	}

    #short
    if oline then if lno>1 then print_line(lno-1,oslno)   ;previous

    f=print_line(lno,slno)		    ;current

    if oline then
	{
	of=f+1:if m[of]b=10 then of++
	print_line(lno+1,of)		    ;next
	print bios
	}
    f--
    }
next_search:
x=f+1
goto look

next_one:
if not first then print bios
goto loop

look_for_name:
ok=0
st=table+2
xs=ss

forever
    {
    next_sb:
    c=m[xs]b:if ocase then c=lcase c
    sb=peekb st
    if sb=0 then ok=1:return
    if sb=' ' then
	{
	if c<>' ' then return
	reano:
	while m[xs]b=' ' xs++
	if m[xs]b=9 then xs++:goto reano
	st++:goto next_sb
	}
    if sb='?' then xs++:st++:goto next_sb
    if c<>peekb st then return
    st++
    xs++
    }

path:
string 65

table:
string 65
