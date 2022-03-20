
;==============================================================================
;QUICK Find program for multiple file search. (original program is ffind.f)
;==============================================================================

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
;22/07/93   Convert ffind.f to qf.f
;	    Case sensitive, print matches only, 64K file limit.
;
;===========================================================================

var ok
unsigned m,f,word

;#trace on

name ? 15
#inpend=0
#short

on error
    {
    print bios
    error msg "\dos.err"
    print bios "!"
    stop
    }

function print_line(word)
    {
    test break

;   print bios "[";
;   namem=name
;   while peekb namem print bios chr peekb namem;:namem++
;   print bios "] ";

    while (word>1) and (m[word-1]b<>13) word--
    if m[word]b=10 then word++
    if m[word]b=26 then return word
    col=0
    while (m[word]b<>13) and (m[word]b<>10)
	{
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
print "QUICK FIND v5.2 (c) Peter Campbell Software, PO Box 54-180 Mana, New Zealand."
print
print "QF [files] [string] [>outfile]"
print
loctocur

m=allocate 4096

n=81h:p=0:s=0
while peekb n<>13
    {
    c=peekb n:n++
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

;== start program here =====================================================

word_found=0

find first path+2
goto entry

loop:
#errors off
find next
#errors on
#long
if error=18 then
    {
    #short
    if word_found=0 then
	{
	print bios "Match not found for : ";chr 34;
	mx=table+2
	while peekb mx print bios chr peekb mx;:mx++
	print bios chr 34
	stop
	}
    print bios "Ok"
    stop
    }

entry:
moveb 15 from dta segment|dta offset+30 to name
curtoloc
print "File: ";
prints name,0:print "        ";
open #1,name
re=read #1,65530 to m|1
close #1
first=1

first_char=peekb (table+2)
a=table+2
word_len=0
while peekb a word_len++:a++
f=1

look:
while f<re
    {
    l=re-f
    nf=searchb l from m|f for first_char
    if nf then f=nf:goto look_found
    else f+=l
    }

test break
goto next_one

look_found:
ok=not compareb word_len at table+2 with m|f
if ok then
    {
    if first then
	{
	print bios "File: ";
	mx=name:while peekb mx print bios chr peek mx;:mx++
	print bios:first=0
	}

    f=print_line(f)
    word_found=1
    }
f++
goto look

next_one:
if not first then print bios
goto loop

path:
string 65

table:
string 65
