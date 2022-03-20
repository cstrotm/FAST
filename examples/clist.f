
;== CLIST - LIST PROGRAMS IN TWO COLUMNS ===================================

var columns,lines,wide
const pmax=16384,imax=10240
pbuffer ? pmax
ibuffer ? imax

#short
#inpend=0
on error
    {
    print bios:error msg "\dos.err"
    print bios "!":stop
    }

proc new_buffer
    {
    pline=0:pcol=0:px=0
    fill pmax/2 from pbuffer with 2020h
    }

proc print_buffer
    {
    lprint chr 15;chr 27;chr '0';
    if pline then
	{
	m=pbuffer
	repeat lines
	    {
	    repeat columns
		{
		lprint chr peekb m;:m++
		}
	    lprint
	    }
	lprint ff;
	}
    new_buffer
    }

proc next_col
    {
    pcol+=wide+4:px=0
    if pcol>=columns then print_buffer
    pline=0
    }

proc next_line
    {
    px=0
    pline++
    if pline>=lines then next_col
    }

proc put(p)
    {
    if px>=wide then next_line
    pokeb pbuffer+pline*columns+pcol+px,p
    px++
    }

print bios "CLIST: By Peter Campbell" cr lf
print bios "File(s) ";

inputs name
print bios
if peekb (name+2)=0 then error 999

lines=72
columns=130
wide=(columns/2)-2
if (lines*columns)>pmax then error 11

find first name+2
goto fentry

forever
    {
    find next
    fentry:
    moveb 13 from dta segment|dta offset+30 to name
    open #1,name
    lprint chr 14;
    m=name:while peekb m lprint chr peekb m;:m++
    lprint

    new_buffer
    next_read:
    ilen=read #1,imax to ibuffer
    if ilen=0 then goto innext
    iadd=ibuffer

    while ilen
	{
	c=peekb iadd:ilen--:iadd++
	if c=26 then goto innext
	if c=10 then goto ill
	if c=13 then
	    {
	    next_line
	    goto ill
	    }
	if c=9 then
	    {
	    x=(px and 248)+8
	    repeat x-px put(' ')
	    }
	else put(c)

	ill:
	}
    goto next_read

    innext:
    close #1
    print_buffer
    }

;== DATA ===================================================================

name:
string 20
