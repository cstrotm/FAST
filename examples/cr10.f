
;==============================================================================
; FAST
; CR - Count Carriage Returns (ie: lines) in the specified file(s)
;==============================================================================

;22-01-93  Use the command line if a parameter is specified
;02-02-93  Print to standard output always (PRINT BIOS)
;14-03-96  count pages also

const p32_bios=1
#include fast32.fi

#inpend=0
#short
var32 lines,tlines,pages,tpages

#include cmd_line.fi

on error
    {
    print bios
    error msg "\dos.err"
    stop
    }
on break error 999

ld=allocate 4096
tlines=0
tpages=0

if cmd_nos then
    {
    l=cmd_len(1)
    moveb l from cmd_add(1) to name+2
    pokeb name+2+l,0			;end marker
    }
else
    {
    print bios "Files: ";
    inputs name
    print bios
    if peekb (name+2)=0 then error 999
    }

print bios
print bios "Counting ..."
print bios

find first name+2
goto entry

forever
    {
    #errors off
    find next
    if error then goto dtotal
    #errors on
    entry:
    moveb 13 from dta segment|dta offset+30 to name

    m=name:l=15
    while l
	{
	c=peekb m:if c then m++ else c=' '
	print bios chr c;
	l--
	}

    open #1,name
    lines=0
    pages=0
    line66=0

    file_loop:
    len=read #1,65534 to ld|0
    m=0
    while m below len
	{
	if ld[m]b=10 then
	    {
	    lines++
	    if line66 then line66--
	    else line66=66:pages++
	    }
	if ld[m]b=12 then pages++
	m++
	}

    getn:
    if len=65534 then goto file_loop
    close #1

    n32=lines:print32(' '):tlines+=lines
    print bios " lines, pp=";
    n32=pages:print32(0):tpages+=pages
    print bios
    test break
    }

dtotal:
print bios
print bios "Total Lines = ";
n32=tlines:print32(0):print bios
print bios "Total Pages = ";
n32=tpages:print32(0)
stop

;==============================================================================

name:
string 14
