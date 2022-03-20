
;== WISDOM Quote Program ===================================================

var32 seeka
work ? 32000
qmem ? 1024
qput ? 1024

on error
    {
    error msg "\dos.err"
    print bios "!"
    stop
    }

proc abort
    {
    close #1
    error 999
    }

proc compress	;transfers qmem to qput
    {
	    si=qmem
	    dl=0:dh=0
    loc_6:  al=peekb si
	    ah=peekb (si+1)
	    si++
	    if al=0 then goto loc_7
	    if al and 0fh then goto loc_6
	    if ah and 0f0h then goto loc_6
	    dl++
    loc_7:  di=qput
	    cl=4
    loc_8:  al=peekb si:si++
	    if dl then goto loc_9
	    al=al/16	;/c
	    si--
	    dl+=2
    loc_9:  dl--
	    al=al and 0fh
	    if dh then goto loc_12
	    if (al and 8)=0 then goto loc_10
	    al=al and 7
	    bx=table1
	    al=peekb (bx+al)
	    pokeb di,al
	    di++
	    goto loc_8
    loc_10: if al then goto loc_11
	    poke di,20h
	    return
    loc_11: dh=al
	    goto loc_8
    loc_12: if dh=1 then goto loc_13
	    dh=dh*16
	    al=al or dh
	    pokeb di,al:di++
	    dh=0
	    goto loc_8
    loc_13: bx=table2
	    al=al*2
	    ah=0
	    bx+=al
	    ax=peek bx
	    poke di,ax:di+=2
	    dh=0
	    goto loc_8
    }

;===========================================================================

open #1,"wis.dat"
r=read #1,10 to work
entries=peek (work+4)
if entries>8000 then entries=8000

if entries=0 then abort

r=read #1,entries*4 to work
curtoloc

m=work:q=1
repeat entries
    {
    print bios "Quote: ";q
    q++
    s=peek m
    n=peekb (m+3)
    seeka=s*16
    #errors off
    seek #1,seeka
    r=read #1,n*16 to qmem
    #errors on
    if r then
	{
	compress
	a=qput
	while peekb a
	    {
	    c=peekb a:a++
	    if c='~' then
		{
		c=peekb a:a++
		if c='X' then print bios
		if c='N' then print bios
		if c='F' then a++
		c=peekb a:if c=' ' then a++
		}
	    else print bios chr c;
	    }
	print bios cr lf
	}
    else print bios "*error*"
    if key=27 then abort
    m+=4
    }
close #1
stop

;===========================================================================

table1: datab 'e notasi'
table2: datab 'herengtheroroud r y  b pal fnd h'
