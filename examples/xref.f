;XREF utility for ASM files.

const mf=16
var lm,ln,count

print bios "XREF by Peter Campbell. v2" cr lf

on error
    {
    num=error
    error msg "\dos.err"
    print bios "!"
    terminate
    }

ls=allocate 4096
fs=allocate 4096
loaded=0

m=81h:f=file
while peekb m<>13
    {
    c=peekb m
    if c>' ' then pokeb f,c:f++
    m++
    }
if f=file then print bios "XREF filename(s)":terminate
pokeb f,0

function sept(c)
    {
    return searchb 28 from sps for c
    sps:
    datab ' ',13,10,9,39
    datab '()+-*/=;:"[]{}.,<>@#^|?'
    }

proc report
    {
    test break
    print bios "  Label: ";
    a=label:while peekb a print bios chr peek a;:a++
    a=label:while peekb a print chr peek a;:a++
    print

    fn=names
    flag=0
    repeat count
	{
	if (count=1) and loaded then goto skipload
	fill 32768 from fs|0 with 1a1ah
	load fn,fs|0
	fs[65535]b=1ah
	loaded=1

	skipload:
	fm=0:fst=fm:fa=peekb label
	lines=1
	while fs[fm]b<>1ah
	    {
	    c=ucase fs[fm]b
	    if c=fa then
		{
		sm=fm
		if sept(fs[fm-1]b) then
		    {
		    la=label
		    while (peekb la<>0) and (la<(label+32))
			{
			if peekb la<>ucase fs[fm]b then goto miss
			fm++:la++
			}
		    if not sept(fs[fm]b) then goto miss
		    if (lm=fm) and (fn=(ln-13)) then goto miss ; Itself?
		    print bios cr lf "    ";
		    pa=fn:while peekb pa print bios chr peekb pa;:pa++
		    print bios "["lines"] ";
		    flag++
		    pa=fst

		    col=low curpos
		    if (25-col)>0 then repeat 25-col print bios " ";
		    col=0
		    while fs[pa]b<>13
			{
			p=fs[pa]b
			if p=10 then goto igp
			if p=9 then
			    {
			    x=(col and 248)+8
			    repeat x-col print bios " ";
			    col=x
			    goto igp
			    }
			else print bios chr p;:col++

			igp:
			pa++
			}
		    goto nextch

		    miss:
		    fm=sm
		    }
		}
	    if c=13 then
		{
		if fs[fm+1]b=10 then fm++ ; Skip lines feed
		fst=fm+1
		lines++
		}
	    fm++
	    nextch:
	    }
	fn+=13
	}
    if flag then print bios
	    else print bios " (not found)"
    }

m=names:count=0
find first file
goto entry

while count<mf
    {
#errors off
    find next
#errors on
    entry:
    if error then goto check
    moveb 13 from dta segment|dta offset+30 to m
    m+=13:count++
    }

check:
ln=names
repeat count-2
    {
    fill 32768 from ls|0 with 1a1ah
    print bios cr lf "References for file: ";
    a=ln
    while peekb a print bios chr peek a;:a++
    print bios
    load ln,ls|0
    ls[65535]b=1ah
    ln+=13:lm=0
    while ls[lm]b<>1ah
	{
	c=ls[lm]b
	if lcase c<>ucase c then
	    {
	    a=label
	    while not sept(ls[lm]b) pokeb a,ucase ls[lm]b:lm++:a++
	    pokeb a,0
	    report
	    goto loope ; Find next lines ... then next label...
	    }
	else
	    {
	    loope:
	    c=ls[lm]b
	    if c=1ah then goto nls
	    if c<>13 then lm++:goto loope
	    lm++
	    if ls[lm]b=10 then lm++
	    }
	nls:
	}
    }

print bios cr lf"Done."
terminate

file:
space 64

names:
space 13*mf

label:
space 32
