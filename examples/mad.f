;Mandelbrot display/print program.

const mem=40000
ar ? 42
m ? mem
screen 4:mode 320,and
lprint chr 27;"A";chr 3;

proc send(n)
    {
    fill 21 from ar with 0
    if n<4 then	goto send_zero
    n=(n*10)/29:if n>41	then n=41
    if n then
	{
	fillb n	from ar	with 1
	for co=0 to n-1
	    r1=rnd mod 42
	    st=peek (ar+r1):pokeb ar+r1,peek (ar+co):pokeb ar+co,st
	next co
	}
    send_zero:
    n=ar
    lprint chr 27;"L";chr 7;chr	0;
    repeat 7
	{
	b=0
	repeat 3 b=b*2+peekb n:n+=7
	lprint chr b*32;
	n-=20
	}
    }

open #1,"\qc\mad.mad"
l=read #1,mem to m
close #1

x=0:y=0:a=m
while a below (m+l)
    {
    pa=peekb a
    if pa=255 then y=0:x++:goto nextb
    if pa>60 then plot x,y
    y++
    nextb:
    a++
    }

a=m
while a below (m+l)
    {
    if key=27 then goto	mad_exit
    pa=peekb a
    if pa=255 then lprint
	else send(pa)
    a++
    }
lprint

wait for key=27
mad_exit:
screen 3
