;WWW - PC/FAST
n=0
m=81h
while peekb m<>13
    {
    c=peekb m-'0'
    if (c>=0) and (c<=9) then n*=10:n+=c
    m++
    }
if n=0 then n=32500
count=n
maxc=n

on int 1
    {
    if peekb 0|417h=15 then stop int 1
    r=rnd
    if (r>n) and (count=0) then
	{
	maxc/=2
	if maxc<100 then maxc=200
	video=0b800h-(800h*mono)+page*100h
	r=r mod 5
	used=0
	if r=0 then
	    {
	    used=1
	    repeat 12
		{
		move 2000 from video|0 to video|320
		move 160 from video|4000 to video|0
		}
	    move 2000 from video|0 to video|160
	    move 80 from video|4000 to video|0
	    }
	if r=1 then
	    {
	    used=1
	    repeat 12
		{
		move 160 from video|0 to video|4000
		move 2000 from video|320 to video|0
		move 160 from video|4000 to video|4000-320
		}
	    move 80 from video|0 to video|4000
	    move 2000 from video|160 to video|0
	    move 80 from video|4000 to video|4000-160
	    }
	if r=2 then
	    {
	    used=1
	    repeat 10
		{
		for y2=0 to 3840 step 160
		    move 8 from video|y2+144 to store
		    move 72 from video|y2 to video|y2+16
		    move 8 from store to video|y2
		next y2
		}
	    }
	if r=3 then
	    {
	    used=1
	    repeat 10
		{
		for y3=0 to 3840 step 160
		    move 8 from video|y3 to store
		    move 72 from video|y3+16 to video|y3
		    move 8 from store to video|y3+144
		next y3
		}
	    }
	if r=4 then
	    {
	    used=1
	    repeat 10
		{
		for y4=0 to 3840 step 320
		    move 8 from video|y4 to store
		    move 72 from video|y4+16 to video|y4
		    move 8 from store to video|y4+144

		    y4+=160
		    move 8 from video|y4+144 to store
		    move 72 from video|y4 to video|y4+16
		    move 8 from store to video|y4
		    y4-=160
		next y4
		}
	    }

	if used then
	    {
	    count=rnd mod maxc
	    maxc/=2
	    if maxc<100 then maxc=200
	    }
	}
    if count then count--
    }
stop resident

store:
space 16