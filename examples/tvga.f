
;test Trident video card - enhanced text modes

buffer ? 10000

reg ax=0056h	    ;132*60
int 10h

load "test",buffer,10000
m=buffer

for y=0 to 59
    x=0
    while (x<264) and (peekb m<>13)
	{
	video[y*264+x]b=peekb m
	x+=2:m++
	}
    if peekb m=10 then m++
next y

wait for key

screen 3
stop

