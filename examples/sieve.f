#short
const max=8191
print bios "Sieve - 25 iterations... ";
t1=timer
flags ? max

repeat 25
    {
    count=0
    fill max/2 from flags with -1
    for i=0 to max
    if peekb (flags+i) then
	{
	prime = i*2+3
    ;	print prime;"   ";
	k = i + prime
	while k<=max
	    {
	    pokeb flags+k,0
	    k+=prime
	    }
	count++
	}
    next i
    }

t2=(timer-t1)*100/182
print bios count" primes in ";t2/10;".";t2 mod 10;" seconds."
