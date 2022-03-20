
#include fsort.fi
ld=allocate 4096
fill 32768 from ld|0 with 1a1ah
load "\asm\sofa.c",ld|0

m=0:recs=0
while ld[m]b<>1ah
    {
    n=1+searchb 16 from ld|m for 13:if n=1 then goto sortit
    if ld[n]b=10 then n++
    move 4000 from ld|n to ld|m+16
    m+=16:recs++
    }

sortit:
if sort(ld,0,16,recs) then
    {
    m=0:repeat recs
	{
	n=m
	while ld[n]b<>13 print bios chr ld[n];:n++
	print bios
	m+=16
	}
    }
