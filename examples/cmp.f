;COMPARE.

#inpend=0
s=allocate 8192 ; 128k
t=s+4096 ; 2nd 64k
fill 32768 from s|0 with 1a1ah
fill 32768 from t|0 with 1a1ah

print bios "Compare ";
inputs f1:if peekb (f1+2)=0 then terminate

print bios " with ";
inputs f2:if peekb (f2+2)=0 then terminate
print bios

load f1+2,s|1:load f2+2,t|1

c=1:dif=0
curtoloc
while c below 65534
    {
    k=key
    if k=27 then terminate
    i=compareb 65001-c at s|c with t|c
    if i then
	{
	printh i-1;:printhb " : "s[i]" ! "t[i];
	if k=' ' then wait for keypressed
	dif++
	if (dif mod 4)=0 then print else print "      ";
	}
    else i=65533
    c=i+1
    }
print cr dif" difference(s)."
loctocur
terminate

f1:
string 20

f2:
string 20
