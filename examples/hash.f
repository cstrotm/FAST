;Fast HASH converter.
forever
{
print bios "Word : ";
inputs word
if peekb (word+2)=13 then print bios:terminate
m=word+2
h=0
while peekb m<>13
     {
     b=lcase peekb m
     repeat 3
	{
	h=rrightz h:if carry then h=h or 128
	}
     h=h xor b
     m++
     }
printb bios "  = ";h
}

word:
string 15
