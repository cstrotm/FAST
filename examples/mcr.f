
;===========================================================================
; MCR: replace line feed with carriage return + line feed	(LF = CR,LF)
;===========================================================================

#short
#inpend=0
unsigned m

on error
    {
    print bios:error msg "\dos.err":print bios "!"
    stop
    }

print bios "Make CR/LF " cr lf
s=allocate 4096

print bios "Input file: ";
inputs iname:print bios
if peekb (iname+2)=0 then error 999
print bios "Output file: ";
inputs oname:print bios
if peekb (oname+2)=0 then error 999

fill 32768 from s|0 with 1a1ah
load iname+2,s|0
len=searchb 65500 from s|0 for 1ah

m=0
while m<len
    {
    if s[m]b=10 then
	{
	moveb len-m from s|m to s|m+1
	s[m]b=13
	len++
	m++
	}
    m++
    }

save oname+2,s|0,len
stop

iname: string 30
oname: string 30
