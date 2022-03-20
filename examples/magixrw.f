
;==============================================================================
;Read 463 bytes and write 463+cr\lf - BF10 -> MAGIX
;==============================================================================

const rlen=463

buffer ? rlen+2

print bios "Opening Files"
open #1,"dprt2125"
create #2,"dprt.new"

print bios "Reading/Writing Records ... ";
records=0

loop:
r=read #1,rlen to buffer
if r<>rlen then
    {
    if r=0 then
	{
	close #1
	close #2
	print bios
	print bios "Completed - Ok"
	stop
	}
    else
	{
	print bios "Record Length = ";r;", should be ";rlen;"."
	stop
	}
    }

pokeb buffer+rlen,13
pokeb buffer+rlen+1,10
write #2,rlen+2 from buffer

records++
curtoloc
print records;
goto loop
