
;== File join utility ======================================================

#inpend=0
#short
var32 flen,seeka

ds=dta segment:do=dta offset
files=0

proc pn(pm)
    {
    l=15
    while peekb pm print bios chr peekb pm;:pm++:l--
    repeat l print bios " ";
    }

on error
    {
    print bios
    error msg "\dos.err"
    print bios "!":stop
    }
on break error 999

ld=allocate 4096

print bios cr lf "JOIN small files into one to save on disk space." cr lf
print bios "File (with wildcards, press ENTER for unJOINing) : ";
inputs name
print bios
if peekb (name+2)=0 then
    {
    print bios "Joint file (to unjoin) : ";
    inputs dest
    print bios cr lf
    if peekb (dest+2)=0 then error 999
    goto unjoin
    }

print bios "Destination file : ";
inputs dest
print bios cr lf
if peekb (dest+2)=0 then error 999
create #2,dest+2

find first name+2
goto entry

forever
    {
    #errors off
    find next
    if error then goto dtotal
    #errors on
    entry:
    moveb 13 from ds|do+30 to name
    open #1,name
    pn(name):print bios
    write #2,17 from ds|do+26
    flen=peek ds|(do+26)

    file_loop:
    len=read #1,65530 to ld|0
    if len then
	{
	write #2,len from ld|0
	flen-=len
	goto file_loop
	}
    if (high flen) or (low flen) then error 24
    close #1:files++
    }

dtotal:
close #2
print bios cr lf "Joined ";files;" files."
if files=0 then delete dest+2
stop

name:
string 30
dest:
string 30
spec:
space 17

unjoin:
open #2,dest+2
seeka=0

unloop:
seek #2,seeka
rn=read #2,17 to spec:if rn=0 then
    {
    print bios cr lf "Unjoined ";files;" files."
    close #2
    stop
    }
if rn<>17 then error 13
flen=peek spec:seeka+=flen+17
pn(spec+4)
#errors off
open #1,spec+4
#errors on
if error=0 then
    {
    print bios "File exists! Overwrite? (y/n) ";
    wait for keypressed:y=lcase key
    if y=27 then error 999
    if y<>'y' then goto unnext
    close #1
    }
else if error<>2 then error

create #1,spec+4
files++

write_loop:
rx=65530
if (high flen)=0 then rx=low flen
rn=read #2,rx to ld|0
if rn then
    {
    write #1,rn from ld|0
    flen-=rn
    goto write_loop
    }
if (high flen) or (low flen) then error 24

unnext:
print bios
close #1
goto unloop
