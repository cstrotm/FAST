;== Shrink utility - fixes text file for proper tabs etc =====================

print bios "TEXT SHRINK by Peter Campbell, Version 4 (22/2/1989)." cr lf

#inpend=0
#short

var len
unsigned olen,tt
const bs=32768
buffer	? 256
sbuffer ? 258

inp=allocate bs/16
oup=allocate bs/16

on error print bios:error msg "\dos.err":print bios "!":beep:terminate
on break error 999

proc compress
    {
    test break
    st=buffer+len-1
    while (peekb st=' ') and (len>0) len--:st--

    est=st+1:x=0
    st=buffer:f=sbuffer
    while st<est
	{
	b=peekb st
	if b=' ' then
	    {
	    bl=(x and 248)+8
	    flag=1
	    for a=1 to bl-x
	      if peekb (st+a-1)<>' ' then goto spput
	    next a
	    b=9:st+=a-2:x=bl-1
	    }
	spput:
	pokeb f,b:f++
	x++:st++
	}
    poke f,0a0dh
    pokeb f+2,1ah
    len=f-sbuffer
    len+=2
    }

print bios "Input file: ";
inputs ifile
if peekb (ifile+2)=0 then error 999

print bios cr lf "Output file: [same] ";
inputs oname
ofile=oname+2
if peekb (oname+2)=0 then ofile=same_name

open #1,ifile+2
create #2,ofile

print bios cr lf cr lf "Shrinking... ";

tt=bs
olen=0

newl:
len=0

forever
    {
    if tt=bs then
	{
	read_len=read #1,bs-1 to inp|0
	inp[read_len]b=26
	tt=0
	}

    byte=inp[tt]b:tt++
    if byte=26 then goto split
    if byte=10 then goto nextb
    #long
    if byte=13 then
	{
    #short
	split:
	compress
	moveb len from sbuffer to oup|olen
	olen+=len
	if olen>(bs-256) then
	    {
	    write #2,olen from oup|0
	    olen=0
	    }
	if byte=26 then goto finish
	goto newl
	}
    if byte=9 then
	{
	nlen=(len and 248)+8
	repeat nlen-len pokeb buffer+len,' ':len++
	goto nextb
	}

    if len<256 then
	{
	pokeb buffer+len,byte
	len++
	}
    else goto split	;Line must be split - too long!

    nextb:
    }
finish:
write #2,olen from oup|0
close #1,#2
if ofile=same_name then
    {
    delete ifile+2
    rename same_name to ifile+2
    }

print bios "complete."
terminate


ifile:
string 30

oname:
string 30

same_name:
fname 'shrink.tmp'
