#inpend=0
#include extinput.fi
#short
ext_inpend=0
var z,y,lasty

print bios cr lf "Message files.ERR for FAST v2. By Peter Campbell." cr lf
on error
    {
    pop ip
    print bios:error msg "\dos.err":printh bios " (IP=";ip;")!"
    stop
    }
ms=allocate 16384/16

print bios "File to edit/create: ";
curtoloc
l=ext_clean_input(name)
if l=0 then error 999

#errors off
open #1,name+2
#errors on
if error then create #1,name+2

file_len=read #1,16384 to ms|0
close #1
changed=0

cls:locate 0,0
colour 15
print "Makemsg file: ";:prints name+2,0:print "    Add	Enter  Delete  Print  ESCape"
colour 7
repeat 80 print "Ä";
colour 15
y=0

function confirm(text,def)
    {
    do_what:
    escaped=0
    open window confirmw
    locate 20,10:prints text,0
    print "? y/n [";chr('n'+def*11);"] ";
    loctocur
    wait for keypressed:ck=lcase key
    close window
    if ck=27 then escaped=1:return 0
    if ck='n' then return 0
    if ck=13 then return def
    if ck='y' then return 1
    goto do_what

    confirmw:
    datab 0,0,6,18,40,22,15,26
    }

proc abort
    {
    if changed then
	{
	if confirm(msave,1) then
	    {
	    create #1,name+2
	    write #1,file_len from ms|0
	    close #1
	    }
	if escaped then return
	}
    cursor 24,0:print bios
    stop

    msave: fname 'Save changes'
    }

function item(nn)
    {
    m=0:c=0
    while (c<=nn) and (m<file_len)
	{
	z=ms[m]b:m++
	if z=0 then m+=2:c++
	}
    if m=file_len then return m
    if c then return m-3 else return 0
    }

function find_msg(nn)
    {
    y=0:m=0
    while m<file_len
	{
	z=ms[m]b:m++
	if z=0 then
	    {
	    num=ms[m]:m+=2
	    if num>=nn then return y
	    y++
	    }
	}
    return y
    }

proc bar(r,c)
    {
    cursor r+2,8
    m=(r+2)*160+1
    repeat 80 video[m]b=c:m+=2
    }

proc display
    {
    #long
    if lasty<>((y/23)*23) then
	{
    #short
	lasty=(y/23)*23
	nn=item(lasty):ms[file_len]=0 ;Setup end character.

	for r=2 to 24
	    locate r,0
	    if nn=file_len then
		{
		fill 80*(25-r) from video|locpos with 0f20h
		return
		}
	    z=ms[nn]b:nn++
	    if z then z=printm whats_this,80
	    else
		{
		print "     ";:locpos-=10
		print ms[nn] tab;
		nn=(printm ms|nn+2,72)-1
		if ms[nn]b=0ah then nn--
		}
	next r
	}
    }

proc delete_message
    {
    m1=item(y)
    m2=item(y+1)
    if m2=m1 then return
    moveb file_len-m2 from ms|m2 to ms|m1
    file_len-=m2-m1:changed=1
    }

proc edit_message(new)
    {
    if not new then m1=item(y)

    me=m1+3
    curtoloc
    en=ms[m1+1]
    ms[file_len]b=0
    fillb 70 from message with ' '
    mess=message
    while ms[me]b pokeb mess,ms[me]b:me++:mess++

    edit_mess2:
    len=ext_string(message,70)
    if len then len--
    if len then
	{
	m2=item(y+1)
	moveb file_len-m2 from ms|m2 to ms|m1+len+3
	moveb len from message to ms|m1+3
	ms[m1]b=0:ms[m1+1]=en
	file_len+=(m1+len+3)-m2
	changed=1
	}
    }

proc add_message
    {
    open window add_number
    cursor 18,61
    en=input
    close window
    if en=0 then return

    y=find_msg(en):m1=item(y)
    moveb file_len-m1 from ms|m1 to ms|m1+3
    ms[m1]b=0:ms[m1+1]=en
    file_len+=3

    lasty=-1
    display
    cursor (y mod 23)+2,8
    edit_message(1)
    }

update:
lasty=-1
msgs=item(32767):msgs=c
locate 0,63:print msgs;" message(s). ";

forever
    {
    if y>=msgs then y=msgs-1
    if y<0 then y=0
    display
    bar(y mod 23,120)
    wait for keypressed
    ks=keyscan:k=lcase low ks:s=high ks
    bar(y mod 23,15)

    if s=71 then y=0
    if s=72 then y--
    if s=73 then y-=23
    if s=79 then y=msgs-1
    if s=80 then y++
    if s=81 then y+=23

    if y>=msgs then y=msgs-1
    if y<0 then y=0

    if k=27 then abort
    #long
    if msgs then
	{
    #short
	if k='d' then
	    {
	    if confirm(mdel,0) then delete_message:goto update
	    }
	if k=13 then edit_message(0):goto update
	#long
	if k='p' then
	    {
	#short
	    lprint " Error Message File: ";
	    m=name+2:while peekb m lprint chr peek m;:m++
	    lprint cr lf

	    m=0
	    while m<file_len
		{
		z=ms[m]b:m++
		if z then lprint "????? ?"
		else
		    {
		    lprint ms[m] chr 9;:m+=2
		    while ms[m]b lprint chr ms[m]b;:m++
		    lprint
		    }
		}
	    }
	}
    if k='a' then add_message:goto update
    }

name:
string 30

message:
space 70

mdel:
fname 'Delete message'

whats_this:
fname '*       *'

add_number:
datab 0,0,45,16,70,20,120
datab 22,2,2,'Error number:',1ah
