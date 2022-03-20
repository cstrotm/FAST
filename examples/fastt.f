
;===========================================================================
;==  FASTT source code debugger - FAST v2.00+				  ==
;==  Written by Peter Campbell 1989.					  ==
;===========================================================================

;12-10-1991  Tidy up printed messages - use FastT: prompt everywhere
;14-04-1994  Support 132*60 columns and others
;23-08-1995  Option to output all executed lines to DOS print (redirection)
;	     eg: fastt -d program_name >output_file
;
;

#short
var m,t_base,seg_ft,fscreen_base,dump_option
unsigned ip,n
var32 addr,addrs

fcom	 ? 64
work	 ? 64
sets	 ? 50*2
cmd_line ? 120
buffer	 ? 650
lines	 ? 24*2

dos 35(3):o3=reg bx:s3=reg es

proc abort
    {
    print bios "FastT: Terminated."
    reg dx=o3,ds=s3:dos 25(3)
    reg ds=reg cs
    terminate
    }

on error
    {
    print bios
    error msg "\dos.err"
    print bios "!"
    abort
    }

on break error 999

function find_file(fh)
    {
    am=t_base
    ffloop:
    if seg_ft[am+1]=0 then return 0
    if seg_ft[am]b=fh then return 1
    am+=6
    goto ffloop
    }

function find_address
    {
    am=t_base

    while seg_ft[am+1]
	{
	n=seg_ft[am+1]
	if n=ip then return am
	am+=6
	}

    return 0
    }

function get_buffer
    {
    gf=seg_ft[m]b
    fp=sets+gf*2
    mn=1+gf*64
    handle#1,peek fp

    a1=seg_ft[m+3]
    a2=seg_ft[m+5]b
    addrs=a2*65536	;24 bit address
    addrs+=a1

    if dump_option then
	{
	seek #1,addrs
	l=read #1,100 to buffer
	}
    else
	{
	r=search 24 from lines for low addrs
	if r then return 1+(r-lines)/2

	addr=addrs
	seek #1,addr
	l=read #1,640 to buffer
	}
    pokeb buffer+l,26
    return 1
    }

proc show_buffer
    {
    colour 15
    fill fscreen_cols1 from video|fscreen_base with 0f20h
    locate 0,0:print "FastT: ";:while seg_ft[mn]b print chr seg_ft[mn]b;:mn++
    md=buffer:ml=lines:mlo=low addr-buffer
    colour 7
    for yp=1 to 24
    locpos=fscreen_base+yp*fscreen_cols2
    poke ml,mlo+md:ml+=2
    if fscreen_cols1=80 then md=printm md,80
    else md=printm md,132
    next yp
    }

print bios "FastT: ";

seg_ft=allocate 4096
dump_option=0

m=81h:f=fcom
mstore=0
while peekb m<>13
    {
    c=peekb m
    if c>' ' then
	{
	if c='-' then
	    {
	    m++
	    c=peekb m
	    if c='d' then dump_option=1         ;23-08-1995
	    }
	else
	    {
	    mstore=1
	    pokeb f,c:f++:if f>(fcom+63) then goto use_file
	    }
	}
    else if mstore then goto use_file
    m++
    }

use_file:
pokeb f,0
if mstore=0 then print bios "file?":abort
moveb 120 from m to cmd_line

print bios "running ";
m=fcom
while (peekb m<>0) and (peekb m<>'.') print bios chr ucase peekb m;:m++
pokeb m,'.':print bios cr lf
m++
move 32 from fcom to work
moveb 4 from ext_com to m
moveb 3 from ext_ft to work+m-fcom

open #1,work
rl=read #1,65500 to seg_ft|0
close #1
fill 6 from seg_ft|rl with 0	;end marker
modify seg_ft to 1+rl/16

m=sets:fp=1
sources=seg_ft[0]b
t_base=1+sources*64
print bios "Source file(s) ";
for so=1 to sources
    move 32 from seg_ft|fp to work
    if find_file(so-1) then
	{
	p=work:while peekb p print bios chr lcase peekb p;:p++
	print bios " ";
	open #1,work
	poke m,handle#1
	}
    m+=2:fp+=64
next so

print bios
poke exe_com+4,reg cs
poke exe_com+8,reg cs
poke exe_com+12,reg cs

setint 3 to fast3
show=1:using=0

execute fcom,exe_com

page 0
abort

;= Data ====================================================================

ext_com:
datab 'com',0

ext_ft:
datab 'ft',0

exe_com:
data 0
data cmd_line,0
data 5ch,0
data 6ch,0

;== Fast3 =========================================================================

fast3:
pushall

reg ds=reg cs
if using then goto exit3
using=1
ip=peek reg ss|(reg sp+20)
cs=peek reg ss|(reg sp+22)

#long
if show then
    {
    fscreen_cols1=cs[256+119]
    fscreen_cols2=cs[256+121]
    fscreen_rows1=cs[256+123]
    fscreen_size=cs[256+117]

    if fscreen_rows1>=50 then fscreen_base=(fscreen_rows1-25)*fscreen_cols2
    else fscreen_base=0

    m=find_address
    if m then
	{
	hy=get_buffer

	if dump_option then
	    {
	    len=12
	    while len
		{
		c=seg_ft[mn]b
		if c then mn++ else c=' '
		print bios chr c;
		len--
		}
	    printh bios " ";ip;" : ";
	    md=buffer
	    while peekb md<=31 md++
	    while peekb md>31 print bios chr peekb md;:md++
	    print bios
	    }
	else
	    {
	    show_buffer
	    m=fscreen_base+hy*fscreen_cols2+1
	    repeat fscreen_cols1 video[m]b=120:m+=2
	    wait for keyscan
	    test break
	    }
	}
    }
#short

;Call ON TRACE statement.
if cs[113h] then
    {
    push reg cs
    push ret_trace
    push cs
    push cs[113h]
    reg ds=cs,es=cs
    retf
    ret_trace:
    reg ds=reg cs
    }

show=1
if peekb 0|417h and 16 then show=0

using=0

exit3:
popall
iret
