;
;MAGIX development toolkit. (C) Peter Campbell, 18/05/1988.
;

#errors on
#short
#window memory 4000
#include extinput.fi

var mx,yp,cc,count_files,ws,wscreen,sy,exe_ok
const basex=30,basey=0,options=12
const wsize=600*48,cmd_size=64*options
const mdl=0,full=64,part=128,ldir=192,project=256
const magix=320,source=384,dict=448,proj=512,gen=576,testl=640,genall=704
const files2=500*13
unsigned wss
work	? 128
name	? 64
program ? 32
store	? 513


oldcur=curpos
cmd_seg=allocate cmd_size/16
fseg=allocate files2/16+1

dos 35(21)
poke bseg,reg es:poke boff,reg bx
on break goto abort

proc print_to(ptm,ptn)
    {
    pokeb ptm,(ptn/10)+'0'
    pokeb ptm+1,(ptn mod 10)+'0'
    }

proc write_log(logmsg)
    {
    #errors off
    open #10,logfile
    if error then
	{
	if error<>2 then error
    #errors on
	create #10,logfile
	write #10,113 from logtop
	}
    else seek #10,eof

    fill 6 from logline with 2020h
    m=logmsg:n=logline
    while peekb m<>'.' pokeb n,peekb m:m++:n++
    dos 2a
    rday=low reg dx
    rmonth=high reg dx
    ryear=reg cx-1900
    print_to(logline+12,rday)
    print_to(logline+15,rmonth)
    print_to(logline+18,ryear)
    dos 2c
    rhour=high reg cx
    rminute=low reg cx
    print_to(logline+22,rhour)
    print_to(logline+25,rminute)

    if exe_ok then pokeb logline+29,'û' else pokeb logline+29,'x'
    write #10,32 from logline
    close #10
    }

proc print_2(p2c)
    {
    if p2c=13 then reg dx=13:dos 2:p2c=10
    reg dx=p2c
    dos 2
    }

proc wup(wss)
    {
    wlast=wss
    for wy=1 to 23
	locate basey+wy,basex+1
	if wss<(wsize-48) then wsp=printm ws|wss,48:wss+=48
	else
	    {
	    repeat 48
		{
		video[locpos]b=ws[wss]b
		locpos+=2
		wss++
		if wss>=wsize then wss=0
		}
	    }
    next wy
    }

proc wait_ws
    {
    forever
	{
	wbase=wscreen
	ks=keyscan:k=ucase low ks:s=high ks
	if s=72 then wbase-=48
	if s=80 then wbase+=48
	if s=73 then wbase-=48*23
	if s=81 then wbase+=48*23
	if wbase above 64000 then wbase+=wsize
	if wbase above (wsize-2) then wbase=0
	if (wbase=wscreen) and (s<>0) then
	    {
	    print_2(ucase k)
	    print_2(13)
	    return
	    }
	wscreen=wbase
	wup(wscreen)
	}
    }

proc print_2m(p2a)
    {
    while peekb p2a print_2(peekb p2a):p2a++
    }

proc error_sound
    {
    pop ip:push ip
    printh bios "Internal error at ";ip
    t=(rnd mod 10)+10
    for a=200 to 800 step 50
    for b=a to a+t
    noise 2,b
    next b,a
    noise off
    }

on error
    {
    pop ad
    cls
    cursor 0,0
    error msg "\dos.err"
    printh bios ", address=";ad
    error_sound
    goto abort
    }

proc exe(cmd)
    {
    exe_ok=0
    ese=reg cs:if cmd<=cmd_size then ese=cmd_seg
    move 32 from ese|cmd to store
    m=store
    print_2('>')
    while peekb m print_2(peekb m):m++
    print_2(13)

    poke exe_com,peek 2ch
    poke exe_com+4,reg cs
    poke exe_com+8,reg cs
    poke exe_com+12,reg cs

    m=store
    while (peekb m<>0) and (peekb m<>' ') m++
    moveb m-store from store to name
    pokeb name+(m-store),0
    while peekb m=' ' m++
    x=searchb 42 from m for 0:if x then pokeb x,13
    pokeb m-1,x-m
    poke exe_com+2,m-1
    execute name,exe_com
    dos 4d(0)
    el=reg ax
    if not el then exe_ok=1
    }

function compiler(type,cmd_line)
    {
    any_errors=0
    value=0
    if not cc then goto finish_proc
    mf=0

    repeat cc
	{
	locate 4,0:printb "Programs to compile: ";count_files;" ";

	moveb 13 from fseg|mf to program:mf+=13

	v=program:flag=0
	while peekb v
	    {
	    if peekb v='$' then flag=1
	    v++
	    }
	if (type=0) and (flag=0) then goto comp_it
	if (type<>0) and (flag<>0) then goto comp_it
	goto skip

	comp_it:
	plen=searchb 13 from program for '.'
	if not plen then goto skip
	plen-=program
	added=0
	if cmd_seg[source+1]b=':' then
	    {
	    added=2
	    plen+=2
	    moveb 13 from program to program+2
	    move 1 from cmd_seg|source to program
	    pokeb program,ucase peekb program
	    }
	move 32 from cmd_seg|cmd_line to work
	a=searchb 64 from work for '%'
	if not a then error_sound:goto skip
	moveb 40 from a+1 to a+plen
	moveb plen from program to a

	if yp=25 then
	    {
	    scroll 0,6,20,24,1
	    yp=24
	    }
	locate yp,2
	m=program:while peekb m print chr peekb m;:m++
	yp++

	count_files--

	exe(work)
	write_log(program)

	locate yp-1,16
	#long
	if exe_ok then
	    {
	    value=1
	    print chr 251;
	    reg dx=program,cx=0:dos 43(1) ; Reset archive bit.
	    if carry then error_sound

	    if type then
		{
		m=program
		while peekb m<>'.' m++
		spg=program:if peekb (program+1)=':' then spg+=2
		write #1,m-spg from spg
		write #1,2 from crlf
		}

	    move 32 from cmd_seg|ldir to work
	    x=searchb 60 from work for 0
	    if not x then error_sound:goto skip
	    plen-=added
	    mg=program+added
	    moveb plen from mg to x
	    moveb 5 from listext to x+plen
	    delete work
	    }
	else
	    {
	    print "x";
	    any_errors++
	    }
	#short

	skip:
	}

    finish_proc:
    locate 4,0:printb "Programs to compile: ";count_files
    return value
    }

#errors off
load cmd_file,cmd_seg|0,cmd_size
if error then fillb cmd_size from cmd_seg|0 with 0
#errors on

proc cset(csm)
    {
    open window csetw
    locate 17,9
    move 32 from cmd_seg|csm to text+2
    ext_inpend=0
    l=ext_input(text)
    ext_inpend=13
    if l>1 then
	{
	move 32 from text+2 to cmd_seg|csm
	save cmd_file,cmd_seg|0,cmd_size
	}
    close window
    }

function recomps
    {
    move 32 from cmd_seg|ldir to work
    x=searchb 60 from work for 0
    m=project
    a=m
    while cmd_seg[a]b
	{
	if cmd_seg[a]b='\' then m=a+1
	a++
	}
    moveb 30 from cmd_seg|m to x
    plen=searchb 50 from work for '.'
    if not plen then error_sound:return 0
    moveb 5 from listext to plen
    cs=allocate 4096
    fill 32768 from cs|0 with 1a1ah
    load work,cs|0
    cc=0
    lm=0:a1=0:a2=0
    while cs[lm]b<>26
	{
	b=cs[lm]b
	if b=13 then a2=a1:a1=lm+2
	#long
	if (b='!') and (cs[lm+3]b='T') then
	    {
	#short
	    m=text
	    if cmd_seg[source+1]b=':' then
		{
		move 1 from cmd_seg|source to m
		m+=2
		}
	    moveb 13 from cs|a2+1 to m
	    n=searchb 13 from m for 13
	    if not n then deallocate cs:return 0
	    moveb 4 from dms to n
	    pokeb n+4,0 ; ASCIIZ filename for recompiling.
	    cc=1
	    ma=text:while peekb ma print_2(peekb ma):ma++
	    print_2(13)
	    reg dx=text,cx=32:dos 43(1) ; Set edit bit.
	    if carry then error_sound
	    lm+=2
	    }
	lm++
	}
    deallocate cs
    return cc
    }

proc do_cmd(def)
    {
    gd=menu cmdw
    close windows
    #long
    if gd then
	{
    #short
	cls
	cursor 0,0
	exe(peek (ctable+2*gd-2)
	if (gd=1) or (gd=2) then write_log(lgen)
	if gd=3 then write_log(lfull)
	if gd=4 then write_log(lpart)
	if (gd=3) or (gd=4) then
	    {
	    if exe_ok then
		{
		move 32 from cmd_seg|project to store
		delete store
		}
	    }
	}
    return

    ctable:
    data gen,genall,full,part,proj,magix,testl
    }


enter_menu:
mx=menu main_menu:goto ent2
forever
    {
    mx=select main_menu,mx
    ent2:
    if not mx then curpos=oldcur:close window:terminate
    if mx=1 then goto run
    if mx=15 then
	{
	do_cmd ;Closes windows.
	goto enter_menu
	}
    if mx=2 then
	{
	close window
	r=recomps
	goto enter_menu
	}
    else
	{
	ad=(mx-3)*64
	cset(ad)
	}
    }

main_menu:
datab 2,15,52,4,79,22,37h
datab 22,2,1,'CHANGES - PC v9'
datab 22,2,3,'COMPILE CHANGES'
datab 22,2,4,'RECOMPILES'
datab 22,2,5,'MDL command'
datab 22,2,6,'LINK command'
datab 22,2,7,'LINK=PART command'
datab 22,2,8,'List file directory'
datab 22,2,9,'project.PLC name'
datab 22,2,10,'MAGIX path'
datab 22,2,11,'Source path'
datab 22,2,12,'Dict command'
datab 22,2,13,'Proj command'
datab 22,2,14,'Gen command'
datab 22,2,15,'Test Link command'
datab 22,2,16,'Gen=all command'
datab 22,2,17,'Execute command'
datab 26

csetw:
datab 0,0,5,15,79,19,14
datab 26

text:
string 64

listext:
fname '.LST'

run:
ws=allocate wsize/16
fill wsize/2 from ws|0 with 2020h
wplace=0
wline=0
wscreen=0   ;Setup screen display features.
wlast=0

close window ;main_menu
cls
locate 1,5
print "CHANGES - PC (C)1988"
locate 5,0:repeat basex-1 print chr 196;
yp=6

open window bios2
poke 0|21h*4,newdos
poke 0|21h*4+2,reg cs

spref:
move 32 from cmd_seg|project to store
#errors off
open #1,store
if error then
    {
#errors on
    create #1,store
    goto be2
    }

len=0
forever
    {
    fill 256 from store with 1a1ah
    rl=read #1,512 to store
    x=searchb 512 from store for 26
    if x then goto append
    len+=512
    }

append:
seeka=len+x-store
seek #1,seeka

be2:
ds=dta segment:do=dta offset
move 32 from cmd_seg|source to store
#errors off
find first store
m=0:cc=0
goto find_entry
forever
{
find next
find_entry:
if error then goto start
if ds[do+15h]b=32 then
    {
    if m>=files2 then goto start
    moveb 13 from ds|do+30 to fseg|m
    m+=13:cc++
    }
}
#errors on

start:
count_files=cc

do_gen=compiler(0,dict)
if do_gen then
    {
    if any_errors then goto no_gen
    print_2m(genm)
    wait_ws
    if k<>'Y' then goto abort

    exe(gen)
    write_log(lgen)
    if not exe_ok then
	{
	no_gen:
	error_sound
	wait_ws
	goto abort
	}
    exe(proj)
    print_2(13)
    }

do_link=compiler('$',mdl)
write #1,1 from eofc
close #1
#long
if not any_errors then
    {
#short
    fpi:
    print_2m(fpm)
    wait_ws

    exe_ok=0
    push k
    if k='Y' then k='P'
    if k='F' then exe(full):write_log(lfull)
    if k='P' then exe(part):write_log(lpart)
    if k='T' then exe(testl):if exe_ok then goto fpi
    pop k
    #long
    if exe_ok then
	{
    #short
	move 32 from cmd_seg|project to store
	delete store
	print_2m(magm)
	wait_ws
	if k='Y' then exe(magix)
	}
    else
	{
	if (k='F') or (k='T') then
	    {
	    print_2m(recm)
	    wait_ws
	    if k='Y' then if recomps then goto spref
	    }
	}
    }
else
    {
    print_2m(waitm)
    wait_ws
    }


abort:
reg dx=peek boff,ds=peek bseg:dos 25(21)
reg ds=reg cs
terminate


bios2:
datab 0,0,basex,basey,79,24,7,26

newdos:
inline 9ch	    ;PUSHF
inline 80h,0fch,2   ;CMP AH,2
inline 74h,6	    ;JE FUNCTION 2
inline 9dh	    ;POPF
inline 0eah	    ;JMPF OLD
boff:
data 0
bseg:
data 0

inline 9dh	    ;POPF
pushall
reg ds=reg cs
dchar=low reg dx

if dchar=10 then goto findos
if (dchar = 13) or ((wplace-wline) > 47) then
    {
    scroll basex+1,basey+1,78,23,1
    wline+=48
    if wline above (wsize-2) then wline=0
    fill 24 from ws|wline with 2020h
    wplace=wline
    goto findos
    }

ws[wplace]b=dchar:wplace++
wscreen=wline-1056
if wscreen above 64000 then wscreen+=wsize
if wscreen<>wlast then wup(wscreen)
sx=wplace-wline

push locpos
locate basey+23,basex+sx
cursor basey+23,basex+sx+1
print chr dchar;
pop locpos

findos:
popall
inline 0cfh


exe_com:
data 0
data work,0
data 5ch,0
data 6ch,0

crlf:
datab 13,10
eofc:
datab 26

dms:
fname '.MS'

cmd_file:
fname 'c.fil'

genm:
datab 13,'Gen?',13,0

fpm:
datab 13,'Full, Partial or Test link? (F/P/T) ',0

magm:
datab 13,'MAGIX? (Y/N) ',0

recm:
datab 13,'Recompile errors? (Y/N) ',0

waitm:
datab 13,'View mode. (any non-view key aborts)',13,0

cmdw:
datab 1,7,40,14,57,24,15
datab 22,2,1,'Select command'
datab 22,2,3,'Gen'
datab 22,2,4,'All (gen)'
datab 22,2,5,'Full link'
datab 22,2,6,'Link=part'
datab 22,2,7,'Proj'
datab 22,2,8,'Magix'
datab 22,2,9,'Test link'
datab 26

logfile:
fname 'c.log'

logtop:
datab 'MAGIX Compile Utility Log File: By Peter Campbell',13,10,13,10
datab 'Program	   Date      Time',13,10
datab '------------------------------',13,10
logline:
datab ' 	   ??/??/??  ??:??   ',13,10

lgen:
datab '-Gen.'

lfull:
datab '-Full Link.'

lpart:
datab '-Partial.'
