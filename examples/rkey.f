;KEY redifinition program.
;
; RKEY	Written by Peter Campbell 1988.
;

const maxk=32500/3,defk=1000
var place,start,nextk,s,keys,m,main,kseg,sh
var ax,retax,running,defining,kspace,playback

#window memory 4000
#inpend=0

on error
    {
    print bios
    error msg "\dos.err"
    terminate
    }

proc loadfile
    {
    nextk=(read #1,kspace to kseg|0)/3
    if error then nextk=0:print bios " <error>";
    close #1
    }

proc incy
    {
    y++
    while peekb 0|417h and 8 {}
    if y=24 then scroll 1,3,69,23,1:y=23
    locate y,15
    }

print bios cr;lf"RKEY by Peter Campbell. v2"
keys=defk
dseg=peek 0|86h

proc eshift
    {
    if sh and 8 then print "ALT-";
    if sh and 4 then print "CTRL-";
    if sh and 2 then print "LSHIFT-";
    if sh and 1 then print "RSHIFT-";
    }

proc escan
    {
    l=low ax:ax=high ax
    if ax>132 then print "<?>";
    else
	{
	c=peekb(kbase+ax)
	if (c>20) and (l>31) then print chr l;
	else
	    {
	    if c<21 then
		{
		a=(searchb 2000 from ebase for c)+1
		while peekb a<>255
		    {
		    if main and ((peekb a='<') or (peekb a='>')) then goto incm
		    print chr peekb a;
		    incm:
		    a++
		    }
		}
	    else
		{
		print chr c;
		}
	    }
	}
    }

function found
    {
    mem=0
    shift=(peek 0|417h) and 15
    while mem<(nextk*3)
	{
	if (retax=kseg[mem]) and (shift=kseg[mem+2]b) then
	    {
	    address=mem+6
	    finish=kseg[mem+3]
	    return 1
	    }
	mem=kseg[mem+3]
	}
    return 0
    }

proc process
    {
    if running and (not defining) and (not playback) then
	{
	if found then playback=1
	}
    }

x=81h

forever
{
while peekb x=' ' x++
if peekb x=13 then goto startm
if peekb x='/' then
    {
    x++:keys=0

    loopm:
    c=peekb x-'0'
    if (c<0) or (c>9) then goto exitm
    keys*=10
    keys+=c
    x++
    goto loopm

    exitm:
    if keys<100 then keys=100
    if keys>maxk then keys=maxk
    }
else
    {
    m=name
    while (peekb x<>13) and (peekb x<>' ')
	{
	pokeb m,ucase peekb x
	x++:m++
	}
    pokeb m,0
    }
}

startm:
kseg=allocate ((keys*3)+15)/16
kspace=keys*3:nextk=0
fillb kspace from kseg|0 with 0

if peekb name then
    {
    print bios "  Using file: ";
    m=name
    while peekb m print bios chr peekb m;:m++
    open #1,name
    loadfile
    }
print bios "  "keys" keys."
print bios "Installed!"

dos 35(16)
poke oseg,reg es:poke ooff,reg bx
reg dx=new16:dos 25(16)

running=1
playback=0
defining=0
translate=1
internal=0

stop resident

another:
inline 9dh
popall

new16:
enable interupts
pushall

push reg ax

inline 2eh
if playback then
    {
    reg ds=reg cs
    pop ff
    inline 9ch
    retax=kseg[address]
    if (high ff<>0) and (high ff<>16) then goto play16
    address+=3
    if address>=finish then playback=0
    goto play16
    }

inline 58h
push reg ax
inline 9ch,2eh,9ah ; CALLF CS:[old int 16h]
ooff:
data 0
oseg:
data 0

inline 2eh
retax=reg ax
reg ds=reg cs
pop ff
inline 9ch

play16:
if internal then goto ret16
if (high ff<>0) and (high ff<>16) then goto ret16

if defining and (retax<>1) then
    {
    ad=place*3
    if ad>=kspace then beep:defining=0:goto enddef
    kseg[ad]=retax
    kseg[ad+2]b=(peek 0|417h) and 15
    if place=start then
	{
	if found then
	    {
	    ;Delete old definition.
	    moveb kspace-finish from kseg|finish to kseg|mem
	    d=finish-mem
	    nextk-=d/3
	    while (mem/3)<=nextk
		{
		kseg[mem+3]-=d
		mem=kseg[mem+3]
		}
	    }
	place=nextk+2:start=nextk
	goto another
	}
    place++
    goto ret16
    }

if not translate then
    {
    translate=1
    goto ret16
    }

p=playback:process:if p<>playback then goto another

if retax=111 then internal=1

if not running then goto check_toggle

if retax=1 then
    {
    if defining then
	{
	enddef:
	cursor size 6+6*mono,7+6*mono
	ad=(start+1)*3
	kseg[ad]=place*3 ; Set start address for next keydef.
	if place>nextk+2 then nextk=place ; else no definition.
	defining=0
	}
    else
	{
	cursor size 0,7+6*mono
	defining=1
	place=nextk
	start=nextk
	}
    goto another
    }

if retax=7 then
    {
    if dseg<>reg ss
	then gosub functions
	else for a=400 to 5000:noise 2,a:next a:noise off
    goto another
    }

if retax=4 then
    {
    translate=0
    goto another
    }

check_toggle:
if retax=9 then
    {
    running=not running ; Toggle running mode.
    goto another
    }

ret16:
inline 9dh
popall
inline 2eh
reg ax=retax
inline 0cah,2,0 ; RETF 0002


functions:
video=(0b800h-(800h*mono))+page*100h
internal=1
pokeb funct+1,0:open window funct:pokeb funct+1,4
colour 7:locate 13,32:print "> "keys-nextk" free.";

x=1
forever
    {
    x=select funct,x
    if not x then internal=0:close window:return
    colour 78h

    if x=1 then
	{
	open window help
	wait for keyscan
	close window
	}

    if x=2 then
	{
	open window namew
	oc=curpos
	locate 10,42:print "Load:"
	locate 13,42:print "(Press ESC or ENTER to abort)"
	cursor 12,42:print bios "> ";
	inputs iname
	curpos=oc
	close window
	if peekb (iname+2)=0 then goto nextm
	#errors off
	open #1,iname+2
	#errors on
	if error then beep:beep
	else
	    {
	    move 32 from iname+2 to name
	    loadfile
	    }
	}

    if x=3 then
	{
	open window namew
	locate 10,42:print "Save: "
	locate 12,42:print "> ";
	m=name:while peekb m print chr ucase peekb m;:m++
	oc=curpos
	cursor 12,42
	if nextk=0 then
	    {
	    print bios "No keys to save, press ESC!"
	    wait for key=27
	    goto exitsave
	    }
	if peekb name=0 then
	    {
	    print bios "Save name > ";
	    inputs iname
	    if peekb (iname+2)=0 then goto exitsave
	    move 32 from iname+2 to name
	    }

	#errors off
	save name,kseg|0,nextk*3
	#errors on
	if error then beep:beep

	exitsave:
	curpos=oc
	close window
	}

    if x=4 then
	{
	open window viewm
	km=0:y=2:colour 7

	while km<nextk
	    {
	    incy
	    ad=km*3:ax=kseg[ad]:sh=kseg[ad+2]b
	    main=1:locate y,2:eshift:escan:main=0
	    locate y,15
	    finish=kseg[ad+3]
	    km+=2
	    while km*3<finish
		{
		if (locpos mod 160)>120 then
		    {
		    incy
		    }
		a=0
		ax=kseg[km*3]:escan
		if a=edb then incy
		km++
		}
	    }

	wait for keyscan
	close window
	}

    nextm:
    }

viewm:
datab 1,0,0,0,70,24,7
datab 22,2,1,'KEY'
datab 22,15,1,'EXPANSION'
datab 26

funct:
datab 1,4,30,6,49,14,1fh
datab 22,3,1,'RKEY FUNCTIONS'
datab 22,2,3,'Help'
datab 22,2,4,'Load keys'
datab 22,2,5,'Save keys'
datab 22,2,6,'View keys'
datab 26

help:
datab 1,0,0,13,79,24,60h
datab 22,2,1,'RKEY  Key:    Function:'
datab 22,8,3,	   'ALT-1   Start/stop key definition.'
datab 22,8,5,	   'ALT-7   RKEY function menu. HELP/LOAD/SAVE/VIEW'
datab 22,8,5,	   'ALT-4   Ignores translation of next key.'
datab 22,8,6,	   'ALT-9   Activate/Deactivate RKEY.'
datab 22,8,7,	   'ALT-111 Abort RKEY (removes RKEY from memory).'
datab 22,2,9,'Note: The number after ALT must be typed using the keypad and'
datab		    ' with the'
datab 22,8,10,	    'ALT key pressed all the time - release ALT to work.'
datab 26

namew:
datab 0,0,40,8,79,15,78h,26

iname:
string 20

kbase:
datab 20,0,'1234567890-='
datab 1,2,'qwertyuiop[]',3
datab 20,'asdfghjkl;''`',20,'\zxcvbnm,./',20,'*',20,' ',20
datab 4,5,6,7,8,9,10,11,12,13,20,20
datab 14,24,15,'-',27,20,26,'+'
datab 16,25,17,18,19
datab 4,5,6,7,8,9,10,11,12,13
datab 4,5,6,7,8,9,10,11,12,13
datab 4,5,6,7,8,9,10,11,12,13
datab 20,27,26,16,17,14
datab '1234567890-=',15

ebase:
datab 0,'<esc>',255
datab 1,'<backspace>',255
datab 2,'<tab>',255

datab 3,'<enter>'
edb:
datab 255

datab 4,'<F1>',255
datab 5,'<F2>',255
datab 6,'<F3>',255
datab 7,'<F4>',255
datab 8,'<F5>',255
datab 9,'<F6>',255
datab 10,'<F7>',255
datab 11,'<F8>',255
datab 12,'<F9>',255
datab 13,'<F10>',255
datab 14,'<home>',255
datab 15,'<PgUp>',255
datab 16,'<end>',255
datab 17,'<PgDn>',255
datab 18,'<ins>',255
datab 19,'<del>',255
datab 20,'<?>',255

name:
space 64
