;KEY redifinition program, modified for DEMOKEYS.
;
;functions: learn demo
;	    edit demo keys
;	    play demo - repeating
;
;variables: keystroke delay
;	    enter-key delay (delay after enter, before next key)
;
;start demokeys menu by pressing ALT-7
;stop demokeys during playback with CTRL-ALT

const max=10000

var defining,playback,internal
var mem,kseg,len,enter

key_delay=12
enter_delay=70
done_delay=150

delay_time=15001

#short
#errors off

proc put_byte(b)
    {
    rl=b and 15
    rh=(b and 240)/16
    rl+='0':if rl>'9' then rl+=7
    rh+='0':if rh>'9' then rh+=7
    kseg[len]b=rh
    kseg[len+1]b=rl
    len+=2
    }

proc put_hex(n)
    {
    kseg[len]b='#'
    len++
    put_byte(high n)
    put_byte(low n)
    }

function get_byte(b)
    {
    rh=kseg[b]b
    rl=kseg[b+1]b
    rh-='0':if rh>9 then rh-=7
    rl-='0':if rl>9 then rl-=7
    return rh*16+rl
    }

function get_hex(m)
    {
    return get_byte(m)*256+get_byte(m+2)
    }

proc delay(n)
    {
    repeat n repeat delay_time {}
    }

;== setup tsr ==============================================================

startm:
print bios
print bios "DEMOKEYS: (c) Peter Campbell Software."

kseg=allocate max/16+1

open #1,"demokeys.key"
if error=0 then
    {
    len=read #1,max to kseg|0
    close #1
    }

dos 35(16)
poke oseg,reg es:poke ooff,reg bx
reg dx=new16:dos 25(16)

playback=0
defining=0
internal=0

left_shift=0
right_shift=0

on int 1
    {
    #long
    if playback then
	{
	#short
	shift=peekb 0040h|17h
	if (shift and 12)=12 then playback=0
	if shift and 2 then if left_shift=0 then
	    {
	    if delay_time>3000 then delay_time-=3000
	    left_shift=1
	    }
	else left_shift=0
	if shift and 1 then if right_shift=0 then
	    {
	    if delay_time<21000 then delay_time+=3000
	    right_shift=1
	    }
	else right_shift=0
	}
    }

stop resident

;== interupt handling ======================================================

another:
inline 9dh
popall

new16:
enable interupts
pushall

push reg ax

inline 2eh
#long
if playback then
    {
    #short
    reg ds=reg cs
    pop ff
    inline 9ch
    retax=kseg[mem]b:rlen=1
    del=1
    if retax=13 then		;enter?
	{
	retax=7181
	del=enter_delay
	}
    if retax='#' then
	{
	retax=get_hex(mem+1)
	rlen=5
	del=key_delay
	if retax=20011 then	;done key?
	    {
	    del=done_delay	;delay also
	    }
	if retax=5497 then del+=enter_delay
	}
    if (high ff<>0) and (high ff<>16) then goto play16
    delay(del)
    mem+=rlen
    if mem>=len then	    ;end of demo?
	{
	mem=0		    ;repeat demo
	}
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

;== keyboard handler =======================================================

play16:
if internal then goto ret16
if (high ff<>0) and (high ff<>16) then goto ret16

#long
if defining and (retax<>7) then
    {
    if len>=max then defining=0:beep
    else
	{
	#short
	if retax=7181 then	;enter?
	    {
	    kseg[len]b=13
	    len++
	    }
	else
	    {
	    put_hex(retax)	;#hhhh (5 bytes)
	    }
	}
    goto ret16
    }

if retax=7 then
    {
    gosub functions
    goto another
    }

ret16:
inline 9dh
popall
inline 2eh
reg ax=retax
inline 0cah,2,0     ;RETF 0002


;== menu ===================================================================

functions:
video=(0b800h-(800h*mono))+page*100h
internal=1

if defining then defining=0

forever
    {
    x=menu funct
    if not x then goto end_functions
    colour 78h

    if x=1 then 	;learn
	{
	defining=1
	len=0
	goto end_functions
	}

    if x=2 then 	;playback
	{
	playback=1
	mem=0
	goto end_functions
	}

    if x=3 then 	;save
	{
	save "demokeys.key",kseg|0,len
	}

    if x=4 then 	;key delay
	{
	}

    if x=5 then 	;enter delay
	{
	}

    if x=6 then 	;quit
	{
	deallocate kseg 	;free key segment
	stop int 1
	goto end_functions2	;keep internal on!
	}

    close window
    }

end_functions:
internal=0
end_functions2:
close window
return

;== data ===================================================================

funct:
datab 1,6,20,6,42,15,1fh
datab 22,2,1,'DEMOKEYS v1.00'
datab 22,2,3,'Learn'
datab 22,2,4,'Playback Demo'
datab 22,2,5,'Save "demokeys.key"'
datab 22,2,6,'Key Delay'
datab 22,2,7,'Enter Delay'
datab 22,2,8,'Quit Demokeys'
datab 26
