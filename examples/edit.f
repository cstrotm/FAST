
;==============================================================================
; EDIT is a dos command line editor - with popup menu for last 16 commands
;==============================================================================

print bios "EDIT - By Peter Campbell, v3.1"
#window memory 3000:free memory
#short
var tx
const len=60,vect=21h*4
command ? 130
last_command ? 130
last ? len*16
mono*=5:lasttyped=0
fill len*8 from last with 0d0dh

procedure insert_char(putchar)
    {
    moveb 128-tx from command+tx to command+1+tx
    pokeb command+tx,putchar:tx++
    }

dos 35(21):dosseg=reg es:dosoff=reg bx
poke oldseg,dosseg:poke oldoff,dosoff
poke 0|vect,set_signal:poke 0|vect+2,reg cs
stop resident

set_signal:
inline 9ch
inline 80h,0fch,0ah
inline 74h,6
inline 9dh
inline 0eah
oldoff:
data 0
oldseg:
data 0

begin:
inline 0fbh,9dh
pushall

push reg ds
reg ds=reg cs
offset=reg dx
pop segment
pos=curpos

length=segment[offset]b
if length>128 then length=128
tx=0:typed=0:tl=0:insert=0
fill 64 from command with 2020h:changed=0
prted=0

loop:
curpos=pos:prt=0
while prt<typed  print bios chr peek (command+prt);:prt++
while prted>prt  print bios " ";:prted--
prted=prt

curpos=pos+tx:cursor size 6+mono-(insert*(2+mono/2)),7+mono

wait for keypressed:test break
ks=keyscan:ss=high ks:kk=low ks

if ks=11776 then
    {
    cursor 0,0:pos=0
    if mono then screen 3 else screen 7
    }

if ks=11520 then
    {
    poke 0|vect,dosoff:poke 0|vect+2,dosseg
    deallocate reg cs:if error then print bios "Memory error!":beep
    goto endcommand
    }

#long
if ks=15616 then
    {
    #short
    while (tl<lasttyped) and (tx<length)
	{
	kk=peek (last_command+tl)
	if insert then
	    {
	    if typed<length then insert_char(kk):typed++ else beep
	    }
	else
	    {
	    pokeb command+tx,kk:tx++
	    if tx>typed then typed=tx
	    }
	tl++:if tl>127 then tl=127
	}
    goto loop
    #long
    }
#short

if ks=18176 then tx=0:tl=0
if ks=20224 then tx=typed:tl=typed
if ks=19200 then if tx then tx--:tl--
if ks=19712 then if tx<length then tx++:tl++
if ks=21248 then
    {
    if tx<typed then typed--
    if tl<127 then tl++
    moveb 128-tx from command+tx+1 to command+tx
    changed=1
    }
if (ks=29952) or (ks=16384) then
    {
    if tx<typed then typed=tx
    changed=1
    goto loop
    }
if ks=15104 then kk=peekb (last_command+tl)
if ks=20992 then insert=not insert
if kk=13 then goto endcommand
if (kk=27) or (ks=16128) then typed=0:tx=0:tl=0:goto loop
if ks=3592 then
    {
    if tx then
	{
	moveb 128-tx from command+tx to command-1+tx
	tx--:tl--
	if typed then typed--
	changed=1
	}
    goto loop
    }
if (ss=72) or (ss=80) then gosub select_old:goto loop
if kk=0 then goto loop

if typed<(length-1) then
    {
    changed=1
    if not insert then
	{
	tl++
	pokeb command+tx,kk
	tx++
	if tx>typed then typed=tx
	}
    else
	{
	if ks=15104 then tl++
	insert_char(kk)
	typed++
	}
    }
goto loop

endcommand:
pokeb command+typed,13
moveb typed+1 from command to segment|offset+2
segment[offset+1]b=typed
if (typed>1) and changed then
    {
    moveb len*15 from last+len to last
    moveb len from command to last+15*len
    pokeb last+16*len-1,13
    }
lasttyped=typed
fill 64 from last_command with 2020h
moveb typed from command to last_command

popall:iret

select_old:
pokeb oldkeys+1,0
open window oldkeys
start=last
for a=0 to 15
    locate a+6,17
    p=start:start+=len
    while peekb p<>13 print chr peek p;:p++
next a

pokeb oldkeys+1,16
x=select oldkeys,16
close window
if x=0 then return
changed=0
des=last+(x-1)*len
for a=0 to 59
    pokeb command+a,peek(des+a):typed=a
    if peekb (des+a)=13 then goto fin_select
next a
fin_select:
tx=typed:tl=typed
return

oldkeys:
datab 2,16,15,3,79,22,1110b
datab 22,23,1,'Select a command -'
datab 26
