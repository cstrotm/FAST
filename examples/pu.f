;===========================================================================
;=  PRINTER UTILITY, written in FAST. September 1987 (Updated May 89)	   =
;=  By Peter Campbell							   =
;===========================================================================

print bios "PRINTER UTILITIES Written by Peter Campbell	 (Shift-Shift to activate)."
#window memory 5000
#include extinput.fi
#errors off
#short
var using
const usere=30,userd=21*4,usert=21*usere+userd
indo=indoso:inds=indoss

dos 35(17):poke pioff,reg bx:poke piseg,reg es
setint 17h to prt_handle
current_prt=0

cfg=allocate usert/16+1
if error then print bios "PU: Out of memory, need 16K."
open #1,config
if error then
    {
    cfg_default:
    fillb usert from cfg|0 with 0ffffh
    moveb userd from cfg_codes to cfg|0
    }
else
    {
    re=read #1,usert to cfg|0
    if error then close #1:goto cfg_default
    close #1
    if error then goto cfg_default
    if re<>usert then goto cfg_default
    }

activate=0  ;Currently not trying.
on int 1
    {
    if ((peek 0|417h) and 3)=3 then activate=1
    if activate and (inds[indo]b=0) then gosub printer_util
    }
;on idle    problem with reg DS
;    {
;    if (activate=1) and (using=0) then gosub printer_util
;    }

stop resident

;== routines ===============================================================

function ready
    {
    out_status:
    reg dx=current_prt,ax=200h:int 17h:status=high reg ax
    if (status and 10101001b)<>10000000b then
	{
	open window severe:beep
	wout:
	k=ucase key
	if (k=27) or (k='A') then close window:return 0
	if k<>'R' then goto wout
	close window
	goto out_status
	}
    return 1
    }

proc out_codes(a)
    {
    if not ready then return
    while cfg[a]b<>255 lprint chr cfg[a];:a++
    }

;== printer_util ===========================================================

printer_util:
using=1
oldpos=curpos
activate=0
old_psp=psp:psp reg cs
video=(0b800h-(800h*mono))+page*100h
opt=menu main_print:goto print_2

forever
{
opt=select main_print,opt
print_2:
if (opt=0) or (opt=10) then
    {
    printer_ret:
    close window:curpos=oldpos
    psp old_psp
    using=0
    return
    }
if opt=5 then if ready then lprint
if opt=6 then if ready then lprint ff;
if opt=7 then if ready then lprint chr 27;chr '@';
if opt=1 then gosub set_mode
if opt=9 then gosub input_msg
if opt=11 then
    {
    reg dx=peek pioff,ds=peek piseg:dos 25(17)
    reg ds=reg cs
    reset interupts
    deallocate cfg
    deallocate reg cs
    stop idle
    goto printer_ret
    }
if opt=2 then gosub alter_mode
if opt=4 then gosub change_printer
if opt=3 then gosub user_codes
if opt=8 then
    {
    save config,cfg|0,usert
    if error then beep
    }
}
;===========================================================================
set_mode:
code=menu mode_options:goto set_mode2
forever
    {
    code=select mode_options,code
    set_mode2:
    if code=0 then close window:return
    out_codes(code*4-4)
    }
;===========================================================================
alter_mode:
as=1:pokeb mode_options+1,0:open window mode_options
pokeb mode_options+1,21
am1:
base=0
colour 64
for y=3 to 23
locate y,47
x=base:base+=4
print "           ";:locate y,47
while cfg[x]b<>255
    {
    printb cfg[x]b;" ";
    x++
    }
next y

as=select mode_options,as
if as=0 then close window:return
base=4*as-4
open window input_codes
cursor 16,32:c1=inputb
if c1=0 then goto no_input
cursor 16,39:c2=inputb
if c2=0 then c2=255:goto put_code
cursor 16,46:c3=inputb
if c3=0 then c3=255
put_code:
cfg[base]b=c1
cfg[base+1]b=c2
cfg[base+2]b=c3

no_input:
close window
goto am1
;===========================================================================
change_printer:
open window input_change
cursor 21,70
current_prt=inputb
close window:return
;===========================================================================
user_codes:
pokeb user_code_names+1,0
open window user_code_names
pokeb user_code_names+1,21
base=userd
colour 15
for y=3 to 23
locate y,33:print "                                    ";
a=base
locate y,12:while cfg[a]b<>255 print chr cfg[a];:a++
a=base+20:base+=30
locate y,33
while cfg[a]b<>255 printb cfg[a];" ";:a++
next y

colour 7
uc=menu get_function
close window
if uc=0 then close window:return
if uc=1 then gosub select_user_code
if uc=2 then gosub user_add
close window
goto user_codes

select_user_code:
forever
    {
    code=select user_code_names,1
    if code=0 then return
    out_codes(userd+20+(code-1)*usere)
    }

user_add:
code=select user_code_names,1
if code=0 then return
ad=userd+(code-1)*usere
open window enter_window
colour 31:locate 14,23:print "Description: ";
move 10 from cfg|ad to enter_name+2
x=searchb 20 from enter_name+2 for 255:if x then pokeb x,13
l=ext_input(enter_name)
if l then pokeb enter_name+l+1,255
move 10 from enter_name+2 to cfg|ad

x=0:ad+=20
repeat 9 locate 16,x+14:print "[   ]";:x+=6
byte=1:x=0
while (x<9) and (byte<>0)
    {
    cursor 16,(x*6)+15
    byte=inputb
    cfg[ad]b=byte
    ad++:x++
    }
cfg[ad-1]b=255
close window
return
;===========================================================================
input_msg:
open window msg_prt
locate 12,5
colour 60h
l=ext_clean_input(msg)
if l then
    {
    x=msg+2
    if not ready then goto exit_msg
    repeat l lprint chr peek x;:x++
    if l then lprint
    exit_msg:
    }
close window
return

;== Print through BIOS with print re-direction =============================

prt_handle:
push reg ax
inline 2eh
push reg ds
reg ds=reg cs
reg dx=current_prt
pop ds
reg ds=ds
inline 58h,0eah
pioff:
data 0
piseg:
data 0

;== PU data ================================================================

config:
fname 'pu.cfg'

input_change:
datab 1,0,64,17,77,23,111b
datab 22,1,1,'Printer? 0-2'
datab 26

msg:
string 40

msg_prt:
datab 1,0,2,8,47,14,60h
datab 22,3,1,'Input message:',26

cfg_codes:
datab 27,'4',255,0
datab 27,'5',255,0
datab 27,'S0',255
datab 27,'S1',255
datab 27,'T',255,0
datab 27,'P',255,0
datab 27,'M',255,0
datab 15,255,0,0
datab 27,'E',255,0
datab 27,'F',255,0
datab 27,'W1',255
datab 27,'W0',255
datab 27,'-1',255
datab 27,'-0',255
datab 27,'2',255,0
datab 27,'0',255,0
datab 24,255,0,0
datab 27,'n',255,0
datab 27,'o',255,0
datab 27,'p1',255
datab 27,'p0',255

main_print:
datab 1,11,50,3,75,17,1101000b
datab 22,2,1,'PRINTER UTILITIES - PC'
datab 22,2,3,'Set printer to...'
datab 22,2,4,'Alter main codes.'
datab 22,2,5,'Update/Set new codes.'
datab 22,2,6,'Change current printer.'
datab 22,2,7,'Line feed.'
datab 22,2,8,'Form feed.'
datab 22,2,9,'Reset printer.'
datab 22,2,10,'Keep options (save).'
datab 22,2,11,'Print message.'
datab 22,2,12,'ESCape.'
datab 22,2,13,'Disable PU.'
datab 26

mode_options:
datab 2,21,17,0,63,24,1000000b
datab 22,6,1,'PRINTER CONTROLS'
datab 22,2,3,'Italics ON'
datab 22,2,4,'Italics OFF'
datab 22,2,5,'Superscript ON'
datab 22,2,6,'Subscript ON'
datab 22,2,7,'Super/Sub OFF'
datab 22,2,8,'10 cpi'
datab 22,2,9,'12 cpi'
datab 22,2,10,'17 cpi'
datab 22,2,11,'Emphasised ON'
datab 22,2,12,'Emphasised OFF'
datab 22,2,13,'Double printing ON'
datab 22,2,14,'Double printing OFF'
datab 22,2,15,'Underline ON'
datab 22,2,16,'Underline OFF'
datab 22,2,17,'Paper feed 1/6'
datab 22,2,18,'Paper feed 1/8'
datab 22,2,19,'Clear buffer'
datab 22,2,20,'NLQ 10cpi ON'
datab 22,2,21,'NLQ 12cpi ON'
datab 22,2,22,'Proportional spacing ON'
datab 22,2,23,'Proportional spacing OFF'
datab 26

input_codes:
datab 1,0,29,12,51,18,1110000b
datab 22,2,1,'Enter up to 3 codes'
datab 22,2,4,'[   ]  [	 ]  [	]'
datab 26

get_function:
datab 1,2,0,4,10,9,0111111b
datab 22,2,1,'OPTIONS'
datab 22,2,3,'Set'
datab 22,2,4,'Add'
datab 26

user_code_names:
datab 2,21,10,0,70,24,15
datab 22,20,1,'USER DEFINABLE CODES'
datab 26

enter_name:
string 20

enter_window:
datab 1,0,11,11,69,18,31
datab 22,20,1,'USER DEFINABLE CODE'
datab 26

severe:
datab 0,0,29,10,50,15,120
datab 22,2,2,'Printer Not Ready!'
datab 22,5,3,'Abort/Retry?'
datab 26
