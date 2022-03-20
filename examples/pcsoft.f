
;===========================================================================
;=	       PETER CAMPBELL SOFTWARE - ONE FINGERED TYPIST		   =
;===========================================================================

;This program when loaded (say in AUTOEXEC.BAT) will provide users with the
;ability to type with one finger.
;
;When a shift key (Shift, Alt or Control) is pushed then the keyboard status
;remains the same until another key is pushed, giving the shift key a delayed
;effect.

#stack memory 200
var32 int9v

print bios
print bios "PCS OFT - Peter Campbell Software One Fingered Typist" cr lf
print bios " Simply press any Shift key(s) to make use of PCSOFT"
print bios "  To turn PCSOFT off then press all the shift keys."

int9v=getint 9:int9s=high int9v:int9o=low int9v
setint 9 to oft
shifting=0:down=0

stop resident

;== OFT ====================================================================

oft:
pushall
reg ds=reg cs
iscan=in 60h
inline 9ch
call int9s|int9o
skq=searchb 8 from scan_codes for iscan
if skq=0 then
    {
    if down=0 then pokeb 0|417h,peekb 0|417h and 240
    shifting=0
    }
else
    {
    skq-=scan_codes
    n=peekb (power2+skq)
    if skq>=4 then
	{
	down=down and (15-n)
	if shifting then
	    {
	    pokeb 0|417h,peekb 0|417h or n
	    if (peekb 0|417h and 15)=15 then
		{
		reg dx=int9o,ds=int9s:dos 25(9)
		reg ds=reg cs
		pokeb 0|417h,peekb 0|417h and 240
		}
	    }
	}
    else
	{
	shifting=1
	down=down or n
	}
    }
popall
iret

scan_codes:
datab 036h,02ah,01dh,038h
datab 0b6h,0aah,09dh,0b8h

power2: datab 1,2,4,8,1,2,4,8
