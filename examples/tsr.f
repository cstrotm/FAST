;===========================================================================
;=	      TSR Skeleton (c) Peter Campbell, June 1989.		   =
;===========================================================================

;The parts of this program commented out are only used if you want to see how
;TSR will popup.

;The "video=" statement is needed because some applications change video page.

#short
print bios "TSR Resident (Shift-Shift to activate)."
indo=indoso:inds=indoss

proc program
    {
    using=1
    activate=0
    old_psp=psp:psp reg cs

;   oldpos=curpos
;   video=(0b800h-(800h*mono))+page*100h
;   open window w_here
;   wait for key=27
;   close window
;   curpos=oldpos

    psp old_psp
    using=0
    }

;== Main entry =============================================================

activate=0  ;Currently not trying.
on int 1
    {
    if ((peek 0|417h) and 3)=3 then activate=1
    if activate and (inds[indo]b=0) then program
    }
on idle
    {
    if (activate=1) and (using=0) then program
    }

stop resident

;== Data ===================================================================

;w_here:
;datab 0,0,60,18,77,22,15
;datab 22,2,2,'Press ESC.'
;datab 26
