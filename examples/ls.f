#short
print bios "LS Resident."
indo=indoso:inds=indoss

proc program
    {
    ls=serial_status
    locate 0,76:printh ls;
    }

;== Main entry =============================================================

on int 1
    {
    if inds[indo]b=0 then program
    }
stop resident
