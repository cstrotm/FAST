
;test file locking for FastBase

var32 multi_data1,multi_data2

#open 01000010b 	;everyone can read/write
open #1,"file.tst"

function multi_set_rr
    {
    reg cx=high multi_data1,dx=low multi_data1
    reg si=high multi_data2,di=low multi_data2
    reg bx=handle #1
    }

function multi_lock
    {
    multi_set_rr
    dos 5c(00)
    err=reg ax
    if carry then error err
    }

function multi_unlock
    {
    multi_set_rr
    dos 5c(01)
    err=reg ax
    if carry then error err
    }

multi_data1=0
multi_data2=0ffffffffh

multi_lock

printh bios "File is locked from ";high multi_data1;low multi_data1;
printh bios " length=";high multi_data2;low multi_data2

print bios "<press any key>"
wait for key

print bios "unlocking... ";
multi_unlock
print bios "ok"

stop
