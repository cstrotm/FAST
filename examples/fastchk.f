
;==============================================================================
; FAST CHECK - create file, write/read to various parts in multi-user
;==============================================================================

;open/create a file called "fastchk.dat"
;forever...
;    write to 50 random addresses (address-4 bytes)
;    read 500 times any of the 50 random writes and check values

;Test statistics:
;   my local drive	    - NO ERRORS
;   novell netware 3.12     - NO ERRORS
;   a client's machine      - 4/59 ERRORS (was reporting application bugs)

;This test works in multi-user mode so run SHARE if performing the test
;on local hard drives over a peer-peer network (eg: Novell Lite, Personal
;Netware or LanTastic).

;When testing a network run this test on as many machines as possible,
;all from the same directory.

;Delete the file fastchk.dat after returning to DOS.

;Must have 32MB free disk space - change number below for more/less
const max_size=511	;511*65536+65535 = 32MB

#errors off
#short

const max_writes=50
const max_tests=500

var32 addr,test32

seek_table ? max_writes*4
buffer	   ? 4

;==============================================================================

on error
    {
    error_number=error
    print bios
    print bios "Error ";
    error msg "\dos.err",error_number
    beep
    stop
    }

;==============================================================================

print bios "FAST CHECK v1.00 (c) Peter Campbell Software 1994."
print bios

#open 01000010b
print bios "Open 'fastchk.dat'"
open #1,"fastchk.dat"
if error then
    {
    if error<>2 then error
    print bios "  not found... create 'fastchk.dat'"
    create #1,"fastchk.dat"
    if error then error
    }

test_no=1
total_errors=0

start_test:
print bios "Test #";test_no;" - ";

m=seek_table
repeat max_writes
    {
    if key=27 then goto stop_testing

    lo=rnd and 0fffch	;always mod 4
    hi=rnd and max_size ;range upto 32MB
    addr=hi*65536+lo

    curtoloc:printh high addr;low addr;
    seek #1,addr
    if error then
	{
	printh bios "wseek error #";error;" @";high addr;low addr
	total_errors++
	beep:goto next_test
	}

    poke m,lo
    poke m+2,hi

    write #1,4 from m
    w=reg ax
    if error then
	{
	printh bios "write error #";error;" @";high addr;low addr
	total_errors++
	beep:goto next_test
	}
    if w<>4 then
	{
	printh bios "write len 0004<>";w;" @";high addr;low addr
	total_errors++
	beep:goto next_test
	}

    m+=4
    }

repeat max_tests
    {
    if key=27 then goto stop_testing

    m=seek_table+(rnd mod max_writes)*4
    addr=peek m

    curtoloc:printh high addr;low addr;
    seek #1,addr
    if error then
	{
	printh bios "rseek error #";error;" @";high addr;low addr
	total_errors++
	beep:goto next_test
	}

    r=read #1,4 to buffer
    if error then
	{
	printh bios "read error #";error;" @";high addr;low addr
	total_errors++
	beep:goto next_test
	}
    if r<>4 then
	{
	printh bios "read len 0004<>";r;" @";high addr;low addr
	total_errors++
	beep:goto next_test
	}
    test32=peek buffer
    if32 test32<>addr then
	{
	printh bios "data error @";high addr;low addr;" <> ";high test32;low test32
	total_errors++
	beep:goto next_test
	}
    }

print bios "Ok      "

next_test:
#long
if key=27 then
    {
    stop_testing:
    #short
    print bios
    print bios "FastChk aborted after ";test_no;" test(s)."
    if total_errors then print bios "ERRORS = ";total_errors
    else print bios "NO ERRORS"
    #errors on
    close #1
    print bios
    print bios "Delete 'fastchk.dat'? ";
    wait for keypressed
    if (lcase key)='y' then delete "fastchk.dat"
    stop
    }

test_no++
goto start_test
