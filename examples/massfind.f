
;==============================================================================
; find several items in very large file without reading file many times
;==============================================================================

;7/10/1994 (c) PCS

;change file name and search details in source code!!!!!

var32 addr,n32

const size=20480
buffer ? size+100

;==============================================================================

goto skip_data

search_data:
datab 5,'21473'
datab 5,'21479'
datab 5,'21483'
datab 5,'21489'
datab 5,'21494'
datab 5,'21495'
datab 5,'21497'
datab 5,'21501'
datab 6,'300263'
datab 6,'300264'
datab 6,'300265'
datab 0

skip_data:

;==============================================================================

function search_match(smo)
    {
    m=search_data

    while peekb m
	{
	l=peekb m:m++

	if not compareb l at buffer+smo with m then return 1

	m+=l
	}

    return 0
    }

;==============================================================================

proc display_data(ddo)
    {
    n32=addr+ddo
    printh bios "Found at ";high n32;low n32
    }

;==============================================================================

print bios "open file"
open #1,"as.fbd"                ;<<<<<<<< file name
addr=0

;==============================================================================

forever
    {
    curtoloc:printh high addr;low addr;

    seek #1,addr
    len=read #1,size+100 to buffer
    if len=0 then goto close_file
    if key=27 then beep:stop

    off=0
    while off<len
	{
	curtoloc:locpos+=20:printh off;
	if search_match(off) then display_data(off)
	off++
	}

    addr+=size
    }

close_file:
print bios "close"
close #1
print bios "ok"
stop
