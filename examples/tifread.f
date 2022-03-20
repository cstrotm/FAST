
;===========================================================================
; TEST reading TIF files
;===========================================================================

var32 ifd_offset,ifd_values,ifd_data
var32 n32

seg=allocate 4096
byte_order=seg[0]b

#include \fast\cmd_line.fi

print bios "TIFREAD - test program to view TIF files."

if cmd_nos<>1 then
    {
    print bios "Syntax error. Usage: TIFREAD filename"
    beep:stop
    }

pokeb cmd_add(1)+cmd_len(1),0
load cmd_add(1),seg|0

;== procs ==================================================================

function getw(o)
    {
    if byte_order='I' then return seg[o+1]b*256+seg[o]b
    return seg[o]
    }

function getl(o)
    {
    if byte_order='I' then l=getw(o):h=getw(o+2)
    else h=getw(o):l=getw(o+2)
    n32=h*65536+l
    }

;== start ==================================================================

if getw(2)<>42 then error 13

getl(4)
ifd_offset=n32

ifd_entries=getw(low ifd_offset)
ifd_offset+=2

print bios "TIFF entries = ";ifd_entries
print bios

print bios "Addr  TAG   Type  Count     Data"
print bios "------------------------------------"

repeat ifd_entries
    {
    tag=getw(low ifd_offset)
    type=getw(low ifd_offset+2)
    getl(low ifd_offset+4)
    ifd_values=n32
    getl(low ifd_offset+8)
    ifd_data=n32

    printh bios low ifd_offset;"  ";tag;"  ";type;"  ";high ifd_values;low ifd_values;"  ";high ifd_data;low ifd_data;" ";

    printh bios seg[low ifd_data];" ";

    if type=2 then
	{
	m=low ifd_data
	repeat low ifd_values
	    {
	    print chr seg[m]b;:m++
	    }
	}

    print bios

    ifd_offset+=12
    }
