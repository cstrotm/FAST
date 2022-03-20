
;==============================================================================
; command: D [filespec]
;
; Print directory of files in 4 columns with size in k (sorted by name).
; (c) Peter Campbell Software
;==============================================================================

;11/01/93 print using bios always so directory can be redirected.
;	  when printing total file sizes and free space test print in K if
;	  less than 20,000 else print using Megabytes, eg: 500K or 90MB.
;

#short

#include fsort.fi

var32 fsize,tsize,freek,totald,n32
const dlen=22

waiting=0
tsize=0

;== procedures ================================================================

proc abort beep:stop

on error
    {
    print bios
    error msg "\dos.err"
    print bios "!"
    abort
    }

proc print_large(addr)
    {
    n32=peek addr
    if32 n32>10000 then
	{
	n32=(n32+1023)/1024
	print bios low n32;"MB";
	}
    else print bios low n32;"K";
    }

proc get_file_spec
    {
    m=81h
    while peekb m<>13
	{
	b=peekb m
	if (b<>' ') and (b<>9) then goto use_line
	m++
	}
    return

    use_line:
    f=files
    while peekb m<>13
	{
	b=peekb m:m++
	if (b=' ') or (b=9) then goto zero_it
	pokeb f,b:f++:if f>files_end then goto zero_it
	}
    zero_it:
    if not searchb f-files from files for '.' then
	{
	pokeb f,'*'
	pokeb f+1,'.'
	pokeb f+2,'*'
	f+=3
	}
    pokeb f,0
    }

proc get_files
    {
    ds=allocate 4096
    count=0:m=0
    reg dx=files,cx=16h:dos 4eh
    if carry then return
    goto fentry

    forever
	{
	#errors off
	find next
	fentry:
	if error then return
	#errors on

	os=dta segment:oo=dta offset
	moveb 13 from os|oo+1eh to ds|m
	attr=os[oo+15h]b
	ds[m+13]b=attr
	#long
	if ((attr or 33) and 49)=33 then
	    {
	    fsize=peek os|(oo+1ah)
	    tsize+=fsize
	    ds[m+14]=low fsize
	    ds[m+16]=high fsize
	    ds[m+18]=os[oo+16h]
	    ds[m+20]=os[oo+18h]
	    }
	#short
	m+=dlen:count++
	}
    }

proc sort_files
    {
    if not sort(ds,0,dlen,count) then abort
    }

proc show(attr,all)
    {
    m=0:any=0:two=0
    #long
    if count then repeat count
	{
	if ((ds[m+13]b or 33) and 49)=attr then ;Set archive bit.
	    {
	    f=m:any++
	    len=0
	    while ds[f]b print bios chr ds[f];:f++:len++
	    if len<12 then repeat 12-len print bios " ";
	    if all then
		{
		n32=(peek ds|(m+14)+1023)/1024
		nn=low n32
		nd=digits nn
		if nd<>5 then repeat 5-nd print bios " ";
		print bios nn;"k";
		two++

		#short
		if two=4 then
		    {
		    two=0
		    if keypressed or (waiting<>0) then
			{
			if key=27 then abort
			wait for keypressed
			k=key:if k=27 then abort
			if k=13 then waiting=0 else waiting=1
			}
		    print bios
		    }
		else print bios "  ";

		#long
		}
	    else print bios "    ";
	    }
	m+=dlen
	}
    #short
    if all then
	{
	if two then print bios
	if not any then print bios "(none)"
	}
    }

proc display_files
    {
    print bios "Directories:    ";
    show(49,0)
    print bios
    print bios "Files:"
    show(33,1)

    tsize=(tsize+1023)/1024

    drive=0
    if peekb(files+1)=':' then drive=1+(ucase peekb files)-'A'
    reg dx=drive:dos 36
    sectors_cluster=reg ax
    if carry then abort
    free_clusters=reg bx
    bytes_sector=reg cx
    total_clusters=reg dx
    freek=sectors_cluster*bytes_sector*free_clusters
    freek=(freek+1023)/1024
    print bios "  ";any;" files (";
    print_large(.tsize)
    print bios "), ";
    print_large(.freek)
    print bios " free."
    }

;== start =====================================================================

get_file_spec
get_files
sort_files
display_files
stop

;== data ======================================================================

files:
fname '*.*'
space 61
files_end:
