
;===========================================================================
;	Program to display files in Alphabetical Order - VIEW/DELETE
;===========================================================================

#short
work ? 2000
filem ? 16*1000+16
filee=filem+16000

unsigned m
var32 fsize
var files,totalk

#include fsort.fi

on error
    {
    cursor 24,0
    print bios
    if error<>999 then error msg "\dos.err":print bios "!"
    stop
    }

proc display(dy)
    {
    y=(dy/23)*23
    m=filem+y*16
    for d=2 to 24
	colour 15:if y=dy then colour 120:cursor d,0
	locate d,0
	if y>=files then
	    {
	    fill 40 from video|locpos with 0720h
	    goto nextd
	    }
	if peekb (m+13) then print "û "; else print "  ";
	x=printm m,20
	locate d,20
	nn=peek (m+14)
	nd=digits nn
	if nd<>5 then repeat 5-nd print " ";
	print nn;"k  ";
	m+=16
	y++
	nextd:
    next d
    colour 7
    locate 3,50:print files;" Files, ";totalk;"K    ";
    locate 6,50:print "Enter=Select/Unselect File";
    locate 7,50:print "F1 Select ALL Files";
    locate 8,50:print "F2 Unselect ALL Files";
    locate 10,50:print "V=View Highlighted File";
    locate 11,50:print "D=Delete Selected File(s)";
    }

proc setf(sf)
    {
    m=filem
    repeat files
	{
	pokeb m+13,sf
	m+=16
	}
    }

function confirm
    {
    open window wconfirm
    cursor 17,62
    wait for keypressed
    close window
    if (lcase key)<>'y' then return 0
    return 1

    wconfirm:
    datab 0,0,40,15,67,19,15
    datab 22,2,2,'Press Y to confirm [ ]'
    datab 26
    }

proc delete_selected
    {
    open window wdelete
    dsm=filem:dsy=0
    wy=13

    repeat files
	{
	#long
	if peekb (dsm+13) then
	    {
	    #short
	    display(dsy)
	    #errors off
	    wm=dsm:locate wy,42
	    while peekb wm print chr peekb wm;:wm++
	    delete dsm
	    #errors on
	    if error then
		{
		print " ";
		loctocur
		error msg "\dos.err"
		print bios "!";
		deleted=0
		}
	    else
		{
		print " deleted ok.";
		totalk-=peek (dsm+14)
		files--
		moveb filee-dsm from dsm+16 to dsm
		deleted=1
		}
	    wy++
	    if wy>23 then
		{
		scroll 41,13,78,23,1
		wy=23
		}
	    if deleted then goto fdeleted
	    }
	dsm+=16
	dsy++
	fdeleted:
	}

    locate wy,42:print "Press any key...";
    loctocur
    wait for keyscan
    close window
    return

    wdelete:
    datab 0,0,40,12,79,24,7,26
    }

proc clear_bot scroll 0,2,79,24,0

proc view(m)
    {
    #errors off
    open #1,m
    if error then beep:return
    r=work+read #1,2000 to work
    close #1
    #errors on

    clear_bot
    y=2:m=work
    while (y<25) and (m<r)
	{
	locate y,0
	m=printm m,80
	if peekb m=26 then m++	;skip eof
	y++
	}
    wait for keyscan
    clear_bot
    }

;== Start ==================================================================

print bios "PURGE: File Viewer and Deleter. (c) Peter Campbell 1990" cr lf

nm=81h
while peekb nm<>13
    {
    if peekb nm>' ' then
	{
	move 10 from nm to fscan
	m=searchb 20 from fscan for 13
	if m then pokeb m,0
	goto scanning
	}
    nm++
    }

scanning:
m=fscan
print bios "Scanning Files... ";
while peekb m print bios chr peekb m;:m++

m=filem:files=0:totalk=0
find first fscan:goto entry

while files<1000
    {
    #errors off
    find next
    #errors on
    entry:
    if error then goto purge

    moveb 13 from dta segment|dta offset+30 to m
    fsize=peek dta segment|(dta offset+1ah)
    fsize=(fsize+1023)/1024
    pokeb m+13,0    ;not selected
    poke m+14,low fsize
    totalk+=low fsize

    m+=16
    files++
    }

purge:
x=sort(reg cs,filem,16,files)
cls:locate 0,0
colour 15
print "PURGE: File Viewer and Deleter. (c) Peter Campbell 1990."
colour 7
repeat 80 print "Ä";

fy=0

forever
    {
    display(fy)
    if files=0 then error 18
    wait for keypressed
    m=filem+fy*16
    ks=keyscan:k=lcase low ks:s=high ks
    if s=1 then error 999

    if k=13 then pokeb m+13,not peekb (m+13):fy++
    if k='d' then if confirm then delete_selected
    if k='v' then view(m)

    if s=72 then fy--
    if s=80 then fy++
    if s=73 then fy-=23
    if s=81 then fy+=23
    if (s=71) or (fy<0) then fy=0
    if (s=79) or (fy>=files) then fy=files-1
    if ks=15104 then setf(1)
    if ks=15360 then setf(0)
    }

fscan: fname '*.*'
space 20
