
;== FAST, SOFA & FFD Setup Utility for Public Domain =======================

files ? 13*100
var bs

#short
if mono and (screen<>7) then screen 7
if not mono and (screen<>3) then screen 3

m=81h
drive='C'
while peekb m<>13
    {
    if peekb m=':' then drive=ucase peekb (m-1)
    m++
    }
#long
if (drive<'A') or (drive>'Z') then
    {
    print bios "To use the FSET utility enter 'FSET d:' d is the desired drive letter"
    print bios "Type just FSET for the default drive C"
    beep
    stop
    }
#short
for a=data1 to data2-1
if peekb a='?' then pokeb a,drive
next a
drive-='A'

on error
    {
    pop ath
    cursor 19,0
    error msg "\dos.err":printh bios "! Address=";ath
    beep:stop
    }
on break error 999

function yesno
    {
    forever
	{
	kk=lcase key
	if kk='y' then print bios "yes":return 1
	if kk='n' then print bios "no":return 0
	}
    }

proc header
    {
    cls:locate 0,0
    repeat 80 print chr 205;
    locate 1,12
    print "Setup Utility 1.1 - (C) PETER CAMPBELL SOFTWARE May 1989."
    repeat 80 print chr 205;
    print cr
    }

proc setup(sa)
    {
    header
    print "Do you wish to install ";
    prints sa,0
    sa=1+searchb 1000 from sa for 0
    print "? (y/n) ";:loctocur
    print cr cr

    push sa,locpos
    print "Create Directory: ";:sa=printm sa,20:print cr
    while peekb sa
	{
	print "Copy file(s) ";:sa=printm sa,15:locpos+=30:print "-> ";
	sa=printm sa,20:print
	}
    pop locpos,sa

    #long
    if yesno then
	{
    #short
	colour 15
	print "Create Directory: ";:na=printm sa,20:print cr
	#errors off
	make dir sa:sa=na
	#errors on
	if error then if error<>5 then error

	while peekb sa
	    {
	    print "Copy file(s) ";:f1=printm sa,15:locpos+=30:print "-> ";
	    f2=printm f1,20:print
	    moveb 10 from sa to source+2
	    moveb 10 from f1 to dest
	    sa=f2:f2=searchb 15 from dest for 0

	    flm=files:flc=0
	    find first source:goto fentry
	    forever
		{
		#errors off
		find next
		#errors on
		fentry:
		if error then goto fstart
		moveb 13 from dta segment|dta offset+30 to flm
		flm+=13:flc++
		}

	    fstart:
	    flm=files
	    repeat flc
		{
		moveb 13 from flm to source+2
		moveb 13 from flm to f2
		open #1,source:create #2,dest
		rlen=1
		while rlen
		    {
		    rlen=read #1,65530 to bs|0
		    write #2,rlen from bs|0
		    }
		close #1,#2
		flm+=13
		}
	    }

	colour 7
	}

    }

;== Start ==================================================================

bs=allocate 4096    ;Segment for buffer.
pokeb source,cdisk+'A'

setup(s_fast)
setup(s_sofa)
setup(s_ffd)

header
print "Now PRINT the file PCS for news, registration and upgrade details." cr
loctocur
stop

;== Data ===================================================================

data1:

s_fast:
datab 'FAST, the compiler for a completely new language 	specifically designed for the PC',0
datab '?:\FAST',0
datab 'SFAST.BAT' ,0,'?:\',0
datab 'UNFAST.EXE',0,'?:\FAST\',0
datab '*.ERR'	  ,0,'?:\',0
datab '*.HLP'	  ,0,'?:\',0
datab 0

s_sofa:
datab 'SOFA, a 1000 lps assembler for the PC (CHASM 4.0 compat.)',0
datab '?:\ASM',0
datab 'SSOFA.BAT' ,0,'?:\',0
datab 'UNSOFA.EXE',0,'?:\ASM\',0
datab 0

s_ffd:
datab 'FFD, an EASY TO USE FREE FLOW DATABASE with complete helpfor FAST as a demo'
datab ' database',0
datab '?:\FFD',0
datab 'SFFD.BAT' ,0,'?:\',0
datab 'UNFFD.EXE',0,'?:\FFD\',0
datab '*.ERR'	 ,0,'?:\',0
datab 0

data2:

source:
datab 'A:'
space 20

dest:
space 30
