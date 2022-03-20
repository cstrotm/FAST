;HEXD program in FAST.
#window memory 4500

ax=0:ay=0
number=0

proc printbs(n)
    {
    a=digits(low n)
    if a<3 then repeat 3-a print " ";
    printb n;
    }

proc colours
    {
    open window ccc
    for x=0 to 15
    c=x+'0':if c>'9' then c+=7
    colour 15
    locate 3,x*2+26:print chr c;
    if x<8 then locate x*2+5,22:print chr c;
    for y=0 to 7
    locate y*2+5,x*2+26
    colour x+y*16:print chr 4;
    next y,x
    wait for keyscan
    close window
    }

proc ascii_display
    {
    open window display
    for a=0 to 31
    for b=0 to 7
    locate b*2+3,a*2+8:print chr b*32+a
    next b,a

    forever
	{
	n=ay*32+ax
	colour 15
	locate 20,10:print "Character: ";n;"  (Hex=";
	printhb n;")  ";
	x=ax*2+7:y=ay*2+2
	locate y,x:print "зд©";
	locate y+1,x:print "Ё";
	locate y+1,x+2:print "Ё";
	locate y+2,x:print "юды";
	wait for keypressed:s=scan
	if s=1 then close window:return
	locate y,x:print "   ";
	locate y+1,x:print " ";
	locate y+1,x+2:print " ";
	locate y+2,x:print "   ";

	if s=75 then ax--
	if s=77 then ax++
	if s=72 then ay--
	if s=80 then ay++

	if s=71 then ax--:ay--
	if s=73 then ax++:ay--
	if s=79 then ay++:ax--
	if s=81 then ay++:ax++

	if ax<0 then ax=31
	if ax>31 then ax=0
	if ay<0 then ay=7
	if ay>7 then ay=0
	}
    }

proc display_keys
    {
    open window menu_key
    colour 23
    wait_key:
    locate 18,32:printbs(peek 0|417h);
    locate 18,44:printbs(peek 0|418h);
    if not keypressed then goto wait_key
    ks=keyscan
    if ks=283 then close window:return
    locate 13,35:printbs(low ks);
    locate 13,47:printbs(high ks);
    locate 15,32:print "KEYSCAN = ";ks;"    ";
    goto wait_key
    }

proc idecimal
    {
    open window iii
    cursor 19,6:number=input
    close window
    }

proc ihex
    {
    open window iii
    cursor 19,6:number=inputh
    close window
    }

proc numeric_display
    {
    open window numdisp
    colour 60h
    num_wait:
    for r=0 to 15:x=number+r
    locate r+4,26
    repeat 6-digits(x) print " ";
    print x;" ";
    printh x;" ";
    bit=32768
    for k=0 to 15
    if x and bit then print "1"; else print "0";
    bit/=2
    next k:next r
    k=scan
    if k=81 then number+=16
    if k=73 then number-=16
    if k=1 then cursor 25,0:close window:return
    if k=32 then idecimal
    if k=35 then ihex
    goto num_wait
    }

on int 1
    {
    if (peek 0|417h and 3)=3 then
	{
	position=curpos
	opt=menu main
	goto mm2
	forever
	    {
	    opt=select main,opt
	    mm2:
	    colour 7
	    if not opt then goto finish
	    if opt=5 then stop int 1:goto finish

	    if opt=1 then ascii_display
	    if opt=2 then numeric_display
	    if opt=3 then display_keys
	    if opt=4 then colours
	    }
	finish:
	close window
	curpos=position
	}
    }
print bios cr lf "To activate HEXD press both shift keys."
stop resident

;-- DATA ----------------------------------------------------------------------
main:
datab 1,5,5,3,30,11,15
datab 22,6,1,'HEXD'
datab 22,2,3,'Ascii table'
datab 22,2,4,'Number conversion'
datab 22,2,5,'Keyboard codes'
datab 22,2,6,'Colours'
datab 22,2,7,'Disable HEXD'
datab 26

display:
datab 0,0,6,1,72,22,7
datab 26

menu_key:
datab 1,0,26,9,52,20,23
datab 22,1,1,'Push a key (ESC to exit).'
datab 22,3,4,'KEY     0   SCAN	  0'
datab 22,3,8,'0040:0017   0040:0018'
datab 26

numdisp:
datab 1,0,25,1,55,21,60h
datab 22,3,1,'DEC   HEX      BINARY'
datab 22,1,19,20,7,'	 Input:  ',20,15,'D',20,7,'ecimal '
datab 20,15,'H',20,7,'ex     '
datab 20,1100000b,26

iii:
datab 0,0,3,15,12,21,1010000b
datab 22,2,2,'Number'
datab 22,2,4,'>',26

ccc:
datab 0,0,20,2,59,21,7,26
