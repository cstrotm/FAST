;===========================================================================;
;									    ;
;			 PETER CAMPBELL SOFTWARE			    ;
;									    ;
;===========================================================================;
; A demonstration program for FAST, it's about time I did something to show ;
; some of the things that FAST is capable of.				    ;
;===========================================================================;

#window memory 10000
#short
store ? 16
unsigned m

proc clear_screen
    {
    if mono then screen 7
    if not mono and (screen<>3) then screen 3
    cls
    cursor 23,0
    }

on error
    {
    clear_screen
    cursor 0,0
    print dos "DEMO has aborted, error encountered: ";
    error msg "\dos.err"
    print dos "!"
    stop
    }

proc abort
    {
    clear_screen
    locate 0,0
    print "DEMO ended!"
    print
    print "How about looking at some FAST source code?"
    print
    print "To edit (or simply view) the demo source code then type"
    print "WT DEMO"
    print
    print "WT is written in FAST and will show you a practical example of FAST"
    print "while simultaneously showing you the DEMO.F source file"
    print "(WT is a text editor written in FAST)."

    loctocur
    stop
    }

proc push_any_key
    {
    open window wpush
    wait for keypressed
    if scan=1 then abort
    close window
    }

fseg=allocate 4096

;== INTRO ==================================================================

clear_screen
open window wpc_fast

;== FAST ===================================================================

m=afast
for y=15 to 21
    locate y,0
    m=printm m,80
next y

delay=0
while delay<500
    {
    repeat 580-(delay)
	{
	l=15*160
	repeat 7
	    {
	    c=video[l]
	    move 79 from video|l+2 to video|l
	    video[l+158]=c
	    l+=160
	    }
	for d=0 to delay:next d
	}
    delay+=100
    }

push_any_key

repeat 12
    {
    scroll down 0,15,79,24,1
    repeat 12000 {}
    }

;== WINDOW SPLAT ===========================================================

c=100
while c
    {
    x=rnd mod 70
    y=rnd mod 20
    pokeb wrnd+2,x:pokeb wrnd+4,x+9
    pokeb wrnd+3,y:pokeb wrnd+5,y+5
    pokeb wrnd+6,(rnd mod 3)+5
    open window wrnd
    c--
    if c>=18 then
	{
	repeat 6000 {}
	close window
	}
    }

repeat 50000 {}

repeat 10
    {
    open window wall
    repeat 20000 {}
    close window
    }

repeat 60
    {
    open window wall
    close window
    }

push_any_key

;== SCROLL SCREEN ==========================================================

repeat 60
    {
    r=rnd mod 5
    #long
    if r=0 then
	{
	repeat 12
	    {
	    move 2000 from video|0 to video|320
	    move 160 from video|4000 to video|0
	    }
	move 2000 from video|0 to video|160
	move 80 from video|4000 to video|0
	}
    if r=1 then
	{
	repeat 12
	    {
	    move 160 from video|0 to video|4000
	    move 2000 from video|320 to video|0
	    move 160 from video|4000 to video|4000-320
	    }
	move 80 from video|0 to video|4000
	move 2000 from video|160 to video|0
	move 80 from video|4000 to video|4000-160
	}
    if r=2 then
	{
	repeat 10
	    {
	    for y2=0 to 3840 step 160
		move 8 from video|y2+144 to store
		move 72 from video|y2 to video|y2+16
		move 8 from store to video|y2
	    next y2
	    }
	}
    if r=3 then
	{
	repeat 10
	    {
	    for y3=0 to 3840 step 160
		move 8 from video|y3 to store
		move 72 from video|y3+16 to video|y3
		move 8 from store to video|y3+144
	    next y3
	    }
	}
    if r=4 then
	{
	repeat 10
	    {
	    for y4=0 to 3840 step 320
		move 8 from video|y4 to store
		move 72 from video|y4+16 to video|y4
		move 8 from store to video|y4+144

		y4+=160
		move 8 from video|y4+144 to store
		move 72 from video|y4 to video|y4+16
		move 8 from store to video|y4
		y4-=160
	    next y4
	    }
	}
    #short
    }

repeat 5 repeat 20000 {}
repeat 18 close window
push_any_key

;== HEX, DECIMAL DUMP ======================================================

proc dump(hd)
    {
    c=10000:y=11
    while c<=16000
	{
	locate y,0
	if hd=0 then repeat 16 printh c;" ";:c++
	else
	    {
	    n=locpos+148
	    while locpos<n
		{
		print c;" ";:c++
		}
	    }
	y++:if y>24 then scroll 0,11,79,24,1:y=24
	}
    }

dump(0)
dump(1)
push_any_key

;===========================================================================

fill 32768 from fseg|0 with 1a1ah
load "wt.f",fseg|0
m=0
close window

proc display_page
    {
    for y=0 to 24
	locate y,0
	m=printm fseg|m,80
    next y
    }

display_page
repeat 4 repeat 50000 {}
while fseg[m]b<>1ah display_page

fill 80*7 from video|2080 with 7820h
colour 120
locate 16,10:print "THAT WAS A 40K SOURCE FILE (WT.F) PAGE DOWN ON YOUR SCREEN"
colour 7
push_any_key

;===========================================================================

abort

;== DEMO DATA ==============================================================

wpc_fast:
datab 0,0,1,1,78,10,120
datab 22,27,2,'PETER CAMPBELL SOFTWARE'
datab 22,11,3,'FAST: The new PC language to make your computer perform!'
datab 22,11,5,'This demonstration shows FAST at work on the screen.'
datab 22,4,6,'Programs look fast if what appears on the screen is quick and smooth.'
datab 22,3,7,'FAST is fastest on screen and on internal IF-THEN-ELSE and numbers etc.'
datab 26

afast:
fname 'лллллллл    лллллл     ллллллл	ллллллл'
fname 'л	  л	 л   л		   л'
fname 'л	  л	 л   л		   л'
fname 'ллллл	  лллллллл    лллллл	   л'
fname 'л	  л	 л	    л	   л'
fname 'л	  л	 л	    л	   л'
fname 'л	  л	 л   ллллллл	   л'

wpush:
datab 0,0,62,19,79,24,7
datab 22,2,2,'Push any key...'
datab 22,3,3,'(esc aborts.)'
datab 26

wrnd:
datab 0,0,0,0,0,0,15
datab 22,2,2,'Window.'
datab 26

wall:
datab 0,0,0,0,79,24,15
datab 22,31,12,'Full Screen Window'
datab 26
