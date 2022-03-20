
;== MUSIC ==================================================================

#short
const blen=40*16
bars ? blen+16
bend=bars+blen

sharps ? 24

tabl   ? 80*7

tempo=2000

on error
    {
    print bios:error msg "\dos.err":print bios "!"
    stop
    }

proc print_bars(bar)
    {
    colour 120
    locate 2,11:print bar+1;"  ";
    locate 2,30:print bar+2;"  ";
    locate 2,49:print bar+3;"  ";
    locate 2,68:print bar+4;"  ";
    colour 7
    fill 1760 from video|480 with 0720h     ;clear bottom of screen
    l=10*160
    repeat 5
	{
	fill 80 from video|l with 07c4h     ;white Ä's
	l2=l
	repeat 3
	    {
	    video[l2+42]b=197
	    video[l2-118]b=179
	    l2+=38
	    }
	l+=320
	}
    l2=l-118
    repeat 3 video[l2]b=179:l2+=38

    m=bars+bar*16
    x=4
    repeat 4
	{
	x2=x+16
	repeat 16
	    {
	    by=peekb m:m++
	    r=by and 31
	    if r then
		{
		colour 7
		bn=peekb (note+r)
		if by and 128 then n+=32    ;lowercase = flat
		if by and 64 then colour 15 ;bright    = sharp
		locate 2+r,x:print chr bn;
		colour 7
		}
	    if by and 32 then	;repeat.
		{
		locate 13,x2:print "ú";
		locate 15,x2:print "ú";
		}
	    else if (by and 192)=192 then   ;intro.
		{
		locate 13,x-1:print "i";
		}
	    x++
	    }
	x+=3
	}
    }

proc put_note(py,px)
    {
    r=py and 31
    px=(px/16)*19+4+(px and 15)
    if r then
	{
	colour 7
	n=peekb (note+r)
	if py and 128 then n+=32    ;lowercase = flat
	if py and 64 then colour 15 ;bright    = sharp
	locate 2+r,px:print chr n;
	colour 7
	}
    cursor 2+r,px
    }

function music_end
    {
    me=0
    n=0
    while n<blen
	{
	if peekb (bars+n) then me=n or 15   ;to end of bar.
	n++
	}
    return me
    }

function set_sharps
    {
    shp=sharps
    fillb 24 from sharps with 0
    if (peekb bars and 192)<>192 then return 0

    n=bars+1
    repeat 15
	{
	sy=peekb n:n++
	if sy and 64 then
	    {
	    y=sy and 31
	    pokeb shp,y
	    pokeb shp+1,y+7
	    pokeb shp+2,y+14
	    shp+=3
	    }
	}
    return shp-sharps
    }

proc play_music
    {
    play_it_again:
    last_note=music_end
    shp=set_sharps

    n=0:bar=0:bx=0
    reps=0:rbar=0:rbx=0
    intro_bar=0
    while n<=last_note
	{
	sy=peekb (bars+n):n++
	y=sy and 31
	bx++:if bx>31 then bx=16:bar++:intro_bar=0
	d=5
	#long
	if y then
	    {
	    #short
	    freq=peek (freq_table+y*4)
	    print_bars(bar)
	    px=(bx/16)*19+4+(bx and 15)
	    locate 2+y,px
	    video[locpos-1]b=120    ;highlight note.
	    if shp then
		{
		if searchb shp from sharps for y then
		    {
		    freq=peek ((freq_table-2)+y*4)  ;sharp!
		    }
		}
	    if (intro_bar<>0) and ((sy and 64)=64) then goto dont_play
	    noise off:repeat 200 {}
	    noise 1,freq/2
	    d=4
	    }
	dont_play:
	if sy and 32 then
	    {
	    if reps<>n then
		{
		swap reps,n
		swap rbar,bar
		swap rbx,bx
		}
	    }
	if (sy and 192)=192 then
	    {
	    intro_bar=1
	    reps=n+15
	    rbar=bar+1
	    rbx=bx-1
	    }
	ignore_key:
	repeat d repeat tempo {}    ;delay.
	if scan=1 then noise off:return     ;escape?
	}
    noise off
    repeat 5 repeat 10000 {}
    goto play_it_again
    }

proc print_tab
    {
    lprint "MUSIC (C) Peter Campbell"
    lprint "Music File: ";
    m=mname+2
    while peekb m lprint chr ucase peekb m;:m++
    lprint cr lf

    last_note=music_end or 63	    ;print even bars
    shp=set_sharps
    n=0

    line4:
    bar=0
    bx=0
    xx=0
    fillb 80   from tabl    with ' '
    m=tabl+80
    repeat 6*4
	{
	fillb 16 from m with 'Ä'
	poke m+16,2020h
	poke m+18,2020h
	m+=20
	}

    while n<=last_note
	{
	sy=peekb (bars+n):n++
	y=sy and 31
	#long
	if y then
	    {
	    #short
	    pokeb tabl+bx,peekb (note+y)    ;actual note.

	    strg=peekb (gstring+y*2)
	    fret=peekb (gstring+1+y*2)
	    if shp then
		{
		if searchb shp from sharps for y then
		    {
		    strg=peekb (gstring+2+y*2)
		    fret=peekb (gstring+3+y*2)
		    }
		}
	    tm=tabl+strg*80+bx
	    pokeb tm,fret+'0'
	    }
	if sy and 32 then
	    {
	    pokeb tabl+1+3*80+bx,'.'
	    pokeb tabl+1+4*80+bx,'.'
	    }
	bx++:xx++
	if xx>15 then
	    {
	    xx=0
	    bx+=4:bar++
	    if bar>3 then
		{
		m=tabl
		if scan=1 then	    ;cancel printing.
		    {
		    lprint chr 24;
		    return
		    }
		repeat 80*7 lprint chr peekb m;:m++
		lprint cr lf
		goto line4
		}
	    }
	}
    }

;== Main Program ===========================================================

#inpend=0
print bios "Music file name? ";
inputs mname
print bios
if peekb (mname+2)=0 then error 999
x=searchb 20 from mname+2 for 0
moveb 3 from ext to x

fillb blen+16 from bars with 0
#errors off
load mname+2,bars,blen
#errors on
if error then
    {
    if error<>2 then error
    print bios "Unknown file, create? ";
    wait for keypressed
    k=lcase key
    if k<>'y' then error 999
    }

cls
locate 0,0:print "MUSIC PROGRAM (C) PETER CAMPBELL 1990"
print "Current Music File: ";:prints mname+2,0
fill 80 from video|320 with 7820h

bar=0
bx=0
nstep=2     ;16/nstep

forever
    {
    locate 0,60:print "Tempo=";4000-tempo;" ";
    print_bars(bar)
    nm=bars+bar*16+bx
    oy=peekb nm:ny=oy and 31
    put_note(oy,bx)
    wait for keypressed
    ks=keyscan:s=high ks:k=lcase low ks
    if s=1 then
	{
	cursor 21,0
	print bios "Saving file...";
	save mname+2,bars,blen
	print bios " ok"
	stop
	}

    if k='-' then tempo+=100:if tempo>3900 then tempo=3900
    if k='+' then tempo-=100:if tempo<500 then tempo=500

    if s=72 then ny--
    if s=80 then ny++
    if s=73 then ny=0
    if s=81 then ny=22
    if ny<0 then ny=0
    if ny>22 then ny=22
    if (k>='a') and (k<='g') then
	{
	if ny=0 then ny=6
	again:
	x=searchb 8 from note+ny+1 for ucase k
	if x=0 then ny=0:goto again
	ny=x-note
	}
    if k='s' then oy=oy xor 64	;s=sharp
    if k='t' then oy=oy xor 128 ;t=flat temp!
    if ks=21248 then oy=0:ny=0	;remove note
    pokeb nm,(oy and 224)+ny

    if k='p' then play_music
    if ks=4864 then
	{
	m=bars+bar*16+(bx or 15)
	pokeb m,peekb m xor 32	    ;toggle repeat bit.
	}
    if ks=5888 then
	{
	m=bars+bar*16+(bx and 48)
	pokeb m,peekb m xor 192     ;toggle intro bits.
	}

    if s=75 then bx-=nstep
    if s=77 then bx+=nstep
    if s=71 then bx=bx and 48
    if s=79 then bx=bx or 15
    if ks=29696 then bx+=16
    if ks=29440 then bx-=16
    if ks=30464 then bar=0:bx=0
    if ks=29952 then bar=36:bx=63
    if ks=8192 then
	{
	m=bars+bar*16
	moveb bend-m from m+16 to m
	}
    if ks=11776 then
	{
	m=bars+bar*16
	moveb bend-m from m to m+16
	}

    if ks=8704 then print_tab

    if bx>63 then bx=63:if bar<36 then bx=48:bar++
    if bx<0  then bx=0:if bar then bx=15:bar--
    if bar<0 then bar=0
    if bar>36 then bar=36
    }

;== Data ===================================================================

mname:	string 20
ext:	fname '.m'

note:	datab ' FEDCBAGFEDCBAGFEDCBAGF'

;== Music Note Data ========================================================

;old notes table
;4832b 4561c 4305c# 4063d 3855d# 3620e 3417f 3225f# 3044g 2873g# 2712a
;2560a# 2416b 2280c 2152c# 2032d 1918d# 1810e 1708f 1612f# 1522g 1437g# 1356a

freq_table:
data 0
data 0
data 1708	;f
data 1810	;***
data 1810	;e
data 1918	;d#
data 2032	;d
data 2152	;c#
data 2280	;c
data 2416	;***
data 2416	;b
data 2560	;a#
data 2712	;a
data 2873	;g#
data 3044	;g
data 3225	;f#
data 3417	;f
data 3620	;***
data 3620	;e
data 3855	;d#
data 4063	;d
data 4305	;c#
data 4561	;c
data 4832	;***
data 4832	;b
data 5120	;a#
data 5424	;a
data 5746	;g
data 6088	;g
data 6450	;f#
data 6834	;f
data 7240	;***
data 7240	;e
data 7710	;#
data 8126	;d
data 8610	;c#
data 9122	;c
data 9664	;***
data 9664	;b
data 10240	;a#
data 10848	;a
data 11492	;g#
data 12196	;g
data 12900	;f#
data 13668	;f

gstring:
datab 0,0
datab 1,13  ;f
datab 1,12  ;e
datab 1,10  ;d
datab 1,8   ;c
datab 1,7   ;b
datab 1,5   ;a
datab 2,8   ;g	     2,9
datab 2,6   ;f	     2,7
datab 2,5   ;e	     3,9
datab 3,7   ;d	     3,8
datab 3,5   ;c	     3,6
datab 3,4   ;b	     4,9
datab 4,7   ;a	     4,8
datab 4,5   ;g	     4,6
datab 5,8   ;f	     5,9
datab 5,7   ;e	     5,7
datab 5,5   ;d	     5,6
datab 6,8   ;c	     6,9
datab 6,7   ;b	     6,7
datab 6,5   ;a
datab 6,3   ;g
datab 6,1   ;f
