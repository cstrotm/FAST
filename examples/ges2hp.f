;==============================================================================
;
;		       Gestetner Laser Control Codes
;			 to HP Laser Jet Series II.
;
;==============================================================================

var n1,n2,n3,pn,putlead,om
const esc=27
lcl='|'

in_buffer  ? 100
pre_buffer ? 100
out_buffer ? 100

print bios
print bios "GES2HP - Gestetner to HP Laser Jet Conversion - v1.30 2nd Nov 1990"
print bios
print bios "Ideas:   QEDSME, 21 Treatts rd, Lindfield NSW 2070, Australia."
print bios "Program: Peter Campbell Software, PO box 54-180 Mana, New Zealand."
print bios "SALES:   Try This!, PO Box 363, Mona Vale NSW, Australia 2103."
print bios
print bios "GES2HP is ON, the commands |ON. and |OFF. turn GES2HP conversions on or off."

on error
    {
    print bios
    error msg "\dos.err"
    print "!"
    stop
    }

ignores=ignore2:ignores-=ignore1:ignores/=2
t=table:items=(table_end-t)/10

place=0

dos 35(17):poke pioff,reg bx:poke piseg,reg es
setint 17 to gesp

emulator=1  ;see |ON. and |OFF.

stop resident

;== Some procs =============================================================

proc send(sstart,ssend)
    {
    while sstart<ssend
	{
	c=peekb sstart:sstart++
	reg dx=0,ax=c
	inline 9ch
	inline 0ffh,1eh
	data pioff
	}
    }

proc putone(pod)
    {
    l=pn/pod
    if l then pokeb om,l+'0':om++:pn=pn mod pod:putlead=0
    else if l=putlead then pokeb om,'0':om++
    }

proc putnum(pn)
    {
    if pn<0 then pokeb om,'-':om++:pn=0-pn
    putlead=' '
    putone(10000)
    putone(1000)
    putone(100)
    putone(10)
    putlead=0
    putone(1)
    }

;== GES control code trapping ==============================================

gesp:
pushall
inline 50h
reg ds=reg cs
pop cmd

if high cmd then goto old
c=low cmd

if (c<>lcl) and (place=0) then goto old

if place=0 then place=in_buffer
    else if c=lcl then goto donet	;ignore || - means | (ASDF.GES) ?

pokeb place,ucase c:place++
if place>=(in_buffer+100) then goto outted

if c='.' then
    {
    n1=0:n2=0:n3=0
    m=in_buffer+1 ;skip lcl pipe symbol.

    fc=search ignores from ignore1 for peek m
    if fc then
	{
	if emulator=0 then goto emulate_off
	goto outted
	}

    fc=findbin(m,reg cs,table,items,10,place-m-1)
    if fc then
	{
	fc=peek ((fc-1)*10+table+8)
	if fc=0 then emulator=0:goto outted
	if fc=1 then emulator=1:goto outted
	if emulator=0 then goto emulate_off
	mend=searchb 100 from fc for 0
	send(fc,mend)
	goto outted
	}

    if emulator=0 then
	{
	emulate_off:
	send(in_buffer,place)
	goto outted
	}

    fill 50 from pre_buffer with 2020h
    dig=0:diggers=0:digger=0
    nx=pre_buffer
    while m<place
	{
	c=peekb m:m++
	if (c>='0') and (c<='9') then
	    {
	    digger=digger*10+c-'0'
	    dig=1
	    }
	else
	    {
	    if dig then
		{
		c='#':dig=0
		if diggers=0 then n1=digger
		if diggers=1 then n2=digger
		if diggers=2 then n3=digger
		digger=0:diggers++
		}
	    if c<>'.' then pokeb nx,ucase c:nx++
	    }
	}
    if dig then
	{
	if diggers=0 then n1=digger
	if diggers=1 then n2=digger
	if diggers=2 then n3=digger
	}
    fc=findbin(pre_buffer,reg cs,table,items,10,8)
    if fc then
	{
	fill 50 from out_buffer with 2020h
	fc=peek ((fc-1)*10+table+8)
	diggers=0
	digger=n1
	om=out_buffer
	while peekb fc
	    {
	    c=peekb fc:fc++
	    if c='#' then
		{
		putnum(digger)
		nextnum:
		diggers++
		if diggers=1 then digger=n2
		if diggers=2 then digger=n3
		}
	    else if c='$' then
		{
		putnum(digger*72)
		goto nextnum
		}
	    else if c='^' then
		{
		putnum(digger*7)
		goto nextnum
		}
	    else pokeb om,c:om++
	    }
	send(out_buffer,om)
	}
    else
	{
	print bios "  GES2HP: Unknown ";
	m=in_buffer
	while m<place print bios chr peekb m;:m++
	print bios
	}

    outted:
    place=0
    }

donet:
popall
iret

old:
popall
inline 0eah
pioff:
data 0
piseg:
data 0

;== Laser Printer Control Codes ===============================================

ignore1:
datab 'CM'
datab 'MS'
ignore2:

table:

datab 'BM      ':data hp_null
datab 'CF0     ':data hp_font0
datab 'CF1     ':data hp_font1
datab 'CF2     ':data hp_font2
datab 'CF3     ':data hp_font3
datab 'CF4     ':data hp_font4
datab 'CF5     ':data hp_font5
datab 'CF6     ':data hp_font6
datab 'CF7     ':data hp_font7
datab 'CF8     ':data hp_font8
datab 'CF9     ':data hp_null
datab 'CP#     ':data hp_char_pitch
datab 'CX#     ':data hp_horizontal
datab 'CY#     ':data hp_vertical
datab 'EJ      ':data hp_eject
datab 'ES#     ':data hp_null
datab 'LB###   ':data hp_line_box
datab 'LC##    ':data hp_null
datab 'LH##    ':data hp_hline
datab 'LM      ':data hp_null
datab 'LP#     ':data hp_line_spacing
datab 'LV##    ':data hp_vline
datab 'OFF     ':data 0
datab 'ON      ':data 1
datab 'PF#     ':data hp_paper_feed
datab 'PL#     ':data hp_page_len
datab 'RM      ':data hp_null
datab 'RS      ':data hp_reset
datab 'TFI##   ':data hp_null
datab 'TM      ':data hp_null
datab 'UL0     ':data hp_underlineoff
datab 'UL1     ':data hp_underlineon
datab 'VB#     ':data hp_vary_bold
datab 'VC      ':data hp_clear
datab 'VH0     ':data hp_dheight0
datab 'VH1     ':data hp_dheight1
datab 'VI0     ':data hp_slope_up
datab 'VI#     ':data hp_slope
datab 'VO#     ':data hp_null
datab 'VP#     ':data hp_null
datab 'VS0     ':data hp_shade0
datab 'VS1     ':data hp_shade1
datab 'VS2     ':data hp_shade2
datab 'VS3     ':data hp_shade3
datab 'VW0     ':data hp_wide0
datab 'VW1     ':data hp_wide1

table_end:

;== HP CODES ===============================================================

;note: #=n, $=n*72, ^=n*7

hp_null:	    datab 0
hp_reset:	    datab esc,'E',0
hp_paper_feed:	    datab 0
hp_page_len:	    datab esc,'&l#P',0
hp_char_pitch:	    datab esc,'(s#H',0
hp_font0:	    datab 0
hp_font1:	    datab esc,'(s3T',esc,'(s10V',esc,'(s12H',0
hp_font2:	    datab esc,'(s0T',esc,'(s7.2V',esc,'&k2S',0
hp_font3:	    datab esc,'(s3T',esc,'(s24V',0
hp_font4:	    datab esc,'(s3T',esc,'(s3B',0
hp_font5:	    datab esc,'(s0T',esc,'(s7.2V',esc,'&k2S',esc,'(s3B',0
hp_font6:	    datab esc,'(s3T',esc,'(s1S',0
hp_font7:	    datab esc,'(s4T',esc,'(s1P',esc,'(s14.4V',0
hp_font8:	    datab esc,'(s4T',esc,'(s24V',0
hp_underlineon:     datab esc,'&d0D',0
hp_underlineoff:    datab esc,'&d@',0
hp_eject:	    datab 12,0
hp_vary_bold:	    datab esc,'(s#B',0
hp_horizontal:	    datab 13,esc,'&a$H',0
hp_vertical:	    datab esc,'&a-7200V',esc,'&a$V',0	;logical page limit?
hp_hline:	    datab esc,'*c^V',esc,'*c$H',esc,'*c0P',0
hp_vline:	    datab esc,'*c^H',esc,'*c$V',esc,'*c0P',0
hp_line_spacing:    datab esc,'&l#D',0
hp_line_box:	    datab esc,'*c0G',esc,'*c#H'     ;ignore #H - use it up.
		    datab esc,'*c$H',esc,'*c$V',esc,'*c2P',0
hp_slope_up:	    datab esc,'(s0S',0
hp_slope:	    datab esc,'(s1S',0
hp_dheight0:	    datab esc,'(s12V',0
hp_dheight1:	    datab esc,'(s24V',0
hp_shade0:	    datab esc,'(s0B',0
hp_shade1:	    datab esc,'(s-3B',0
hp_shade2:	    datab esc,'(s-5B',0
hp_shade3:	    datab esc,'(s-7B',0
hp_clear:	    datab esc,'(3@',0
hp_wide0:	    datab esc,'(s12H',0
hp_wide1:	    datab esc,'(s6H',0
