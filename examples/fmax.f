;==============================================================================
;=		FMAX  FAST Language Help File Display/Reporter.	      =
;==============================================================================
;
;		      Written by Peter Campbell, 1988.
;

var seg
var last_page,current_page,lhelp,end_page,found_at,store
const stacks=20
stack_place=0
stack_table ? stacks*2
#inpend=0

on error
    {
    loctocur
    print bios cr lf
    error msg "\dos.err"
    print bios ", FMAX aborted." cr lf
    terminate
    }

if mono
    then if screen<>7 then screen 7
    else if screen<>3 then screen 3

cls:locate 0,4:cursor 1,0
colour 15
print " FMAX  FAST Cross-Reference File Display   Written by Peter Campbell. "
colour 120
print "F1 Help  F3 Print page  F4 Print ALL  F5 Find?  ESC Back one reference  F10 Exit"

proc abort
    {
    cursor 24,0
    print bios cr lf lf "ออ FMAX finished"
    terminate
    }

proc forward(nn)
    {
    repeat nn
	{
	forward_char:
	c=seg[current_page]b
	if c=26 then return
	current_page++
	if c<>13 then goto forward_char
	}
    }

function dec_m
    {
    current_page--
    if current_page=-1 then current_page=0:return 1
    return 0
    }

proc back(nn)
    {
    repeat 2 if dec_m then return
    repeat nn
	{
	back_char:
	c=seg[current_page]b
	if dec_m then return
	if c<>13 then goto back_char
	}
    repeat 2 current_page++
    return
    }

proc go_to
    {
    if stack_place>=stacks then
	{
	move stacks-1 from stack_table+2 to stack_table
	stack_place=stacks-1
	}
    om=stack_table+stack_place*2
    poke om,current_page
    stack_place++
    }

proc return_to
    {
    if stack_place then
	{
	stack_place--
	om=stack_table+stack_place*2
	current_page=peek om
	}
    }

proc clear_page
    {
    colour 7
    scroll 0,2,79,24,0
    locate 2,0
    }

proc display_page
    {
    if current_page above end_page then current_page=end_page
    colour 7
    if last_page=current_page then return

    m=current_page
    for y=2 to 24
	locate y,0
	c=0

	while c<>13
	    {
	    c=seg[m]b:m++
	    if c>31 then print chr c;:goto next_x
	    if c=26 then goto fill_end
	    if c=9 then
		{
		x=(locpos mod 160)/2
		repeat (x and 248)+8-x print " ";
		}
	    next_x:
	    }

	fill_line:
	fl=80-((locpos mod 160)/2)
	if fl>0 then fill fl from video|locpos with 0720h
    next y

    fill_end:
    fl=2000-(locpos/2)
    if fl>0 then fill fl from video|locpos with 0720h

    last_page=current_page
    }

proc find_string(nn)
    {
    go_to
    m=0
    forever
	{
	find_in_seg:
	if lhelp=m then goto find_not
	th=searchb lhelp-m from seg|m for 13
	if th=0 then
	    {
	    find_not:
	    beep:return_to:return
	    }
	m=th

	start_new_line:
	n=nn
	found_at=m
	while peekb n
	    {
	    next_fc:
	    c=seg[m]b:m++
	    if c=10 then goto next_fc
	    if c=13 then goto start_new_line
	    if (lcase peekb n)<>(lcase c) then goto next_find_line
	    n++
	    }
	current_page=found_at
	display_page
	y=2:store=current_page
	repeat 23
	    {
	    if current_page=found_at then
		{
		locate y,0:repeat 80 video[locpos+1]b=15:locpos+=2
		goto end_foundit
		}
	    forward(1)
	    y++
	    }
	end_foundit:
	current_page=store
	return

	next_find_line:
	found_at=-1
	}
    find_end:
    beep:return_to
    }

proc print_line(cc)
    {
    m=current_page
    repeat cc
	{
	c=0
	while c<>13
	    {
	    c=seg[m]b:m++
	    if c>31 then lprint chr c;
	    if c=26 then return
	    }
	lprint
	}
    }

proc print_page print_line(23)
proc print_all
    {
    current_page=0
    print_line(32767)
    }

proc help
    {
    clear_page
    colour 120
    print cr "FMAX HELP" cr
    colour 15
    print "Keys:" cr
    colour 7
    print "To move the view page:    Press Up, Down, Page Up, Page down, Home or End." cr
    print "To move highlighted"
    print "reference cursor:         Press Tab & BackTab (shifted tab) or Left & Right." cr
    print "To select reference:      Press ENTER or the Grey +." cr
    print "To return from reference: Press ESC or the Grey -." cr

    colour 15
    print "Notes:" cr
    colour 7
    print "FMAX remembers the last ";stacks;" reference points." cr
    print "Find? (F5) will find only keywords, ie words in the left column only." cr
    print "Print (F3 or F4) assumes the printer is ready and starts printing immediately." cr

    locate 24,26:colour 120:print "Press any key to leave help.";
    wait for keyscan
    last_page=-1
    }

;Start program from here:
seg=allocate 4096

m=81h:f=file+2
while peekb m<>13
    {
    c=peekb m
    if c>' ' then poke f,ucase c:f++ ;POKE will make f+1 = 0.
    if f>(file+40) then goto usef
    m++
    }
usef:
open #1,file+2
lhelp=read #1,65535 to seg|0
close #1
if lhelp=0 then error 13
seg[lhelp]b=26

last_page=-1
current_page=lhelp-1:back(23):end_page=current_page
current_page=0

forever
    {
    cursor 1,0
    display_page
    wait for keypressed
    ks=keyscan:s=high ks
    if s=81 then forward(23)
    if s=80 then forward(1)
    if s=73 then back(23)
    if s=72 then back(1)
    if s=79 then current_page=-1
    if s=71 then current_page=0
    if ks=17408 then abort
    if ks=15104 then help
    if ks=15616 then print_page
    if ks=15872 then print_all
    if ks=16128 then
	{
	colour 15
	scroll 0,23,79,24,0
	locate 23,0:repeat 80 print chr 196;
	print "Find: ";
	inputs find_text
	last_page=-1
	if peekb (find_text+2) then find_string(find_text+2)
	}
    if s=1 then return_to
    }

find_text:
string 20

file:
data 40
datab '\DOC\FAST.HLP'
space 40
