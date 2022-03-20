
;== BANK PROGRAM ===========================================================

const max=200
name ? 20
tran ? max*52
var atot,adep,awid,btot,bdep,bwid,amount
var y,lasty
#include extinput.fi
#include fsort.fi
#inpend=0
ext_inpend=0
fill 10 from name with 2020h

on error
    {
    cursor 24,0:repeat 3 print bios
    pop ip
    error msg "\dos.err"
    printh bios "! (IP=";ip;")"
    stop
    }

proc msg(ma)
    {
    open window msg_window
    locate 17,22:prints ma,0
    wait for keyscan
    close window
    }

proc setup_screen
    {
    cls
    colour 15
    locate 0,18
    print "BANK v1.1 written by Peter Campbell 10/09/90"
    locate 1,8
    print "Add	Delete	 Save  Load  ESCape  (press first letter of option)"
    colour 7
    print "Date      Particulars				   Deposit  Withdrawl  Paid";
    repeat 80 print chr 'Ä';
    locate 22,0
    repeat 80 print chr 'Í';
    print "Actual Balance   $"
    print "Budgeted Balance $";
    }

function confirm(ca)
    {
    open window ask_window
    locate 11,32:prints ca,0:loctocur
    sf:
    k=lcase key
    if k=27 then close window:return k
    if k='y' then close window:return k
    if k='n' then close window:return k
    goto sf
    }

proc load_file
    {
    open window load_window
    load_fn:
    locate 12,14
    print "File name: ";
    l=ext_string(name,20)
    if l then
	{
	#errors off
	open #1,name
	#errors on
	if error then
	    {
	    if error=2 then
		{
		locate 14,14:print "New file... 	 ";
		repeat 2 repeat 50000 {}
		r=0:goto read0
		}
	    cursor 14,14:error msg "\dos.err"
	    goto load_fn
	    }
	r=read #1,max*52 to tran
	close #1
	read0:
	trans=r/52
	}
    else error 999
    close window
    lasty=-1:y=0
    changed=0
    }

function save_file(ask)
    {
    if changed=0 then return 1 ;done.
    if ask then
	{
	k=confirm(save_question)
	if k=27 then return 0
	if k='n' then return 1
	}
    save name,tran,trans*52
    changed=0
    return 1
    }

proc print2(p)
    {
    printhb (p mod 10)+(p/10)*16;
    }

proc printn(p)
    {
    n=digits p
    if n<5 then repeat 5-n print " ";
    print p;
    }

proc display
    {
    if (y/18)<>lasty then
	{
	lasty=y/18
	m=tran+lasty*52
	yy=lasty*18

	for s=4 to 21
	locate s,0
	if yy>=trans then
	    {
	    fill 80 from video|locpos with 0720h
	    }
	else
	    {
	    print2(peekb (m+2))
	    print "/";
	    print2(peekb (m+1))
	    print "/";
	    print2(peekb m)
	    print "  ";
	    n=printm m+3,40
	    locate s,56
	    printn(peek (m+43)
	    print "    ";
	    printn(peek (m+47)
	    print "	 ";
	    print chr 'N'+11*(peekb(m+51)>0);
	    }
	m+=52
	yy++
	next s
	}
    }

proc bar(b,c)
    {
    m=(b+4)*160+1
    repeat 80 video[m]b=c:m+=2
    }

function add_tran
    {
    if trans>=max then msg(no_more):return 0
    fillb 52 from tran+trans*52 with 0
    y=trans
    trans++
    changed=1:lasty=-1
    return 1
    }

function edit_tran
    {
    if not trans then return 0
    locate y+4,0
    changed=1:lasty=-1
    return 1
    }

proc delete_tran(ask)
    {
    if not trans then return
    if ask then
	{
	k=confirm(delete_question)
	if k<>'y' then return
	}
    m=tran+52*y
    moveb (trans-y)*52 from m+52 to m
    trans--
    changed=1:lasty=-1
    }

;== Entry ==================================================================

setup_screen
load_file

forever
    {
    display
    bar(y mod 18,120)
    wait for keypressed
    s=scan
    bar(y mod 18,7)

    if s=1 then
	{
	if save_file(1) then error 999
	}
    if s=31 then save_file(0)
    if s=38 then
	{
	if save_file(1) then load_file
	}

    if s=30 then
	{
	if add_tran then if not edit_tran then delete_tran(0)
	}
    if s=28 then edit_tran
    if s=32 then delete_tran(1)

    if s=71 then y=0
    if s=72 then y--
    if s=73 then y-=18
    if s=79 then y=trans-1
    if s=80 then y++
    if s=81 then y+=18
    if y>=trans then y=trans-1
    if y<0 then y=0
    }

;== data ===================================================================

load_window: datab 0,0,12,10,70,16,15,26
ask_window:  datab 0,0,30,9,50,13,15,26
msg_window:  datab 0,0,20,15,60,19,15,26

no_more:	 fname 'Maximum Transactions Entered'
save_question:	 fname 'Save? y/n '
delete_question: fname 'Delete? y/n '
