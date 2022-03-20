;DOTS, written to win a $10 bet with MF at work (with any luck).

const size=7	;size*size dots
const sz=size-1
const distx=32,disty=16
const absize=5
array_size=(size-1)*(size-1)*absize
total_dots=(size-1)*(size-1)
video=0b800h
board ? size*size*absize   ;Approximate size def.

proc abort screen 3:colour 7:cls:terminate

proc reset_game
    {
    dx=0:dy=0
    fillb array_size from board with 0
    }

proc clear_bottom colour 0:scroll 0,20,79,22,0:colour 1

proc display_screen
    {
    colour 0:screen 6:cls
    colour 7
    cursor 1,6
    print bios ".. D O T S .."
    for x=0 to size-1
    for y=2 to size+1
	px=x*distx:py=y*disty
	plot px,py
	plot px+1,py
	plot px+1,py+1
	plot px,py+1
    next y,x
    }

proc move_p(pl)
    {
    }

proc move_c(pl)
    {
    }

function more_moves
    {
    c=0:m=board+4
    repeat total_dots
	{
	if not peekb m then c++
	m+=absize
	}
    return c
    }

function total_squares(pc)
    {
    c=0:m=board+4
    repeat total_dots
	{
	if peekb m=pc then c++
	m+=absize
	}
    return c
    }

function box_filled(pl,pc)
    {
    c=0:m=board
    for y=0 to sz
    for x=0 to sz
	if not peekb(m+4) then
	    {
	    if peekb m and peekb(m+1) and peekb(m+2) and peekb(m+3) then
		{
		pokeb m+4,pc
		c++
		cursor y*2+3,x*4+2:print bios pc;
		}
	    }
	m+=absize
    next x,y
    return c
    }


begin_dots:

reset_game
display_screen
cursor 20,0:print bios "Player 1, press first letter of your name (space=computer) ";
another1:
wait for keypressed
k=ucase key
if k=27 then abort
if k=' ' then pc1='*':goto get2
if k=(lcase k) then beep:goto another1
pc1=k

get2:
print bios chr pc1 cr lf
print bios "Player 2, press first letter of your name (space=computer) ";
another2:
wait for keypressed
k=ucase key
if k=27 then abort
if k=' ' then pc2='*':goto start_game
if (k=pc1) or (k=(lcase k)) then beep:goto another2
pc2=k

start_game:
clear_bottom
mpl=0

while more_moves
    {
    pcc=pc1:if mpl then pcc=pc2
    cursor 20,0:print bios "Your turn: ";chr pcc
    if key=27 then abort
    if mpl=0
	then if pc1='*' then move_p(1) else move_c(1)
	else if pc2='*' then move_p(2) else move_c(2)
    if box_filled(mpl,pcc) then goto next_move
    mpl=not mpl
    next_move:
    }

cursor 22,0:print bios "Game over."
while keyscan {}
ts1=total_squares(pc1):ts2=total_squares(pc2)
print bios tab chr pc1;" has ";ts1;" squares."
print bios tab chr pc2;" has ";ts2;" squares.";
wait for keyscan
goto begin_dots
