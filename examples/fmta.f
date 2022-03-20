
;== FORMAT DISK A: =========================================================

var head,track

function reseta
    {
    reg dx=0,ax=0:int 13h
    }

function format
    {
    m=table
    repeat 9
	{
	pokeb m,track
	pokeb m+1,head
	m+=4
	}
    fh=head*256
    ft=track*256
    reg es=reg cs,bx=table,cx=ft,dx=fh,ax=0509h
    int 13h
    if carry then return 0
    return 1
    }

print bios "Formatting Drive A:"
reseta
errors=0

for track=0 to 39
print bios track;" ";
for head=0 to 1
print bios "(";head;") ";

repeat 3
    {
    if format then goto next_loop
    print bios "r ";
    reseta
    }
errors++

if scan=1 then print bios cr lf "ABORTED" cr lf:stop

next_loop:
next head
if (track and 3)=3 then print bios
next track

print bios cr lf "Finished, ";errors;" error(s)."
stop

;===========================================================================

table:
datab 0,0,0,2
datab 0,0,3,2
datab 0,0,6,2
datab 0,0,1,2
datab 0,0,4,2
datab 0,0,7,2
datab 0,0,2,2
datab 0,0,5,2
datab 0,0,8,2
