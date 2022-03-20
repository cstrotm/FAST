;Shut system, park heads.
#short

print bios cr lf "System SHUT activated...";

e=0
for h=0 to 3
print bios "  ";chr '0'+h;
dx=h*256+80h
reg dx=dx,cx=6691h,ax=0c01h:int 13h
if carry then print bios "x";:e=1 else print bios chr 251;
next h

if e then print bios cr lf "Error!":terminate

print bios cr lf lf "Done.";
forever {}
