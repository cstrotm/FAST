;Print all sectors found on a disk...
buffer ? 512

print bios "Working ..." cr lf

for track=0 to 41
for head=0 to 1
printhb bios "Track "track", head "head"  ";
for sector=0 to 16
t=256*head
reg es=reg cs,cx=256*track+sector,dx=t,bx=buffer,ax=0201h:int 13h
if carry then goto nexts
printhb bios sector" ";
nexts:
test break
next sector
print bios
next head
next track
