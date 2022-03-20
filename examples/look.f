;FAST LOOKUP free table locations.

m ? 40000

fill 20000 from m with 0
load "\asm\fast.com",m

print bios "Searching... ";
for tf=1 to 255
if not searchb 20000 from m+4000h for tf then print bios tf;" ";
next tf
