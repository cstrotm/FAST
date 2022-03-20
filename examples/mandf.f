screen 6
mode 640,or
#inpend=0

gr ? 128

randomize timer
on break screen 3:terminate
proc file_error screen 3:print bios "File error.":terminate
a=allocate 10240/16:if error then break

print bios "File: ";
inputs name
if peekb (name+2)=0 then break
load name+2,a|0:if error then file_error

cursor 1,0
print bios "Scale: ";
sc=inputb:sb=sc*sc*2
if sb>128 then break
red:
colour 0:cls:colour 7
m=0
x=0:y=0
forever
{
nb:
b=a[m]b
m++
if b=254 then goto endx
if b=255 then x++:y=0:goto nb

fill sc*sc from gr with 0
d=40/(b/sc)
if d<=0 then goto prt
if (d*d*2)>sb then d=sc/2

fill d*d from gr with 0101h
ma=gr
repeat d*d*2
    {
    ga=gr+(rnd mod sb)
    mc=peekb ma
    pokeb ma,peekb ga
    pokeb ga,mc
    ma++
    }
ga=gr
for px=1 to sc*2
for py=1 to sc
if peekb ga then plot 40+x*sc*2+px,20+y*sc+py
ga++
next py,px
prt:
y++
}

endx:
cursor 24,0:print bios "R=REDRAW ESC=ABORT";
wait for keypressed
ew:
k=lcase key
if k='r' then goto red
if k<>27 then goto ew
break

name:
string 32
