
var32 a32,b32


print bios

a32=100:b32=100
if 0=0 then if32 a32>=b32 then print bios "100,100" else print "x"
a32=10:b32=100
if32 a32>=b32 then print bios "10,100"
a32=100:b32=10
if32 a32>=b32 then print bios "100,10"

a32=0:b32=0
if32 a32>=b32 then print bios "0,0"
a32=0:b32=100
if32 a32>=b32 then print bios "0,100"
a32=100:b32=0
if32 a32>=b32 then print bios "100,0"

a32=100000:b32=100000
if32 a32>=b32 then print bios "100000,100000"
a32=0:b32=100000
if32 a32>=b32 then print bios "0,100000"
a32=100000:b32=0
if32 a32>=b32 then print bios "100000,0"


a32=90000:b32=100000
if32 a32>=b32 then print bios "90000,100000"
a32=100000:b32=90000
if32 a32>=b32 then print bios "100000,90000"

print bios
print bios "ok"
