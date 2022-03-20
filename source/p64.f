
area ? 20
number ? 8
var32 n32,n64

n32=100000
n64=0

s=timer

repeat 10000
repeat 100
    {
    poke number,low n32
    poke number+2,high n32
    poke number+4,low n64
    poke number+6,high n64

    div64 number,10

    n32+=1
    if carry then n64++
    }

print bios timer-s;"/18.2 seconds."
beep
stop
