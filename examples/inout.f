
const address=378h,clock=378h,portin=379h
const chigh=0fdh,clow=0fch,mask=80h,shift=2
const counter=30000

var32 n32

adc ? counter+1     ;30,000 byte variable

out address,0feh
out address,0fch

print bios "counter = ";counter
start=timer

for i=1 to counter
    bits=0
    repeat 8
	{
	out clock,chigh
	bits=((in portin) and mask)+bits*2
	out clock,clow
	}

    pokeb adc+i,(bits/mask) xor 0ffh
next i

end=timer
clicks=end-start
n32=(counter/clicks)*182
n32=n32/10
print bios low n32;" bytes per second."
