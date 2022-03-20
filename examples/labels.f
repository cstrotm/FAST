cls
copies=1
serial=0

proc abort print:loctocur:stop

proc output(m)
    {
    repeat copies
	{
	om=m
	lprint
	while peekb om lprint chr peekb om;:om++
	if serial then lprint "Serial No. ";serial else lprint
	lprint "(c) Peter Campbell Software"
	lprint
	if serial then serial++
	}
    }

start:
locate 3,0
print "Peter Campbell Software Label Printing: v1.0"
repeat 44 print chr 'Í';
print
print "S - Serial Number: ";serial;"	"
print "C - Copies: ";copies;"	 "
print
print "1 - Free Flow Database"
print "2 - FASTBASE - SALES"
print "3 - FASTBASE - 4GL"
print "4 - FAST Registered"
print "5 - FAST PD"

cursor 14,0
wait for keypressed
k=lcase key

if k=27 then abort
if k='s' then cursor 5,19:serial=input
if k='c' then cursor 6,12:copies=input:if copies=0 then copies=1

if k='1' then output(lab_ffd)
if k='2' then output(lab_fbd)
if k='3' then output(lab_fb)
if k='4' then output(lab_freg)
if k='5' then output(lab_fun)

goto start

;each area takes 2 lines.

lab_ffd:
datab 'Free Flow Database',13,10
datab 'Start: type readme',13,10
datab 0

lab_fbd:
datab 'FASTBASE - SALES',13,10
datab '4GL Applications Developer.',13,10
datab 0

lab_fb:
datab 14,'FASTBASE',13,10
datab '4GL Applications Developer.',13,10
datab 0

lab_freg:
datab 'FAST',13,10
datab 'Registered Version',13,10
datab 0

lab_fun:
datab 'FAST',13,10
datab 'Public Domain',13,10
datab 0
