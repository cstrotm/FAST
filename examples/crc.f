;XMODEM CRC

var poly

crctab ? 256*2

proc make_table
    {
    for val=0 to 255
	result=val
	for i=1 to 8
	    if result and 1 then result=(rrightz result) xor poly
		else result=rrightz result
	next i
	poke crctab+val*2,result
    next val
    }

function calc_crc(ptr,count)
    {
    crc=0
    while count
	{
	crc=crc xor peekb ptr:ptr++
	crc=(high crc) xor peek (crctab+2*(crc and 0ffh))
	count--
	}
    return crc
    }

proc do(poly)
    {
    make_table
    printh bios calc_crc(name1,1)
    printh bios calc_crc(name2,3)
    printh bios calc_crc(name3,30)
    print bios
    }

do(8404h)
do(0a001h)
stop

name1: datab 'T'
name2: datab 'THE'
name3: datab 'THE,QUICK,BROWN,FOX,0123456789'
