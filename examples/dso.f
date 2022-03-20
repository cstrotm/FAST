#inpend=0
buffer ? 25*82+1

forever
    {
    print bios "File: ";
    inputs name
    print bios

    if peekb (name+2)=0 then terminate
    load name+2,buffer
    create #1,name+2
    m=buffer
    repeat 25
	{
	write #1,80 from m:m+=80
	write #1,2 from w_eol
	}
    write #1,1 from w_eof
    close #1
    }

name:
string 20

w_eol: datab 13,10
w_eof: datab 26
