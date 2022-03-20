
repeat 5
    {
    moveb 2 from tempdir to temporary_file2
    reg dx=temporary_file2
    reg cx=0
    dos 5ah:ax=reg ax
    handle #6,ax	    ;set file handle #6

    m=temporary_file2
    while peekb m
	{
	print bios chr peekb m;
	m++
	}
    print bios

    close #6
    delete temporary_file2
    }

stop

temporary_file2: space 64
tempdir: fname '.'
