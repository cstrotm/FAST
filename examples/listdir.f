
#errors off

buffer ? 80

reg dx="*.*",cx=16h:dos 4eh
goto find_next

forever
    {
    find next
    find_next:
#errors on
    if error then goto end_of_directory

    ds=dta segment
    do=dta offset

    attribute=ds[do+15h]b
    if attribute and 10h then	    ;directory?
	{
	m=do+1eh
	while peekb m print bios chr ds[m]b;:m++
	print bios
	}
    }

end_of_directory:
print bios
reg si=buffer
reg dx=0
dos 47h
print bios "Current Directory = ";
m=buffer
while peekb m print bios chr peekb m;:m++
print bios
stop
