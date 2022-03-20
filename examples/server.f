
serial_size=20000:serial_seg=allocate serial_size/16+1
serial_head=0:serial_tail=0

function get
    {
    c=0:got=0
    if serial_tail<>serial_head then
	{
	c=serial_seg[serial_tail]b:got=1
	serial_tail++:if serial_tail>=serial_size then serial_tail=0
	}
    return c
    }

proc send_string(sss,ssm)
    {
    while sss[ssm]b
	{
	send_chr:
	if key=27 then return
	l=serial_send sss[ssm]b
	if l and 8000h then goto send_chr

	ssm++
	}
    }

enable serial,0a3h

forever
    {
    k=key
    if k=27 then
	{
	disable serial
	stop
	}
    if k then send_string(reg cs,.k)
    c=get:if c then print bios chr c;
    }
