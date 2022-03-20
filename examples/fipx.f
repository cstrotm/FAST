
;===========================================================================
; FAST IPX Communications Handler
;===========================================================================

const ipx_socket=4246h		;FB
const test_length=15

;== functions ==============================================================

function ipx_init
    {
    reg ax=7a00h
    int 2fh
    ax=reg ax
    poke enteripx,reg di
    poke enteripx+2,reg es
    return low ax
    }

function ipx(func)
    {
    reg bx=func
    reg dx=ipx_socket
    inline 9ah		    ;callf enteripx routine
    enteripx: data 0,0
    ax=reg ax
    socket=reg dx
    return low ax
    }

proc setup_ecb_header(length)
    {
    poke ecb_fragments,1
    poke ecb_frag_addr,ipx_packet
    poke ecb_frag_addr+2,reg ds
    poke ecb_frag_size,length
    }

function poll_in_use
    {
    counter=0
    while peekb ecb_in_use
	{
	ipx(0ah)			;relinquish control
	if key=27 then beep:goto exit_fipx
	counter++
	}
;   print bios counter;" polls"
    return peekb ecb_completion
    }

function ipx_send(ipx_node,length)
    {
    length+=30
    moveb 6 from ipx_node to ipx_dest_node
    reg es=reg ds
    reg si=ipx_dest_network
    reg di=ecb_immediate
    if ipx(2) then return 0		;get targets address

    setup_ecb_header

    ;setup ipx packet(s)
    poke ipx_length,length

    ;send packet...
    reg es=reg ds
    reg si=ecb_header
    ipx(3)				;send packet

    c=poll_in_use
    return c
    }

function ipx_receive
    {
    setup_ecb_header(576)	;maximum length

    ;start listening...
    print bios "listening ... ";
    reg es=reg ds
    reg si=ecb_header
    if ipx(4) then
	{
	print bios " socket does not exist."
	return 0
	}

    c=poll_in_use

    if c=0 then
	{
	rl=peek ipx_length-30
	rm=ipx_data
	print bios "'";
	while rl print bios chr peekb rm;:rm++:rl--
	print bios "'"
	}

    return c
    }

;== start ==================================================================

print bios "FAST IPX Communicator"

if ipx_init <> 0ffh then
    {
    print bios "Error initialising IPX (not loaded?)."
    stop
    }

if ipx(0)=0feh then	    ;open socket?
    {
    printh bios "Error opening socket - no more sockets available."
    stop
    }

print bios "IPX initialised and socket open Ok"

server=0
s=81h
while peekb s<>13
    {
    if (lcase peekb s)='s' then server=1
    s++
    }

if server=0 then
    {
    moveb test_length from test_data to ipx_data    ;hello?
    if ipx_send(node_broadcast,test_length) then
	{
	print bios "Error sending packet."
	goto exit_fipx
	}

    if ipx_receive=0 then
	{
	print bios "FIPX server is NOT listening."
	goto exit_fipx
	}
    }

chat:

if not server then
    {
    print bios
    print bios "Lets have a chat?"
    print bios "<msg> ";
    inputs talk
    print bios

    length=peekb (talk+1)
    if length=0 then print bios "User aborted.":goto exit_fipx
    moveb length from talk+2 to ipx_data

    if ipx_send(ipx_source_node,length) then
	{
	print bios "Error sending packet."
	goto exit_fipx
	}
    }

if not ipx_receive then
    {
    print bios "Listening: nobody there."
    goto exit_fipx
    }

if server then
    {
    moveb 2 from ipx_ok to ipx_data
    if ipx_send(node_broadcast,2) then	    ;use source node if not server
	{
	print bios "Error sending packet."
	goto exit_fipx
	}
    }

goto chat

exit_fipx:
ipx(1)			    ;close socket
stop

;== data ===================================================================

test_data: datab 'Hello FastBase?'

ipx_ok: datab 'Ok'

node_broadcast: datab 0ffh,0ffh,0ffh,0ffh,0ffh,0ffh

talk: string 70

ipx_packet:

ipx_checksum:		data	0ffffh
ipx_length:		data	0		;set this
ipx_transport:		datab	0
ipx_type:		datab	0
ipx_dest_network:	datab	0,0,0,0
ipx_dest_node:		datab	0,0,0,0,0,0	;set this
ipx_dest_socket:	data	ipx_socket
ipx_source_network:	datab	0,0,0,0
ipx_source_node:	datab	0,0,0,0,0,0
ipx_source_socket:	data	0000h

ipx_data:		space 546	;maximum data size per packet

;notes:
;if the node = 0ffffffffffffh this is broadcast to all


ecb_header:			;ecb

ecb_link:	    data    0,0
ecb_address:	    data    0,0
ecb_in_use:	    datab   0
ecb_completion:     datab   0
ecb_socket:	    data    ipx_socket
ecb_workspace:	    data    0,0
ecb_driver:	    datab   0,0,0,0,0,0,0,0,0,0,0,0
ecb_immediate:	    datab   0,0,0,0,0,0
ecb_fragments:	    data    0	    ;lo-hi !!!
ecb_frag_addr:	    data    0,0
ecb_frag_size:	    data    0
		    space   9*6     ;room for 10 fragments

;notes:
;send must set esr address, socket, immediate address and 1 fragment
;receive must set esr address, socket and 1 fragment

;in use flag:
; 00 = done
; FB = send or receive has occurred but ecb is in a holding queue waiting
; FD = an event is scheduled and IPX awaits expiration
; FE = IPX is listening for incoming packets
; FF = ECB is in use sending a packet
; FA = ECB is being processed
; F8 = send was attempted while IPX was busy, queued for later processing

;completion code on send:
; 00 = sent ok
; FC = send cancelled
; FD = packet is malformed (ie: ecb or ipx packet has invalid values)
; FE = undelivered - there was no listener
; FF = hardware/network failure - can't send packet

;completion code on receive:
; 00 = packet has been received
; FC = listen request has been cancelled
; FD = overflow - packet received but fragment count = 0 or not enough space
; FF = the socket is closed
