
;==============================================================================
; FAX
;==============================================================================

const resident_tsr=0	    ;1=tsr

#include cmd_line.fi
#include stick.fi

const fax_dle=010h
const fax_etx=003h
const fax_tsi=0c2h
const fax_dcs=0c1h
const fax_tcf=000

;==============================================================================

function wait_response(wr_message,attempts)
    {
    wrl=0:wrm=wr_message
    while peekb wrm wrl++:wrm++

    while attempts
	{
	if key=27 then abort_error("User Aborted")
	wait_for_data(5,1)

	if len then
	    {
	    m=buffer
	    while len>=wrl
		{
		if not compareb wrl at m with wr_message then return 1
		m++:len--
		}
	    }
	attempts--
	print bios ".";
	}

    return 0
    }

;==============================================================================

proc establish_connection
    {
    ;dial!!!
    wait_seconds(1)
    print bios "Dialing ";

    len=cmd_len(1)
    moveb len from cmd_add(1) to dial_number+4
    pokeb dial_number+4+len,0

    send_string(dial_number)
    wait_seconds(1)

    if wait_response("CONNECT",99) then return

    abort_error("Failed to connect.")

    dial_number:
    datab 'atdt'
    space 128
    }

;==============================================================================

proc pre_message
    {
    send_string("at +frh=3")
    if not wait_response("CONNECT",2) then abort_error("No CONNECT #1")

    send_string("at +frh=3")
    if not wait_response("CONNECT",2) then abort_error("No CONNECT #2")

    send_string("at +frh=3")
    if not wait_response("NO CARRIER",2) then abort_error("No NO CARRIER #1")

    send_string("at +fth=3")
    if not wait_response("CONNECT",2) then abort_error("No CONNECT #3")

    send_character(fax_tsi)
    send_character(fax_dle)
    send_character(fax_etx)
    if not wait_response("CONNECT",2) then abort_error("No CONNECT #4")

    send_character(fax_dcs)
    send_character(fax_dle)
    send_character(fax_etx)
    if not wait_response("OK",2) then abort_error("No OK #1")

    send_string("at +ftm=96")
    if not wait_response("CONNECT",2) then abort_error("No CONNECT #5")

    send_character(fax_tcf)
    send_character(fax_dle)
    send_character(fax_etx)
    if not wait_response("OK",2) then abort_error("No OK #2")

    send_string("at +frh=3")
    if not wait_response("CONNECT",2) then abort_error("No CONNECT #6")
    if not wait_response("OK",2) then abort_error("No OK #3")

    print bios "phase 3 ok"
    }

;==============================================================================

proc message_transmit
    {
    }

;==============================================================================

proc post_message
    {
    }

;==============================================================================

initialise_modem
send_string("at +fclass=1"):wait_for_data(1,0)

establish_connection	;phase a

pre_message		;phase_b
message_transmit	;phase_c
post_message		;phase_d

abort_error("Exit")     ;phase e
