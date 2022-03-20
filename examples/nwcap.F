
;==============================================================================
; Novell Netware Capture Program, based on Assembly API SKD
;==============================================================================

;(c) Peter Campbell Software 1996

var nw_nlm
var request_len,reply_len

request_buffer ? 62
reply_buffer   ? 62

const object_queue=3

;==============================================================================

const object_len=6
object_name: datab 'SERVER'

;==============================================================================

function netware_init
    {
    ;see if VLM multiplexor is installed?
    reg ax=7a20h
    int 2fh
    nw_vlm=not reg ax
    if nw_vlm then	;vlm?
	{
	poke enternet,reg bx
	poke enternet+2,reg es
	}
    else		;netx F2?
	{
	reg ax=0dc00h
	int 21h
	iconnection=low reg ax
	}
    }

function netware_call(func)
    {
    if nw_vlm then
	{
	reg ax=func
	inline 9ah		;callf VLM handler
	enternet: data 0,0
	ax=reg ax
	return ax
	}
    else
	{
	reg si=request_buffer
	reg di=reply_buffer,es=reg ds
	reg cx=request_len
	reg dx=reply_len
	reg ax=0f200h+func
	int 21h 		;call NETX F2
	}
    }

;==============================================================================

#entry
print bios "FastBase: Novell Capture Program; v1.00 [08-08-96]"
print bios

netware_init

print bios "Using Netware ";
if nw_vlm then print bios "VLM"; else print bios "NETX";
print bios " DOS Requester."



pokeb request_buffer+2,35h
poke request_buffer+3,object_queue
pokeb request_buffer+5,object_len
move object_len from object_name to request_buffer+6
request_len=6+object_len
poke request_buffer,request_len
reply_len=62
netware_call(17h)	    ;get bindery object ID

x=reply_buffer
repeat reply_len
    {
    c=peekb x:x++
    if c<32 then cp='.'
    else if c>127 then cp='.'
    else cp=c
    printhb bios c;" ";
    }
print bios

print bios "Ok"
stop

;==============================================================================

reply_buffer:
rb_status:		    datab 0
rb_print_flags: 	    datab 0
rb_tab_size:		    datab 8
rb_printer:		    datab 0
rb_number_of_copies:	    datab 1
rb_form_type:		    datab 0
rb_reserved1:		    datab 0
rb_banner_text: 	    datab 0,0,0,0,0,0,0,0,0,0,0,0,0
rb_reserved2:		    datab 0
rb_lpt_device:		    datab 0
rb_flush_timeout:	    datab 0,0
rb_flush_capture:	    datab 0
rb_max_lines:		    datab 0,66
rb_max_chars:		    datab 0,132
rb_form_name:		    datab 0,0,0,0,0,0,0,0,0,0,0,0,0
rb_lpt_capture_flag:	    datab 0
rb_file_capture_flag:	    datab 0
rb_timing_out_flag:	    datab 0
rb_print_setup_buffer:	    datab 0,0,0,0
rb_print_reset_buffer:	    datab 0,0,0,0
rb_position_in_connection:  datab 1
rb_capture_in_progress:     datab 0
rb_print_queue_flag:	    datab 0
rb_print_job_valid:	    datab 0
rb_print_queue_object_id:   datab 0,0,0,0
rb_print_job_number:	    datab 0,0
