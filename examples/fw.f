
;==============================================================================
;=		    Fast Word - Word Processor for DOS			      =
;=		    (c) Peter Campbell Software 1994... 		      =
;==============================================================================

;setup
;load file (size = memory limit)
;display has title, page box, font display box & other options
;editor stores file as word processor (text, newline, formfeed) - auto wrap?
;control character storage : 0,control,length [data]

;screens 25*80, 50*80, 60*132

#errors off
#short

#window memory 6000
ext_inpend=0

goto fw_main

;== includes ==================================================================

const xms_move_function=1

#include fwdata.f
#include extinput.fi
#include fsort.fi
#include cmd_line.fi
#include xms.fi

#include fwprocs.f
#include fwedit.f
#include fwkeys.f

;== main ======================================================================

fw_main:
fast_word_setup
fw_keys=(key_end-key_table)/2

forever
    {
    set_positions
    display_main
    fw_not_working

    wait for keypressed
    ks=keyscan

    x=search fw_keys from key_table for ks
    if x then call peek (proc_table+x-key_table)
    else
	{
	k=low ks
	if (k<>0) and (k<>26) then put_char(k)
	}
    }

;== documentation =============================================================

#if 0

(14-09-1995 rewrote to scrap the WP features and use as WT look alike)

All word processor files will be stored with the prefix "FWv1.xx!"

Character 13 = hard carriage return
Character 10 = soft carriage return
Character 12 = hard form feed
Character 11 = soft form feed

Character 10 is used for display purposes only, it is automatically
inserted/removed when the characters on a line exceed the printable size on a
page - this is so the display automatically reflects the printed page.

The control file FW.CFG will store all fonts and the character widths using a
scale in 300dpi.

FW.CFG format	(hex)
-------------
0000  0008  FWv1.xx!
0008  000A  Number of fonts 0-200
0100  0100  Fonts
	    0000=font name (20 chars)
	    0014=number of characters
	    0015=font height
	    0016=?
	    0018=font file (8 chars)
	    0020=width [256] points (scaled to 72pt)

#endif
