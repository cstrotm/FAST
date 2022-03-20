;See DL
;DA simply loads the COM file and produces an output file in the form of
;an assembly langauge file - nearly SOFA compatible.

#include DL.F

var b
buffer ? 120

proc put(c)
    {
    pokeb b,c
    b++
    }

#entry	    ;***** override code entry point

setup
curtoloc
load_file

moveb 40 from open_name to asm_name
x=searchb 40 from asm_name for '.'
moveb 4 from asm_ext to x+1

create #1,asm_name

GET_SYMBOL=0
SCROLLF=1
ORG_IP=CUR_IP
while CUR_IP below FINEND
    {
    m=locpos
    CUR_IP=UNASSEMBLE(CUR_IP)

    b=buffer
;   sl=start_loc-130	    ;start of Lxxxx?
;   if video[sl]b='L' then
;	{
;	put(video[sl]b)
;	put(video[sl+2]b)
;	put(video[sl+4]b)
;	put(video[sl+6]b)
;	put(video[sl+8]b)
;	}
;   put(9)	;tab
;
;   sl+=14		    ;start of instruction

    sl=start_loc-170	    ;temp

    l=start_loc-2
    while video[l]b=' ' l-=2

    while sl<=l
	{
	put(video[sl]b)
	sl+=2
	}

    put(13)
    put(10)
    write #1,b-buffer from buffer

    if keyscan then
	{
	m=locpos
	print "Abort? (y/n) ";
	loctocur
	wait for keypressed
	if (lcase key)='y' then
	    {
	    print cr "Assembly file incomplete."
	    goto aborted
	    }
	locpos=m
	print "             ";
	locpos=m
	}
    }

close_asm:
print "Assembly file create Ok."
aborted:
close #1
loctocur
stop

;===========================================================================

asm_name:
space 45

asm_ext: fname 'asm'
