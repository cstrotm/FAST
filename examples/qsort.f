
;Quick Sort testing for very large fixed record length data
;----------------------------------------------------------

;(c) Peter Campbell, June/July 1994.

;designed program for sorting records with maximum length of about 256 bytes.
;input file can be any size at all - for use with high speed re-indexing
;eg: create and sort indexes in a database of 500,000 records where the
;index data for each record is 50 bytes (25MB file).

;use extensive splitting and caching routines to optimise performance on
;machines - preferably heaps of memory.

;use conventional memory for primary disk caching
;use XMS memory for secondary caching

unsigned array,items,levels,maxlevels

#stack memory 3000
#short

const use_assembly=1
const p32_bios=1
const xms_move_function=0

file_name ? 80

#include fast32.fi
#include xms.fi
#include cmd_line.fi
#include diskc.fi

proc abort(m)
    {
    print bios
    print bios "Aborted : ";
    while peekb m print bios chr peekb m;:m++
    print bios
    if cache_type then cache_type=0:xms_deallocate
    stop
    }

#include qsort.fi

;==============================================================================

proc print_array
    {
    m=0
    for n=1 to items
	print bios "[";n-1;"] = ";array[m]
	m+=2
    next n
    }

;==============================================================================

on error
    {
    print bios
    print bios "QSORT: error #";error
    ;temp
    print bios "xms_size=";xms_size;" ";
    printh bios peek (emms_bytes+2);peek emms_bytes
    printh "xms_??? (";xms_size;",";xms_seg;",";xms_off;",";high xms_offset;low xms_offset;")"

    abort("severe error")
    }

print bios
print bios "Quick Sorter v1.03 - Large File Processing. (c) Peter Campbell, July 1994."
print bios

#if use_assembly
if not processor32 then
    {
    print bios "Quick Sort requires an 80386 or higher processor!"
    beep:stop
    }
#endif

#long
if cmd_nos<>2 then
    {
    print bios "syntax: qsort file_name record_length"
    print bios "    eg: qsort test.dat 20"
    print bios
    print bios "please try again :-)"
    beep
    stop
    }
#short

m=cmd_add(1)
l=cmd_len(1)
moveb l from m to file_name
pokeb file_name+l,0

m=cmd_add(2)
l=cmd_len(2)
qwidth=0

while l
    {
    c=peekb m-'0'
    qwidth=qwidth*10+c
    m++:l--
    }

if (qwidth<1) or (qwidth>256) then
    {
    print bios "qsort: ";qwidth;" is an invalid length, range=1-256!"
    beep
    stop
    }

init_cache

open_file(file_name)
sort_file
close_file

print bios "Ok"
stop
