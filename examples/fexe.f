
;==============================================================================
; read .EXE files and display header
; designed to evaluate files for generating EXE with Fast v3.000
;==============================================================================

;22-02-1995

buffer ? 5000
file_name ? 60
var32 exe_length

#include cmd_line.fi
const p32_bios=1
#include fast32.fi

print bios
print bios "EXE File Analyser - (c) Peter Campbell, Feb 1995."
print bios

if cmd_nos<>1 then
    {
    print bios "syntax: fexe filename.exe"
    stop
    }

m=cmd_add(1)
l=cmd_len(1)
moveb l from m to file_name
pokeb file_name+l,0

print bios "opening ";
m=file_name
repeat l
    {
    print bios chr ucase peekb m;
    m++
    }
print bios " ";

open #1,file_name
r=read #1,1000 to buffer
print bios
print bios

exe_id=peek buffer
exe_len1=peek (buffer+2)
exe_len2=peek (buffer+4)
exe_seg=peek (buffer+6)
exe_hsize=peek (buffer+8)
exe_minp=peek (buffer+10)
exe_maxp=peek (buffer+12)
exe_ssdisp=peek (buffer+14)
exe_sp=peek (buffer+16)
exe_checksum=peek (buffer+18)
exe_ip=peek (buffer+20)
exe_scode=peek (buffer+22)
exe_reloc=peek (buffer+24)
exe_overlay=peek (buffer+26)

exe_length=(exe_len2*512)+exe_len1

print  bios "Header                   ";chr low exe_id;chr high exe_id
print  bios "Length             ";:n32=exe_length:print32(' '):print bios
printh bios "Segment Addresses        ";exe_seg
printh bios "Header Size (para)       ";exe_hsize
printh bios "Minimum Paragraphs       ";exe_minp
printh bios "Maximum Paragraphs       ";exe_maxp
printh bios "Stack Seg Displacement   ";exe_ssdisp
printh bios "SP register              ";exe_sp
printh bios "Checksum on header       ";exe_checksum
printh bios "IP register              ";exe_ip
printh bios "Start of code segment    ";exe_scode
printh bios "Relocation table address ";exe_reloc
printh bios "Overlay Number           ";exe_overlay
printh bios "Buffer Memory            "

exe_buffer=peek (buffer+28)


stop

;==============================================================================

