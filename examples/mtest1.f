
;===========================================================================
; FBGRAPH testing
;===========================================================================

#inpend=0
var32 y32

const show_graph=1
const show_image=0

function mux(f)
    {
    reg ax=0c600h+f		    ;mux command
    int 2fh
    return low reg ax
    }

;== init ===================================================================

if mux(00)<>0ffh then print bios "FBGRAPH not loaded!":beep:stop

;== send data ==============================================================

#if show_graph

randomize timer

for x=1 to 16

y32=rnd and 511
reg bx=x
reg cx=high y32
reg dx=low y32
mux(01)

next x

y32=2800
for x=0 to 20
reg bx=x
reg cx=high y32
reg dx=low y32
mux(01)
n=rnd and 255
y32-=n
next x


reg es=reg ds
reg si=msg_title
reg dx=01100111b	       ;bar graph with grid, 8 colours
reg dx=00110111b		;line graph with diamonds

#endif

;== image ==================================================================

#if show_image

print bios "Input File Name : ";
inputs file_name
if peekb (file_name+2)=0 then stop

seg=allocate 4096
load file_name+2,seg|0
moveb 30 from msg_title to seg|65000
reg es=seg
reg si=65000
reg dx=10000000b

#endif

mux(10h)

stop

;== data ===================================================================

msg_title:
fname 'FBGRAPH Test Report'
fname 'x axis'
fname 'y axis'
fname 'zero'
fname 'one'
fname 'two'
fname 'three'
fname 'four'
fname 'five'
fname 'six'
fname 'seven'
fname 'eight'
fname 'nine'
fname 'ten'
fname 'green'
fname 'red'
fname 'white'
fname 'blue'
fname 'cyan'
fname 'magenta'
fname 'brown'
fname 'yellow'
datab 1ah

file_name: string 40
