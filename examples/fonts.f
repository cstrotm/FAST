
;==============================================================================
;test vga character sets
;==============================================================================

var counter

function load_char(c,addr)
    {
    reg es=reg ds
    reg bp=addr
    reg bx=0f00h
    reg cx=1
    reg dx=c
    reg ax=1100h
    int 10h
    }

counter=0

;on int 1
;    {
    load_char('A',chara+counter)
    load_char('B',charb+counter)
    load_char('C',charc+counter)
;    counter++
;    if counter>15 then counter=0
;    }

;stop resident
stop

;==============================================================================

charA:
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00011110b
datab 00010010b
datab 00011110b
datab 00010010b
datab 00010010b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00011110b
datab 00010010b
datab 00011110b
datab 00010010b
datab 00010010b
datab 00000000b
datab 00000000b
datab 00000000b

charB:
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00011100b
datab 00010010b
datab 00011110b
datab 00010010b
datab 00011100b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00011100b
datab 00010010b
datab 00011110b
datab 00010010b
datab 00011100b
datab 00000000b
datab 00000000b
datab 00000000b

charC:
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00001110b
datab 00010000b
datab 00010000b
datab 00010000b
datab 00001110b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00000000b
datab 00001110b
datab 00010000b
datab 00010000b
datab 00010000b
datab 00001110b
datab 00000000b
datab 00000000b
datab 00000000b

