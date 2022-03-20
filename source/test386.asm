
;test if 32 bit processor?
;=========================

;excerpt of code taken from PC-INTERN book.
;only used this much code because wanted to test if 32 bit processor or not!

;test if 80286 or lower?

		pushf
		xor ax,ax
		push ax 		;push 0
		popf			;pop, sets 12-15 bit if 8086?
		pushf
		pop ax
		and ax,0f000h
		cmp ax,0f000h
		lea dx,p8086
		je print_type

;test if 486,386,286?

		mov ax,7000h
		push ax
		popf
		pushf
		pop ax
		and ax,7000h
		lea dx,p80286
		je print_type

		lea dx,p80386

print_type	mov ah,9
		int 21h

		popf
		int 20h

;==============================================================================

p8086		db 'Processor 8086-80186',13,10,'$'
p80286		db 'Processor 80286',13,10,'$'
p80386		db 'Processor 80386+ (32)',13,10,'$'
