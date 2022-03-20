
	#org 100h	;Optional (could make ROMable code).

option	equ 1

label	mov ax,20

	#if option
	inc ax
	#endif

#include copiloti.asm

	int 20h

	#para		;Paragraph align data.
	db 1000101b	;Binary number.
	dw 0fdeah	;Hex number.
