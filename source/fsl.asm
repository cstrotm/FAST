;
;Anti - Protection program (LOSER).
;Developed by Peter Campbell - 25/10/1988.
;

bso	equ 9*32+20
bssign	equ 0fa57h	;As close to FAST as possible.
bstimes equ 50		;You can use the program 50 times before termination.
serial	equ 44112	;Unique for each installation.

	mov ah,1
	mov dl,80h	;Find status of HARD DISK C:
	int 13h
	jc bscomp	;Only check protection if disk is ok,
	or al,al	;  otherwise have sympathy for user.
	jne bscomp

	call bsread	;Read boot sector.
	lea di,bsbuff+bso
	cmpw [di],bssign
	jne bscomp

	mov ax,[di+2]	;Counter.
	mov bx,[di+4]	;Timer (random).

	cmp ax,bx
	je bscomp	;Counter=Timer, no need to run FSL.

	cmp bx,serial
	jne bspo	;Wrong SN so piss off.

	mov [di+2],bx	;Counter=Timer.
	call bswrite	;Write sector.

	lea dx,bsdone
bsend	mov ah,9
	int 21h
	int 20h 	;Go no further!

bscomp	lea dx,bsexit
	jmps bsend

bspo	lea dx,bserr
	jmps bsend

bsread	mov ah,2	;Read boot sector.
	jmp bsboot

bswrite mov ah,3	;Write boot sector.
	mov si,3
bsboot	push ax,si
	mov bx,ds
	mov es,bx
	lea bx,bsbuff
	mov al,1
	mov cx,0001h
	mov dx,0080h
	int 13h
	pop si,ax
	jnc bsdisk
	dec si
	jnz bsboot
	jmps bscomp	;Too many errors, abort.
bsdisk	ret


bsexit	db 'FSL: Your disk does not need updating, if FAST/SOFA still does not allow',13,10
	db 'compiling then please ring NZ (04) 399 721 and ask for Peter.$'

bserr	db 'FSL: Serial number is incorrect.$'

bsdone	db 'FSL: All done, unlimited compiling is yours.$'

bsbuff	ds 512

loser1
