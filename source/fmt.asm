;format disk a?


	mov ax,0509h
	mov bx,offset table
	push cs
	pop es
	mov ch,0
	mov dx,0
	int 13h 	;format?

	mov ah,1
	mov dl,0
	int 13h 	;status?

	int 20h

table	db 0,0,0,2
	db 0,0,1,2
	db 0,0,2,2
	db 0,0,3,2
	db 0,0,4,2
	db 0,0,5,2
	db 0,0,6,2
	db 0,0,7,2
	db 0,0,8,2
