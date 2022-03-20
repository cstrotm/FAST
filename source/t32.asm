
;==============================================================================
;test 32 bit loop?
;==============================================================================

	mov eax,ebx
	mov esi,edi

	mul ebx

	mov eax,0000h
	dw 0010h

start	add ebx,ecx
	add esi,edi
	add edi,esi
	sub ebx,ecx
	sub esi,edi
	sub edi,esi

	dec eax
	jnz start

	int 20h 	;stop
