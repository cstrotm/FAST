
;******* RUNTIME MACHINE CODE ENTRY POINT

hfmain	db tf,hfset,xhfmain
	db tf,ptitle,'Main',0
	mov ah,4ah
	mov bx,0
	int 21h 	;FREE MEMORY.

	db tf,trun
	mov al,onetot
	sub bp,4
	call hfncsu
	add bp,2	;Skip over 'INT 21h'
	movb [bp-4],'f' ;See FASTI-ONELP for ONETOT('f')
	lea di,hfmainf
	jmp loop

hfmainf
;	 mov ah,30h	 ;Get DOS version number.
;	 int 21h
;	 db 3dh
;chkdos  dw 2		 ;CMP AX,major+minor*256 DOS version.
;	 jae doshigh
;	 db tf,hcall,xbpmess
;	 db 'Requires DOS '
;dostext db '2.00',7,26
;	 int 20h

doshigh cli
	push cs
	pop ss
	db 0bch,tf,trun ;MOV SP,ENDCODE-1

	mov al,onetot
	call hfncsu
	movw [bp-2],-1	 ;STACK=total_length-1

	lea di,hfmcsp
	jmp loop

hfmcsp	sti
	mov bx,0b800h	;Find base address of screen type and store as scrseg.
	sub cx,cx
	mov es,cx
	cmpb es:[463h],0bah-6
	jnz nmono
	inc cx
	sub bh,8
nmono	mov [scrseg],bx   ;Screen type initialised as either MDA/HGA or CGA.
	mov [monof],cx

	db tf,trun
	cmpb hflags+xhfint,0
	jz mainc2
	lea di,maino1
	jmp loop
maino1	mov ax,3508h
	int 21h 	;Get old int 8 vector.
	mov [oldoff],bx
	mov [oldseg],es   ;Store old int 8 vector.
	mov ax,2508h
	db 0bah,tf,hfuse,xhfint
	int 21h 	;Set new multiplex interupt system into operation.
	db tf,trun

mainc2	cmpb hflags+xikeyit,0
	jz mainc3
	lea di,maino2
	jmp loop
maino2	mov ax,3509h
	int 21h 	;Get old int 9 vector.
	mov [ikeyoff],bx
	mov [ikeyseg],es   ;Store old int 9 vector.
;	mov ax,2509h
;	db 0bah,tf,hfuse,xikeyit
;	int 21h 	;Set new multiplex interupt system into operation.
	db tf,trun

mainc3	lea di,maino3
	jmp loop
maino3	mov ax,3523h
	int 21h
	mov [oldboff],bx
	mov [oldbseg],es	;Get break vector at start.

	db tf,trun

	mov ax,entry		;Start executing USER code.
	jmp nodret		;JMP to start of code after intialisation.

	;HFMAIN END


;...  Multiple interupt system	...
hfint	db tf,hfset,xhfint
	db tf,ptitle,'Interupts',0
	pushf
	callf cs:[oldoff]
	sti

	push ds
	push ax

	push cs
	pop ds

	cmpw [count],0
	jz nosound
	decw [count]
	jnz nosound

	in al,61h
	and al,252	;Cancel the sound.
	out 61h,al

nosound cmpb [ihere],0
	jnz hfcont-1
	movb [ihere],1

	push es
	push bp
	push di
	push si
	push bx
	push cx
	push dx

	mov cl,10
	db 0bfh,tf,hfuse,xtabint
nosiit	push di
	push cx
	mov ax,[di]
	or ax,ax
	jz nointd		;Activate normal INT if desired.
	call ax

nointd	pop cx
	pop di
	inc di
	inc di
	dec cl
	jnz nosiit

	movb [ihere],0

	pop dx
	pop cx
	pop bx
	pop si
	pop di
	pop bp
	pop es

hfcont	pop ax
	pop ds

igbreak db tf,hfset,xigbrek
	iret		;Restore all registers and return from interupt.
	db tf,hfset,xtabint
	db tf,trun
	mov ax,10*2
	jmp csplps

ikeyint db tf,hfset,xikeyit
	db tf,ptitle,'Keyboard Interupts',0
	pushf

	callf cs:[ikeyoff]

	push ds 	;Save all registers.
	push es
	push bp
	push di
	push si
	push bx
	push cx
	push dx
	push ax

	push cs
	pop ds

	mov ax,40h
	mov es,ax
	mov si,es:[1ah]
	cmp si,es:[1ch]
	je nokeys-2
	cli		;No interupts.
	mov ax,es:[si]
	db 0bfh,tf,hfuse,xmitab
	cmp ah,1
	jb noscan
	cmp ah,132
	ja noscan
	mov al,ah
	sub ah,ah
	add di,ax
	add di,ax
	mov ax,[di]
	or ax,ax
	jz noscan
	db tf,hcall,xadvanc
	sti		;Interupts are re-enabled.
	call ax
	jmps nokeys

noscan	cmp al,127
	ja nokeys-1
	sub ah,ah
	db 0bfh,tf,hfuse,xmitab
	add di,ax
	add di,ax
	mov ax,[di]
	or ax,ax
	jz nokeys
	db tf,hcall,xadvanc
	sti
	call ax

nokeys	pop ax
	pop dx
	pop cx
	pop bx
	pop si
	pop di
	pop bp
	pop es
	pop ds
	iret		;Restore all registers and return from interupt.

	db tf,hfset,xmitab
	db tf,trun
	mov ax,522
	jmp csplps
;End ikeyint.


handles db tf,hfset,xhandle
	db tf,ptitle,'File Handles',0
	db tf,trun
	mov ax,48
	jmp csplps

rndb1	equ 0
rndb2	equ 2
rndb3	equ 5
rndb4	equ 7
rndt1	equ rndb1+37
rndt2	equ rndb2+17
rndt3	equ rndb3+1721
rndt4	equ rndb4+101

getrnd	db tf,hfset,xgetrnd
	db tf,ptitle,'Random Numbers',0
	incw [rndn1]
	incw [rndn2]
	incw [rndn3]
	incw [rndn4]
	cmpw [rndn1],rndt1
	jbe gr2
	movw [rndn1],rndb1
gr2	cmpw [rndn2],rndt2
	jbe gr3
	movw [rndn2],rndb2
gr3	cmpw [rndn3],rndt3
	jbe gr4
	movw [rndn3],rndb3
gr4	cmpw [rndn4],rndt4
	jbe grx
	movw [rndn4],rndb4
grx	mov ax,[rndn1]
	add ax,[rndn1]
	mulw [rndn2]
	adc ax,[rndn2]
	mulw [rndn3]
	adc ax,[rndn3]
	mulw [rndn4]
	adc ax,[rndn4]
	ret
	db tf,tend

hfrun	db tf,hfset,xhfrun
	db tf,ptitle,'Default Runtime Error',0
	mov ah,6
	mov dl,7
	int 21h
	db tf,hjump,xexit
	db tf,tend

;on error default beeps & exits, saves space
;	sub ax,ax
;	db tf,hcall,xcpos
;	db tf,hcall,xbpmess
;	db 'Error ',26
;	mov ax,[errorv]
;	db tf,hcall,xhfpb
;	db tf,hcall,xbpmess
;	db ' at ',26
;	pop ax		;Get address off stack.
;	db tf,hcall,xbprinh
;	db tf,hjump,xexit
;	db tf,tend

exit	db tf,hfset,xexit
	db tf,ptitle,'Exit',0
	db tf,trun
	cmpb hflags+xhfint,0
	je exitc2
	lea di,exito1
	call loop
	jmps exitc2
exito1	lds dx,[oldoff]
	mov ax,2508h
	int 21h
	db tf,tend
;	lds dx,[ikeyoff]
;	mov ax,2509h
;	int 21h 	;Reset keyboard interupt.
	db tf,tend

exitc2	mov ax,20cdh	;INT 20h
	jmp lpute2

advance db tf,hfset,xadvanc
	mov si,es:[1ah]
	inc si
	inc si
	cmp si,es:[82h]  ;Restore AX, but remember to advance buffer pointer.
	jne advputx
	mov si,es:[80h]
advputx mov es:[1ah],si
	ret
	db tf,tend

hfloc	db tf,hfset,xhfloc
	db tf,ptitle,'Locate',0
	mov dx,ax
	mov al,[fscreen_cols1]	;Locate AX.
	mul dh
	add ax,ax
	sub dh,dh
	add ax,dx
	add ax,dx
	mov [scrpos],ax
	ret
	db tf,tend

wlocate db tf,hfset,xwlocat
	db tf,ptitle,'Window Locate',0
	mov dx,cx
	mov al,[fscreen_cols1]	     ;Locate AX.
	mul dh
	add ax,ax
	sub dh,dh
	add ax,dx
	add ax,dx
	mov [wscrpos],ax
	ret
	db tf,hfset,xwprt
	mov es,[scrseg]
	mov ah,[wscrcol]
	mov di,[wscrpos]
	mov es:[di],ax
	addw [wscrpos],2	;Update screen position.
	ret
	db tf,hfset,xwpms
wpms	mov al,[si]
	inc si
	cmp al,26
	jne wpcont
	ret
wpcont	cmp al,22
	jne wpcol2
	mov cx,[si]
	inc si
	inc si
	add cx,[wxy]	;CL+=X1, CH+=Y1.
	db tf,hcall,xwlocat
	jmps wpms
wpcol2	cmp al,20
	jne wpmout
	mov al,[si]	;Colour byte.
	mov [wscrcol],al
	inc si
	jmps wpms
wpmout	db tf,hcall,xwprt
	jmps wpms
	db tf,tend

print	db tf,hfset,xprint
	db tf,ptitle,'Print',0
	mov es,[scrseg]
	mov ah,[scrcol]
	mov di,[scrpos]
	cmp di,[fscreen_size]
	jb pspok
	push ax
	db tf,hcall,xtscrl	;Also updates DI and scrpos.
	pop ax
pspok	mov es:[di],ax
	add di,2
	mov [scrpos],di   ;Update screen position.
	ret
	db tf,tend

sprint	db tf,hfset,xsprint
	db tf,ptitle,'Sprint',0
	mov es,[scrseg]
	mov es:[bp],al
	add bp,2
	ret
	db tf,tend

pmeslp	db tf,hfset,xpmessp
	db tf,ptitle,'Print Message',0
	pop si
pmlp2	mov al,[si]
	inc si
	cmp al,26
	jne pcont
	push si
	ret
pcont	db tf,hcall,xprint
	jmps pmlp2
	db tf,tend

spmess	db tf,hfset,xspmess
	db tf,ptitle,'Sprint Message',0
	pop si
	mov es,[scrseg]
spmess2 mov al,[si]
	inc si
	cmp al,26
	jne spcont
	push si
	ret
spcont	mov es:[bp],al
	add bp,2
	jmps spmess2
	db tf,tend

bpmess	db tf,hfset,xbpmess
	db tf,ptitle,'Print bios Message',0
	pop si
bpmeslp mov al,[si]
	inc si
	cmp al,26
	jne bpcont
	push si 	;Return to after code.
	ret
bpcont	db tf,hcall,xbprinb
	jmps bpmeslp
	db tf,tend

lpmess	db tf,hfset,xlpmess
	db tf,ptitle,'Lprint Message',0
	pop si
lpmeslp mov al,[si]
	inc si
	cmp al,26
	jne lpcont
	push si 	;Return to after code.
	ret
lpcont	db tf,hcall,xlpchar
	jmps lpmeslp
	db tf,tend

printb	db tf,hfset,xprintb
	db tf,ptitle,'Print Number',0
	sub ah,ah
printv	db tf,hfset,xhfp
	mov cl,'0'
	mov bx,10000
	db tf,hcall,xpdigv
	mov bx,1000
	db tf,hcall,xpdigv
	mov bx,100
	db tf,hcall,xpdigv
	mov bl,10
	db tf,hcall,xpdigv
	mov bl,1
	sub cl,cl
pdigv	db tf,hfset,xpdigv
	sub dx,dx
	div bx
	add al,48
	push dx
	cmp al,cl
	je pzero
	db tf,hcall,xprint		;Print the digit.
	sub cl,cl
pzero	pop ax
	ret
	db tf,tend

sprintb db tf,hfset,xsprinb
	db tf,ptitle,'Sprint Number',0
	sub ah,ah
sprintv db tf,hfset,xhfsp
	mov cl,'0'
	mov bx,10000
	db tf,hcall,xspdigv
	mov bx,1000
	db tf,hcall,xspdigv
	mov bx,100
	db tf,hcall,xspdigv
	mov bl,10
	db tf,hcall,xspdigv
	mov bl,1
	sub cl,cl
spdigv	db tf,hfset,xspdigv
	sub dx,dx
	div bx
	add al,48
	push dx
	cmp al,cl
	je spzero
	db tf,hcall,xsprint	;Print the digit.
	sub cl,cl
spzero	pop ax
	ret
	db tf,tend

lprintb db tf,hfset,xlprinb
	db tf,ptitle,'Lprint Number',0
	sub ah,ah
lprintv db tf,hfset,xhflp
	mov cl,'0'
	mov bx,10000
	db tf,hcall,xlpdigv
	mov bx,1000
	db tf,hcall,xlpdigv
	mov bx,100
	db tf,hcall,xlpdigv
	mov bl,10
	db tf,hcall,xlpdigv
	mov bl,1
	sub cl,cl
lpdigv	db tf,hfset,xlpdigv
	sub dx,dx
	div bx
	add al,48
	push dx
	cmp al,cl
	je lpzero
	db tf,hcall,xlpchar	;Print the digit.
	sub cl,cl
lpzero	pop ax
	ret
	db tf,tend

bpb	db tf,hfset,xbpb
	db tf,ptitle,'Print bios Number',0
	sub ah,ah
bprintv db tf,hfset,xhfpb
	mov cl,'0'
	mov bx,10000
	db tf,hcall,xbpdigv
	mov bx,1000
	db tf,hcall,xbpdigv
	mov bx,100
	db tf,hcall,xbpdigv
	mov bl,10
	db tf,hcall,xbpdigv
	mov bl,1
	sub cl,cl
bpdigv	db tf,hfset,xbpdigv
	sub dx,dx
	div bx
	add al,48
	push dx
	cmp al,cl
	je bpzero
	db tf,hcall,xbprinb	;Print the digit.
	sub cl,cl
bpzero	pop ax
	ret
	db tf,tend

printh	db tf,hfset,xprinth
	db tf,ptitle,'Print Hex Number',0
	push ax
	mov al,ah
	db tf,hcall,xpdigh
	pop ax
pdigh	db tf,hfset,xpdigh
	push ax
	shr al,1
	shr al,1
	shr al,1
	shr al,1
	db tf,hcall,xdighp
	pop ax
dighp	db tf,hfset,xdighp
	and al,15
	add al,'0'
	cmp al,'9'
	jbe phd
	add al,7
phd	db tf,hjump,xprint
	db tf,tend

sprinth db tf,hfset,xsprinh
	db tf,ptitle,'Sprint Hex Number',0
	push ax
	mov al,ah
	db tf,hcall,xspdigh
	pop ax
spdigh	db tf,hfset,xspdigh
	push ax
	shr al,1
	shr al,1
	shr al,1
	shr al,1
	db tf,hcall,xsdighp
	pop ax
sdighp	db tf,hfset,xsdighp
	and al,15
	add al,'0'
	cmp al,'9'
	jbe sphd
	add al,7
sphd	db tf,hjump,xsprint
	db tf,tend

bprinth db tf,hfset,xbprinh
	db tf,ptitle,'Print bios Hex Number',0
	push ax
	mov al,ah
	db tf,hcall,xbpdigh
	pop ax
bpdigh	db tf,hfset,xbpdigh
	push ax
	shr al,1
	shr al,1
	shr al,1
	shr al,1
	db tf,hcall,xbdighp
	pop ax
bdighp	db tf,hfset,xbdighp
	and al,15
	add al,'0'
	cmp al,'9'
	jbe bprintb
	add al,7
bprintb db tf,hfset,xbprinb
	mov ah,6
	mov dl,al
	int 21h
	ret
	db tf,tend

lprinth db tf,hfset,xlprinh
	db tf,ptitle,'Lprint Hex Number',0
	push ax
	mov al,ah
	db tf,hcall,xlpdigh
	pop ax
lpdigh	db tf,hfset,xlpdigh
	push ax
	shr al,1
	shr al,1
	shr al,1
	shr al,1
	db tf,hcall,xldighp
	pop ax
ldighp	db tf,hfset,xldighp
	and al,15
	add al,'0'
	cmp al,'9'
	jbe lpchar
	add al,7
lpchar	db tf,hfset,xlpchar
	sub dx,dx
	mov ah,dh
	int 17h
	ret
	db tf,tend

testscl db tf,hfset,xtestsc
	db tf,ptitle,'Check Screen Scroll',0
	mov ax,[scrpos]
	add ax,[fscreen_cols2]
	mov bl,[fscreen_cols1]
	shr ax,1
	div bl
	mul bl
	add ax,ax
	cmp ax,[fscreen_size]
	jb testscp-3	;Finish down if no scroll needed else scroll up.
tscrl	db tf,hfset,xtscrl
	mov ax,0601h
	sub cx,cx
	mov dl,[fscreen_cols1]
	dec dl
	mov dh,[fscreen_rows1]
	dec dh
	mov bh,[scrcol]
	int 10h
	mov ax,[fscreen_size]	;Start of bottom line.
	sub ax,[fscreen_cols2]
	mov di,ax
testscp mov [scrpos],ax
	ret
	db tf,tend

sptest	db tf,hfset,xsptest
	db tf,ptitle,'Sprint Screen Scroll',0
	mov ax,bp
	add ax,[fscreen_cols2]
	mov bl,[fscreen_cols1]
	shr ax,1
	div bl
	mul bl
	add ax,ax
	cmp ax,[fscreen_size]
	jb stests	;Finish down if no scroll needed else scroll up.
	mov ax,0601h
	sub cx,cx
	mov dl,[fscreen_cols1]
	dec dl
	mov dh,[fscreen_rows1]
	dec dh
	mov bh,[scrcol]
	int 10h
	mov ax,[fscreen_size]	;Start of bottom line.
	sub ax,[fscreen_cols2]
stests	mov bp,ax
	ret
	db tf,tend

cpos	db tf,hfset,xcpos
	db tf,ptitle,'Set Cursor',0
	mov dx,ax
	sub bh,bh
	mov ah,2
	int 10h 	;Set flashing (real) cursor at DX.
	ret
	db tf,tend


;Sprite routines for the AND operation.
sprite	db tf,hfset,xsprite
	db tf,ptitle,'Sprite Display',0
seprts	cmpb cs:[modew],1	;0=640*200(C) 1=320*200(C) 2=720*348(H)
	jb asp640j
	je asp320
	jmp asp720
asp640j jmp asp640

asp320	call cplace-6-6-3
	mov dx,[di]	;Get size of sprite.
	inc di
	inc di
acsprow push dx
	push si
acspcol mov ax,[di]
	xchg al,ah
	inc di
	inc di
	mov cl,cs:[rotate]
	ror ax,cl

	mov cx,ax
	mov bx,ax
	mov ax,es:[si]
	xchg al,ah
	and ax,cs:[and2]
	and cx,cs:[and1]
	or ax,cx
	xchg al,ah
	mov es:[si],ax
	inc si
	inc si
	mov ax,es:[si]
	xchg al,ah
	and ax,cs:[and1]
	and bx,cs:[and2]
	or ax,bx
	xchg al,ah
	mov es:[si],ax

	dec dl
	jnz acspcol
	pop si
	add si,bp
	mov bp,8192
	jc acspe
	mov bp,-8112
acspe	pop dx
	dec dh
	jnz acsprow
	pop ds
	ret

asp640	call bpos-6-12-3
	mov dx,[di]	;Get size of sprite.
	add di,2
absprow push dx
	push si
abspcol mov ax,[di]
	xchg al,ah
	add di,2
	ror ax,cl	;Rotate number?
	xchg al,ah
	mov dh,al

	mov bl,es:[si]
	and al,bh
	and bl,ch
	or al,bl
	mov es:[si],ax
	add si,2
	and dh,ch
	mov es:[si],dh

	dec dl
	jnz abspcol
	pop si
	add si,bp
	mov bp,8192
	jc abspe
	mov bp,-8112
abspe	pop dx
	dec dh
	jnz absprow
	pop ds
	ret


asp720	call hpos-6-18-3
	mov dx,[di]	;Get size of sprite.
	inc di
	inc di
ahsprow push dx
	push si
ahspcol mov ax,[di]
	xchg al,ah
	inc di
	inc di
	mov cl,cs:[rotate]
	ror al,cl

	mov cx,ax
	mov bx,ax
	mov ax,es:[si]
	xchg al,ah
	and ax,cs:[and2]
	and cx,cs:[and1]
	or ax,cx
	xchg al,ah
	mov es:[si],ax
	inc si
	inc si
	mov ax,es:[si]
	xchg al,ah
	and ax,cs:[and1]
	and bx,cs:[and2]
	or ax,bx
	xchg al,ah
	mov es:[si],ax

	dec dl
	jnz ahspcol
	pop si
	add si,bp
	mov bp,16384
	jc ahspe
	mov bp,-16384
ahspe	pop dx
	dec dh
	jnz ahsprow
	pop ds
	ret

seprtsc jmp seprts

esover	equ 26h

sprites db tf,hfset,xspent
	mov cx,ds
	pop si	;Save address.

	pop ds
	mov di,ax
	pop bx,dx

	push si,cx	;RET DS
	
	mov es,cs:[scrseg]

;DS:DI=Address of sprite data.
;DX=X coordinate of sprite.
;BX=Y coordinate.    

	mov al,cs:[modeop]	;0=AND 1=XOR 2=OR
	cmp al,1
	mov cx,3130h	;XOR
	je modedo
	jb seprtsc+3	;AND
	mov cx,0908h	;OR
modedo	db 2eh,88h,2eh,tf,hfuse,xopm1
	db 2eh,88h,0eh,tf,hfuse,xopm1a
	db 2eh,88h,2eh,tf,hfuse,xopm2
	db 2eh,88h,0eh,tf,hfuse,xopm2a
	db 2eh,88h,2eh,tf,hfuse,xopm3
	db 2eh,88h,0eh,tf,hfuse,xopm3a

	mov cl,cs:[modew]
	cmp cl,1
	jb sp640j
	je sp320
	jmp sp720-12
sp640j	jmp sp640-6

sp320	call cplace-6
	mov dx,[di]	;Get size of sprite.
	inc di
	inc di
csprow	push dx
	push si
cspcol	mov ax,[di]
	xchg al,ah
	inc di
	inc di
	mov cl,cs:[rotate]
	ror ax,cl

	mov bh,ah
	and ax,cs:[and1]
	xchg al,ah
	db esover
opm1	db tf,hfset,xopm1
	xor [si],ax
	inc si
	inc si
	and bh,cs:[and2+1]
	db esover
opm1a	db tf,hfset,xopm1a
	xor [si],bh

	dec dl
	jnz cspcol+6
	pop si
	add si,bp
	mov bp,8192
	jc cspe
	mov bp,-8112
cspe	pop dx
	dec dh
	jnz csprow+6
	pop ds
	ret

cplace	push dx
	sub si,si
	mov bp,8192
	shr bx,1
	jnc cmult
	add si,8192
	mov bp,-8112
cmult	mov cl,2
	shr dx,cl
	add si,dx	;X position.
	mov ax,80
	mul bx
	add si,ax
	pop ax
	and al,3
	add al,al
	mov cs:[rotate],al
	mov dx,-1
	jz cput1
	mov cl,al
	shr dx,cl
cput1	mov cs:[and1],dx
	xor dx,-1
	mov cs:[and2],dx
	ret

sp640	call bpos-6
	mov dx,[di]	;Get size of sprite.
	add di,2
bsprow	push dx
	push si
bspcol	mov ax,[di]
	add di,2
	xchg al,ah
	ror ax,cl	;Rotate?
	xchg al,ah
	mov dh,al

	and al,bh
	db esover
opm2	db tf,hfset,xopm2
	xor [si],ax
	add si,2
	and dh,ch
	db esover
opm2a	db tf,hfset,xopm2a
	xor [si],dh

	dec dl
	jnz bspcol+6
	pop si
	add si,bp
	mov bp,8192
	jc bspe
	mov bp,-8112
bspe	pop dx
	dec dh
	jnz bsprow+6
	pop ds
	ret

bpos	push dx
	sub si,si
	mov bp,8192
	shr bl,1
	jnc bmult
	mov si,bp
	mov bp,-8112
bmult	shr dx,1
	shr dx,1
	shr dx,1
	add si,dx	;X position.
	mov al,80
	mul bl
	add si,ax
	pop cx
	and cl,7	;ROTATE
	mov bh,-1
	shr bh,cl	;AND1
	mov ch,bh	;AND2
	xor ch,-1
	ret

sp720	call hpos-6
	mov dx,[di]	;Get size of sprite.
	inc di
	inc di
hsprow	push dx
	push si
hspcol	mov ax,[di]
	xchg al,ah
	inc di
	inc di
	mov cl,cs:[rotate]
	ror ax,cl

	mov bh,ah
	and ax,cs:[and1]
	xchg al,ah
	db esover
opm3	db tf,hfset,xopm3
	xor [si],ax
	inc si
	inc si
	and bh,cs:[and2+1]
	db esover
opm3a	db tf,hfset,xopm3a
	xor [si],bh

	dec dl
	jnz hspcol+6
	pop si
	add si,bp
	mov bp,16384
	jc hspe
	mov bp,-16384
hspe	pop dx
	dec dh
	jnz hsprow+6
	pop ds
	ret

hpos	push dx
	sub si,si
	mov bp,16384
	shr bx,1
	jnc hmult
	add si,16384
	mov bp,-16384
hmult	mov cl,3
	shr dx,cl
	add si,dx	;X position.
	mov ax,80
	mul bx
	add si,ax
	pop ax
	and al,7
	mov cs:[rotate],al
	mov dx,-1
	jz hput1
	mov cl,al
	shr dx,cl
hput1	mov cs:[and1],dx
	xor dx,-1
	mov cs:[and2],dx
	ret
	db tf,tend


rthit	db tf,hfset,xrthit
	db tf,ptitle,'Sprite Hit Detection',0
	mov [hitc],ax
	mov [hity2],cx

	mov cx,[si]
	sub ah,ah
	mov al,cl
	add ax,ax
	add ax,ax
	add ax,ax
	add ax,ax
	mov [hitw1],ax
	sub ah,ah
	mov al,ch
	mov [hitd1],ax

	mov cx,[di]
	sub ah,ah
	mov al,cl
	add ax,ax
	add ax,ax
	add ax,ax
	add ax,ax
	mov [hitw2],ax
	sub ah,ah
	mov al,ch
	mov [hitd2],ax

	mov ax,bp
	add ax,[hitc]	;AX=X1+C
	mov cx,bx
	add cx,[hitw2]
	sub cx,[hitc]	;CX=X2+W2-C
	cmp ax,cx
	jae hitexit

	mov ax,bp
	add ax,[hitw1]
	sub ax,[hitc]	;AX=X1+W1-C
	add bx,[hitc]	;BX=X2+C
	cmp ax,bx
	jbe hitexit

	mov ax,dx
	add ax,[hitc]	;AX=Y1+C
	mov cx,[hity2]
	add cx,[hitd2]
	sub cx,[hitc]	;CX=Y2+D2-C
	cmp ax,cx
	jae hitexit

	add dx,[hitd1]
	sub dx,[hitc]	;DX=Y1+D1-C
	mov ax,[hity2]
	add ax,[hitc]	;AX=Y2+C
	cmp ax,dx
	jae hitexit

	mov ax,-1	;HIT! All tests were true.
	ret

hitexit sub ax,ax	;At least one test failedax
	ret
	db tf,tend

takey	db tf,hfset,xtakey
	db tf,ptitle,'Get Key',0
	db tf,hcall,xtakchk
	jnz takgetk
	ret
takgetk sub ah,ah
	int 16h
	ret

	db tf,hfset,xtakchk
	mov ah,1
	int 16h
	mov ax,0
	jz tchk2
	dec ax
tchk2	ret
	db tf,tend

inpax	db tf,hfset,xinpax
	db tf,ptitle,'Input Number',0
	db 0bfh,tf,hfuse,xinplen
	db tf,hcall,xgetiii
	inc di
	inc di
	sub ax,ax
	mov dx,ax
inplp	mov cl,[di]
	inc di
	sub cl,'0'
	jc inpret-3
	cmp cl,9
	ja inpret-3
	push cx,di
	sub cx,cx
	mov bx,10
	db tf,hcall,xmul32
	pop di,cx
	sub ch,ch
	add ax,cx
	jnc inplp
	inc dx
	jmps inplp
	db tf,hfset,xinplen
	db 6,0,0,0,0,0,0,0
inpret	ret
	db tf,tend

hinpax	db tf,hfset,xhinpax
	db tf,ptitle,'Input Hex Number',0
	db 0bfh,tf,hfuse,xinplen
	db tf,hcall,xgetiii
	inc di
	inc di
	sub ax,ax
	mov dx,ax
hinplp	mov cl,[di]
	or cl,32
	inc di
	sub cl,'0'
	jc hinpret
	cmp cl,9
	jbe hicont
	sub cl,7+32
	cmp cl,15
	ja hinpret
hicont	push cx,di
	sub cx,cx
	mov bx,16
	db tf,hcall,xmul32
	pop di,cx
	sub ch,ch
	add ax,cx
	jmps hinplp
hinpret ret
	db tf,tend

getiii	db tf,hfset,xgetiii
	db tf,ptitle,'Input String',0
	mov si,di
	mov cl,[di]	;Maximum characters+1 (13).
	mov ch,1	;Characters so far.
	inc si
	inc si
	sub bh,bh	;Display page.
	mov bl,[scrcol]
getilp	push si
	sub ah,ah
	int 16h
	pop si
	cmp al,27
	jne getilx
	mov si,di	;SI=DI+2
	inc si
	inc si
getpe	db 0c6h,4	;MOVB [SI],n
inpnnn	db 13		;Default INPEND
	ret
getilx	cmp al,0dh
	je getiend
	cmp al,8
	je getibk
	cmp ch,cl
	jae getilp
	cmp al,0
	jz getilp
	mov [si],al
	inc si
	inc ch
	mov ah,14
	int 10h
	jmps getilp
getibk	cmp ch,1
	jbe getilp
	mov ax,0e08h
	int 10h
	mov ax,0e20h
	int 10h
	mov ax,0e08h
	int 10h
	dec si
	dec ch
	jmps getilp
getiend dec ch
	mov 1[di],ch	;Put the count of bytes read.
	jmps getpe
	db tf,tend


;WINDOWS operating routines

wdata	db tf,hfset,xwdata
	db tf,ptitle,'Windows',0
	db tf,trun
	mov ax,6*20	;20 windows maximum.
	call csplps
	lea di,wcla	;Close all windows.
	jmp loop

wcla	db tf,hfset,xwclosa
	db tf,ptitle,'Close all Windows',0
wclosea mov al,[windows]
	or al,al
	jz wcar
	db tf,hcall,xwclose
	jmps wclosea
wcar	ret
	db tf,tend

wclose	db tf,hfset,xwclose
	db tf,ptitle,'Close Window',0
	movw [errorv],0
	cmpb [windows],0
	jnz wclok
	mov ax,6	;Invalid handle (to window).
	stc
	ret
wclok	decb [windows]
	db tf,hcall,xwposdi
	mov [wpos],di
	mov si,di
	mov dx,ax
	mov al,[fscreen_cols1]	     ;Locate AX.
	mul dh
	add ax,ax
	xor dh,dh	;0
	add ax,dx
	add ax,dx
	mov di,ax
	mov es,[scrseg]
	cld
	mov bx,cx
	sub ch,ch
wcrow	push di
	mov cl,bl
	rep movsw
	pop di
	add di,[fscreen_cols2]
	dec bh
	jnz wcrow
	ret		;No Carry
	db tf,tend

wopen	db tf,hfset,xwopen
	db tf,ptitle,'Open Window',0
	movw [errorv],0
	cmpb [windows],20
	jne wopen2	;20 windows is the maximum.
	mov ax,4	;No more handles (to windows).
	stc
	ret

wopen2	mov si,ax	;SI is address of window data.
	mov cx,2[si]
	mov [wxy],cx	;Store top left corner in (XS,XY).
	mov dx,4[si]
	mov bh,6[si]
	mov [wscrcol],bh
	push si

;Now store screen image.
	push cx,dx,cx
	db tf,hcall,xwposdi
	pop cx
	incb [windows]
	mov ax,[wpos]
	mov [si],ax
	mov 2[si],cx
	mov al,dl
	sub al,cl
	inc al
	mov ah,dh
	sub ah,ch
	inc ah
	mov 4[si],ax

	mov cx,ax	;Size of window.
	mov dx,2[si]	;Top left.
	mov al,[fscreen_cols1]	    ;Locate AX.
	mul dh
	add ax,ax
	sub dh,dh
	add ax,dx
	add ax,dx
	mov si,ax
	push ds,ds
	pop es
	mov di,[wpos]
	mov ds,[scrseg]
	cld

	push di
	sub ah,ah
	mov al,ch
	mul cl
	add di,ax
	add di,ax
	jc woef-2
	cmp di,1234h
	db tf,tutopia
	jb wook
woef	pop bx,ds,bx,bx,bx
	mov ax,8		;Out of #WINDOW MEMORY!
	stc
	ret
wook	pop di

	push bx
	mov bx,cx
	sub ch,ch
worow	push si
	mov cl,bl
	rep movsw		;Save screen background.
	pop si
	add si,cs:[fscreen_cols2]
	dec bh
	jnz worow
	pop bx			;Restore colour.
	pop ds
	mov [wpos],di

	pop dx,cx

	mov ax,0600h	;Clear the window with the appropriate colour.
	int 10h 	;Assume INT 10 bios db tf,hcall,s destroy SI.
	pop si
	mov dx,[wxy]	;Get the top of the window.
	mov ch,dh
	mov bx,0b8d5h	;Graphics
pl0	mov dx,[wxy]	;Left corner of window.
	mov cl,dl
	db tf,hcall,xwlocat
	mov al,bl
	db tf,hcall,xwprt
	inc cl
pl1	mov al,205
	db tf,hcall,xwprt
	inc cl
	cmp cl,4[si]
	jb pl1	;Wait until finished row.
	mov al,bh	;Print appropriate corner piece.
	db tf,hcall,xwprt
	cmp ch,5[si]
	je plend
	cmp ch,3[si]
	jne plast
	cmpb [si],0
	je plast
	add ch,2
	mov bx,0b5c6h
	jmps pl0
plast	mov ch,5[si]
	mov bx,0bed4h
	jmps pl0
plend	mov cx,[wxy]
	inc ch		;Start printing the down rows on each side.
	db tf,hcall,xwlocat
	mov di,ax
	mov bl,4[si]
	sub bl,2[si]
	add bl,bl
	sub bh,bh
	mov al,179
	mov ah,[wscrcol]
	mov es:[di],ax
	db 26h,89h,1	;mov es:[bx+di],ax
	inc ch		;This stops from printing one line down too far.
	cmp ch,5[si]
	jae pldot
	cmpb [si],0
	jz pldown
	add di,[fscreen_cols2]	    ;Skip one line because of the menu line
	inc ch

pldown	add di,[fscreen_cols2]
	mov es:[di],ax
	db 26h,89h,1	;mov es:[bx+di],ax
	inc ch
	cmp ch,5[si]
	jb pldown	;Print until at bottom of window.

pldot	push si
	add si,7
	db tf,hcall,xwpms	;Print remaining data in window.
	pop si
	cmpb [si],0
	mov dh,0
	jnz exmenu-3	;Is this screen a menu, if it is then display bar.
nomenuz ret
exmsel	db tf,hfset,xexmsel
	mov si,ax
	dec dh
	jns exmenu
	inc dh
exmenu	mov dl,3[si]
	inc dl
	inc dl		;Y position for bar is top+3
	inc dl
	mov bl,1[si]	;BL contains number of options.
	cmp bl,0
	jz nomenuz+3
	mov [options],bl
	mov al,6[si]
	mov [wscrcol],al
	mov al,2[si]
	inc al
	mov [eoff1],al	;Start x for bar.
	mov ah,4[si]
	sub ah,al
	mov [width2],ah ;Width of the highlight bar.

ekey	call putbar
	sub ah,ah
	int 16h
	cmp al,13
	jne noexms
ekfin	mov al,dh
	inc al
	sub ah,ah
	ret
noexms	cmp al,27
	je eexit
	cmp ah,72
	je eup
	cmp ah,80
	je edown
	call klkey
	jmps ekeymn

eexit	sub ax,ax	;If ESC pushed then exit with option 0.
	ret

eup	call resbar
	dec dh
	jns ekey
	mov dh,[options]
	dec dh
	jmp ekey

edown	call resbar
	inc dh
	cmp dh,[options]
	jb ekey
	sub dh,dh
	jmp ekey

ekeymn	push ax
	call resbar
	pop ax
	mov cl,[options]
ekeylp	push cx
	push ax
	inc dh
	cmp dh,[options]
	jb ekldn
	sub dh,dh
ekldn	call resbar
	mov al,es:[di]
	call klkey
	mov bh,al
	pop ax
	cmp al,bh
	je ekyuse
	push ax
	mov al,es:2[di]
	call klkey
	mov bh,al
	pop ax
	cmp al,bh
	je ekyuse
	pop cx
	dec cl
	jnz ekeylp
	jmp ekey
ekyuse	;call putbar
	pop cx
	cmpb [si],1
	jne ekjje
	jmp ekfin
ekjje	jmp ekey

putbar	mov al,[wscrcol]
	push ax
	movb [wscrcol],112	;Black on white.
	call resbar
	pop ax
	mov [wscrcol],al
	ret
resbar	mov ch,dh
	add ch,dl
	mov cl,[eoff1]
	push dx
	db tf,hcall,xwlocat
	pop dx
	mov di,ax
	mov es,[scrseg]
	mov al,[wscrcol]
	mov cl,[width2]
	sub ch,ch
	push di
elpb	mov es:1[di],al
	inc di
	inc di
	loop elpb
	pop di
	ret

klkey	cmp al,'A'
	jb wpkret
	cmp al,'Z'
	ja wpkret
	or al,32
wpkret	ret

	db tf,hfset,xwposdi
wposdi	mov al,[windows]
	add al,al
	sub ah,ah
	db 0beh,tf,hfuse,xwdata
	add si,ax
	add si,ax
	add si,ax
	mov di,[si]
	mov ax,2[si]
	mov cx,4[si]
	ret
	db tf,tend		;end WOPEN.

ctrace	db tf,hfset,xhftrc
	db tf,ptitle,'Default On Trace',0
	ret
	db tf,tend

cdebug	db tf,hfset,xhfdbug
	db tf,ptitle,'Default On Debug',0
	mov ah,0bh
	int 21h
	ret
	db tf,tend

hmove	db tf,hfset,xmove
	db tf,ptitle,'Move Bytes/Words',0
	pop dx		;Return address.
	mov bx,ds

	mov di,ax
	pop es
	pop si
	pop ds
	pop cx

	pushf	;Save byte/word (in carry).
	or cx,cx
	jnz movcont	;MOVE 0 bytes/words.
	popf
	jmps movret

movcont mov ax,es
	mov bp,ds
	cmp ax,bp
	jne movup	;If both segments are different then move up always.
	cmp di,si
	jb movup	;Below? The deleting memory (ie move up is ok).

	mov bp,cx
	dec cx		;Add one less.
	popf
	pushf
	jnc movelcx
	add cx,cx	;Two bytes per CX if words.
movelcx add di,cx
	add si,cx
	mov cx,bp
	popf
	std
	jmps movbw
movup	popf
	cld
movbw	jnc movb	;Moving bytes?
	rep movsw
	jmps movret
movb	rep movsb
movret	mov ds,bx
	jmp dx
	db tf,tend

hline	db tf,hfset,xline
	db tf,ptitle,'Draw Line',0
	pop bx
	pop [newx]
	mov [newy],ax
	pop [lasty]
	pop [lastx]
	push bx 	;Save return address (haven't got any spare registers)

	movb [dlow],0

	mov ax,[newx]
	sub ax,[lastx]
	jns dnly
	neg ax
dnly	mov bx,[newy]
	sub bx,[lasty]
	jns dnnn
	neg bx
dnnn	cmp ax,bx
	mov cx,ax
	jl liney
linex	call lsteps
	mov ax,[newy]
	sub ax,[lasty]
	mov bx,256
	imul bx
	mov bx,[newx]
	sub bx,[lastx]
	jns ldx
	neg bx
ldx	idiv bx
	jns lnsx
	neg al
lnsx	mov [idec],al

dloopx	mov cx,[lastx]
	mov dx,[lasty]
	call plotdot
	;jc lineerr
	mov ax,[xstep]
	add [lastx],ax
	mov ax,[newx]
	cmp ax,[lastx]
	je lineok
	mov al,[idec]
	add [dlow],al
	jnc dloopx
	mov ax,[ystep]
	add [lasty],ax
	jmps dloopx

lineok	clc
	ret
lineerr stc
	ret

liney	call lsteps
	mov ax,[newx]
	sub ax,[lastx]
	mov bx,256
	imul bx
	mov bx,[newy]
	sub bx,[lasty]
	jns ldy
	neg bx
ldy	idiv bx
	jns lnsy
	neg al
lnsy	mov [idec],al

dloopy	mov cx,[lastx]
	mov dx,[lasty]
	call plotdot
	;jc lineerr
	mov ax,[ystep]
	add [lasty],ax
	mov ax,[newy]
	cmp ax,[lasty]
lineok2 je lineok
	mov al,[idec]
	add [dlow],al
	jnc dloopy
	mov ax,[xstep]
	add [lastx],ax
	jmps dloopy

plotdot mov ax,0c0fh
	sub bh,bh
	int 10h
	ret

lsteps	mov ax,[newx]
	cmp ax,[lastx]
	je lineok
	mov ax,1
	jg putstx
	dec ax
	dec ax
putstx	mov [xstep],ax
	mov ax,[newy]
	cmp ax,[lasty]
	je lineok2
	mov ax,1
	jg putsty
	dec ax
	dec ax
putsty	mov [ystep],ax
	ret

	db tf,tend


;FAST. More LOOP stuff for printing.

;Print sub-command table of tricks.
prttab	db 3,'chr'
	dw offset(pchr)
	db 3,'tab'
	dw offset(ptab)
	db 1,'"'
	dw offset(pquote)
	db 1,','
	dw offset(pcomma)
	db 2,'cr'
	dw offset(prtcr)
	db 255

prtcr	db tf,hcall,xtestsc,tf,tend

pchr	db tf,texp,tf,trun
	mov cl,xprint
	jmp ccwlib

pcomma	andw [scrpos],0ffe0h
	addw [scrpos],32
	db tf,tend

ptab	andw [scrpos],0fff0h
	addw [scrpos],16
	db tf,tend

pquote	db tf,trun
	mov cl,xpmessp
quotec	call ccwlib
	mov si,spoint
pqlp	mov al,[si]
	cmp al,'"'
	je pqend
	cmp al,13	;EOL?
	je pqnot
	cmp al,9
	jne pqtabs
	call spctab
	jc pqnot
	inc si
	jmps pqlp
pqtabs	mov [bp],al
	inc bp
	inc si
	jmps pqlp
pqend	inc si
	mov spoint,si
	mov al,26	;End of string marker.
	jmp lput
pqnot	mov al,11	;Missing " or '
	mov spoint,si
	stc
	ret


;Super fast print sub-command table of tricks.
sprttab db 3,'chr'
	dw offset(spchr)
	db 3,'tab'
	dw offset(sptab)
	db 1,'"'
	dw offset(spquote)
	db 1,','
	dw offset(spcomma)

	db 255

spcomma addw bp,32
	andw bp,0ffe0h
	db tf,tend

sptab	addw bp,16
	andw bp,0fff8h
	db tf,tend

spchr	db tf,texp
	mov es,[scrseg]
	mov es:[bp],al
	add bp,2
	db tf,tend

spquote db tf,trun
	mov cl,xspmess
	jmp quotec


;Print BIOS sub-command table of tricks.
bprttab db 3,'chr'
	dw offset(bpchr)
	db 3,'tab'
	dw offset(bptab)
	db 1,'"'
	dw offset(bpquote)
	db 2,'lf'
	dw offset(bioslf)
	db 2,'cr'
	dw offset(bioscr)
	db 255

bpchr	db tf,texp,tf,trun
bpcccc	mov cl,xbprinb
	jmp ccwlib

bioscr	mov al,13
	db tf,hcall,xbprinb,tf,tend

bioslf	mov al,10
	db tf,hcall,xbprinb,tf,tend

bptab	mov al,9
	db tf,trun
	jmps bpcccc

bpquote db tf,trun
	mov cl,xbpmess
	jmp quotec


;LPRINT Command table of tricks.
lprttab db 3,'chr'
	dw offset(lpchr)
	db 1,'"'
	dw offset(lpquote)
	db 2,'lf'
	dw offset(lplf)
	db 2,'cr'
	dw offset(lpcr)
	db 2,'ff'
	dw offset(lpff)
	db 255

lplf	mov al,10
	db tf,hcall,xlpchar,tf,tend

lpcr	mov al,13
	db tf,hcall,xlpchar,tf,tend

lpff	mov al,12
	db tf,hcall,xlpchar,tf,tend

lpchr	db tf,texp,tf,trun
	mov cl,xlpchar
	jmp ccwlib

lpquote db tf,trun
	mov cl,xlpmess
	jmp quotec


cldir	db tf,hfset,xhdir
	db tf,ptitle,'Directory',0
	sub bx,bx
	add ax,2
	push ax,es,ax,bx
	mov ah,4eh	;Find first name.
	int 21h
	jmps dirnext

dirloop push es,di,bx
	mov ah,4fh	;Get next file
	int 21h
dirnext jc dirend
	mov ah,2fh	;Get DTA ES:BX
	int 21h
	mov si,bx
	add si,30
	mov ax,es	;Source = DS:SI
	mov ds,ax
	pop bx,di,es
	inc bx		;Count of files.
	mov cx,13
	cld
	rep	movsb	;Move the name and zero. ASCIIZ
	jmps dirloop
dirend	db tf,terrs	;Set ERROR number.
	pop bx,di,es
	pop di
	mov es:[di-2],bx
	ret
	db tf,tend

hmul32	db tf,hfset,xmul32	;DX:AX * CX:BX
	db tf,ptitle,'32 Bit Multiply',0
emul32	push ax,cx,dx
	mul bx			;    BA
	mov di,dx		;   BD
	mov si,ax		;   CA
				;  ----
	pop ax	;DX		   XXXX
	mul bx
	add di,ax

	pop cx,ax
	mul cx
	jc mul32c
	add di,ax
	jc mul32c

	mov ax,si
	mov dx,di
mul32c	ret
	db tf,tend

;== 32 bit division from PROGAID.EXE =======================================

;DX:AX = DX:AX / CX:BX

hdiv32	db tf,hfset,xdiv32
	db tf,ptitle,'32 Bit Divide',0
ediv32	push	bp
	mov	bp,sp

	mov	ax,[bp+0Ah]
	or	ax,ax				; Zero ?
	jnz	loc_431 			; Jump if not zero
	mov	cx,[bp+8]
	mov	ax,[bp+6]
	xor	dx,dx				; Zero register
	div	cx				; ax,dx rem=dx:ax/reg
	mov	bx,ax
	mov	ax,[bp+4]
	div	cx				; ax,dx rem=dx:ax/reg
	mov	dx,bx
	jmps	loc_435

loc_431 mov	bx,ax
	mov	cx,[bp+8]
	mov	dx,[bp+6]
	mov	ax,[bp+4]
loc_432 shr	bx,1				; Shift w/zeros fill
	rcr	cx,1				; Rotate thru carry
	shr	dx,1				; Shift w/zeros fill
	rcr	ax,1				; Rotate thru carry
	or	bx,bx				; Zero ?
	jnz	loc_432 			; Jump if not zero
	div	cx				; ax,dx rem=dx:ax/reg
	mov	si,ax
	xor	dx,dx				; Zero register
	push	dx
	push	ax
	push	[bp+0Ah]
	push	[bp+8]
	call	sub_136
	cmp	dx,[bp+6]
	ja	loc_433 			; Jump if above
	jc	loc_434 			; Jump if carry Set
	cmp	ax,[bp+4]
	jbe	loc_434 			; Jump if below or =
loc_433 sub	si,1
loc_434 xor	dx,dx				; Zero register
	mov	ax,si

loc_435 mov	sp,bp
	pop	bp
	ret	8
  
sub_136 push	bp
	mov	bp,sp
	mov	ax,[bp+6]
	mov	bx,[bp+0Ah]
	or	bx,ax
	mov	bx,[bp+8]
	jnz	loc_437 			; Jump if not zero
	mov	ax,[bp+4]
	mul	bx				; dx:ax = reg * ax
	mov	sp,bp
	pop	bp
	ret	8

loc_437 mul	bx				; dx:ax = reg * ax
	mov	cx,ax
	mov	ax,[bp+4]
	mulw	[bp+0Ah]			; ax = data * ax
	add	cx,ax
	mov	ax,[bp+4]
	mul	bx				; dx:ax = reg * ax
	add	dx,cx
	mov	sp,bp
	pop	bp
	ret	8

	db tf,tend

;== end of 32 bit division =================================================

hpadd	db tf,hfset,xpadd	;Entry	 BX = y
	db tf,ptitle,'Calculate Pixel Address',0
	mov es,[scrseg] 	;	 DX = x
	sub si,si
				;Returns AH = mask
				;	 AL = colour
				;	 SI = address
				;	 ES = segment

hpcalc	cmpb [modew],1	;0=640*200(C) 1=320*200(C) 2=720*348(H)
	ja pa720j
	je pa320	;BL=y (ignore BH)

	mov cl,dl
	shr bl,1
	jnc p0mult
	mov si,8192
p0mult	shr dx,1
	shr dx,1
	shr dx,1
	add si,dx	;X position.
	mov al,80
	mul bl
	add si,ax
	and cl,7
	inc cl
	mov al,[scrcol]
	mov ah,0feh
	ror ah,cl
	ror al,cl
	ret

pa720j	jmps pa720

pa320	mov ch,dl
	shr bx,1
	jnc p1mult
	mov si,8192
p1mult	mov cl,2
	shr dx,cl
	add si,dx	;X position.
	inc cl
	inc cl
	shl bx,cl
	mov ax,bx
	shl bx,1
	shl bx,1
	add si,bx
	add si,ax
	and ch,3
	mov al,[scrcol]
	mov ah,0fch
	inc ch
	mov cl,ch
	add cl,cl
	ror al,cl
	ror ah,cl
	ret

pa720	mov ch,dl
	shr bx,1
	jnc p2mult
	mov si,16384
p2mult	mov cl,3
	shr dx,cl
	add si,dx	;X position.
	mov ax,90
	mul bx
	add si,ax
	and ch,7
	inc ch
	mov cl,ch
	mov al,[scrcol]
	mov ah,0feh
	ror al,cl
	ror ah,cl
	ret
	db tf,tend

hplot	db tf,hfset,xplot
	db tf,ptitle,'Plot Pixel',0
	db tf,hcall,xpadd
	and es:[si],ah
	not ah
	and al,ah
	or es:[si],al
	ret
	db tf,tend

hpoint	db tf,hfset,xpoint
	db tf,ptitle,'Point Pixel',0
	db tf,hcall,xpadd
	mov al,es:[si]
	not ah
	and al,ah
	rol al,cl
	sub ah,ah
	ret
	db tf,tend

ermsg	db tf,hfset,xermsg
	db tf,ptitle,'Error Message Display',0
	mov ah,48h
	mov bx,5120/16
	push dx
	int 21h 	;Allocate 5k
	mov es,ax
	pop dx
	jc erm_en1

	mov ax,3d00h
	int 21h		;Open file.
	jc erm_en2

	push ax		;Save handle.
	mov bx,ax	;BX=handle
	mov ah,3fh
	mov cx,5120
	push ds
	push es
	pop ds		;DS=segment.
	sub dx,dx	;Offset.
	int 21h 	;Read error message file.
	pop ds		;DS
	pop bx		;Handle.
	jc erm_en2

	push ax		;Count of bytes read.
	mov ah,3eh
	int 21h		;Close the file.
	pop cx		;Restore count.
	jc erm_en2

	sub di,di	;Start search at ES:0000
erm_lop cmp di,cx	;Don't search more than loaded bytes.
	mov ax,87	;Invalid parameter/error!
	jae erm_en2
	inc di
	cmpb es:[di-1],0
	jne erm_lop	;Wait for byte 0.
	mov ax,[errorv]
	add di,2
	cmp es:[di-2],ax
	je erm_use
	jmps erm_lop	;Not the correct error.

erm_use	mov al,es:[di]
	cmp di,cx	;Don't print more than loaded bytes.
	jae erm_x
	or al,al
	je erm_x
	db tf,hcall,xbprinb
	inc di
	jmps erm_use

erm_en2 push ax
	mov ah,49h
	int 21h 	;Free Allocated memory.
	pop ax
erm_en1 push ax
	db tf,hcall,xbpmess
	db 'Error ',26
	mov ax,[errorv]
	db tf,hcall,xhfpb
	db tf,hcall,xbpmess
	db ' [DOS=',26
	pop ax		;6et DOS error.
	db tf,hcall,xhfpb
	mov al,']'
	db tf,hcall,xbprinb
erm_x	mov ah,49h
	int 21h
	ret
	db tf,tend

tptm	db tf,hfset,xprm	;PRINTM
	db tf,ptitle,'Print Memory',0

	db 88h,2eh,tf,hfuse,pma1	;MOV [lab],CH
	db 88h,2eh,tf,hfuse,pma2	;MOV [lab],CH
	db 88h,2eh,tf,hfuse,pma3	;MOV [lab],CH

	pop bx
	mov ax,ds
	pop si,ds
	push bx,ax

	mov es,cs:[scrseg]
	mov di,cs:[scrpos]	;ES:[DI] = current screen pos.
	sub ch,ch
	mov ah,cs:[scrcol]
cpml	mov al,[si]
	cmp al,26
	je cpmf-6
	inc si
	cmp al,13
	je cpmf-6 		;End of line?
	cmp al,0
	je cpmf-6 		;End of line?
	cmp al,9
	jne cpmp-3
	mov bl,ch
	and bl,248
	add bl,8
	sub bl,ch	;Count of spaces to print.
	mov al,' '
cpmt	db esover
	db tf,hfset,pma1
	db 89h,5
	add di,2
	inc ch
	dec cl
	jz cpme-6 	;Check end?
	dec bl
	jnz cpmt+3	;Check count of spaces?
	jmps cpml+3

cpmp	db esover
	db tf,hfset,pma2
	db 89h,5		;Place character on screen.
	inc ch
	add di,2
	dec cl
	jnz cpml+6
	jmps cpme-3

cpmf	mov al,' '
cpmfs	db esover
	db tf,hfset,pma3
	db 89h,5		;Space fill to end of line.
	add di,2
	dec cl
	jnz cpmfs+3

cpme	cmpb [si],10
	jne cpmelf
	inc si
cpmelf	mov ax,si
	pop ds
	ret
	db tf,tend

seek	db tf,hfset,xseek
	db tf,ptitle,'File Seek',0
	mov cx,dx
	mov dx,ax
	mov ax,4200h
	int 21h
	db tf,terrs
	ret
	db tf,tend

ehon	db tf,hfset,xehon
	db tf,ptitle,'Error Handling On',0
	push ax
	jc ehons
	sub ax,ax
ehons	mov [errorv],ax
	or ax,ax
	jz ehonr
	pop bx		;Drop AX.
	jmp [runerra]
ehonr	pop ax
	ret
	db tf,tend

ehoff	db tf,hfset,xehoff
	db tf,ptitle,'Error Handling Off',0
	push ax
	jc ehoffs
	sub ax,ax
ehoffs	mov [errorv],ax
	pop ax
	ret
	db tf,tend

herc	db tf,hfset,xherc
	db tf,ptitle,'Set Hercules - Graphics',0
	db 0bfh,tf,hfuse,xhcode ;MOV DI,HCODE
	mov dx,3bfh
	mov al,3
	out dx,al

	mov dl,0b8h
	mov al,2
	out dx,al	    ;OUT control,disable
	sub ah,ah
hloop	mov al,ah
	mov dx,3b4h
	mov bl,cs:[di]
	out dx,al	    ;OUT index,register#
	inc dl
	mov al,cs:[di]
	out dx,al	    ;OUT port,data
	inc di
	inc ah
	cmp ah,16
	jb hloop

	mov dx,3b8h
	mov al,0ah
	out dx,al	    ;OUT control,enable
	ret

	db tf,hfset,xhcode
	db 35h,2dh,2eh,7,5bh,2,57h,57h,2,3,0,0,0,0,0,0
	db tf,tend

hfind	db tf,hfset,xfind
	db tf,ptitle,'Binary Find',0
	cld
	movw [fbpvar],1
	movw [hitw1],0	;Low=0
	mov ax,[hitd1]
	mov [hitw2],ax	;High=entries
	or ax,ax
	jnz hwhile	;If no records then return 0.
	ret

hwhile	mov bx,[hitw1]
	cmp bx,ax
	jg hwend	;While low<=high

	sub ax,bx
	shr ax,1
	add ax,bx	;mid=(high-low)/2+low

	cmp ax,[hitd1]
	jl hfless	;If mid>entries then find_bin_put=entries+1
	mov ax,[hitd1]
	inc ax
	mov [fbpvar],ax
	jmps hfrz	;Return 0

hfless	push di,cx
	push ax
	mulw [hitd2]
	add di,ax	;di=adr+mid*len
	pop ax

	mov si,[hitc]
	repe cmpsb	;Compare bin_cmp from bin_str with seg|bin_m
	pop cx,di
	je hfrm1	;If found then return mid+1

	ja low_m1

	dec ax
	mov [hitw2],ax
	jmps hwhile

low_m1	inc ax
	mov [hitw1],ax
	mov ax,[hitw2]	;AX=high for while loop.
	jmps hwhile

hfrm1	inc ax
	ret

hwend	cmp bx,[hitd1]
	jle hvl1	;If low>entries then find_bin_put=low-1
	dec bx
hvc2	mov [fbpvar],bx
hfrz	sub ax,ax
	ret
hvl1	inc bx		;else find_bin_put=low+1
	jmps hvc2

	db tf,tend

;== Serial Communications ==================================================

hsinit	db tf,hfset,xsinit
	db tf,ptitle,'Initialise Serial Communications',0
	or al,al
	jz nosin
	sub ah,ah
	mov dx,[com_port]
	int 14h 	;Initialise serial port if desired.

nosin	db 0bah 		;mov dx,XSINT
	db tf,hfuse,xsint
	mov ah,25h
	mov al,[com_irq]

;removed the add 8 (updated all fast source which sets the IRQ) 09-06-97
;	add al,8	;set irq for com1=4 or com2=3 (0ch,0bh)

	int 21h

	mov dx,[com_base]

	inc dl			;3f9
	mov al,1		;interupt on data ready
	out dx,al
	dec dl

	in al,21h
	and al,[com_enable]	;unmask com1 or com2
	out 21h,al

	add dl,4		;3fc
	in al,dx
	or al,1011b
	out dx,al		;enable interupts
	sub dl,4

	mov dx,dx		;delay, was 3f8
	in al,dx		;clear pending data
	in al,dx
	in al,dx

#if 0	;don't use this code - not updated 22-05-96
	mov dh,dh
	mov dl,0fdh	;Reset 8250 controller?
	in al,dx	;Dummy: Line Status
	mov dh,dh
	mov dl,0f8h
	in al,dx	;Dummy: Receive Data
	mov dh,dh
	mov dl,0fah
	in al,dx	;Dummy: Interupt Identification
	mov dh,dh
	mov dl,0feh
	in al,dx	;Dummy: Modem Status
#endif

	ret

	db tf,hfset,xsstop
	db tf,ptitle,'Disable Communications',0
	mov dx,[com_base]

	add dl,4		;3fch
	in al,dx
	and al,11110111b	;disable interupts
	out dx,al
	sub dl,4

	in al,21h
	mov al,[com_enable]
	xor al,255
	or al,00010000b 	;mask com1
	out 21h,al

	inc dl			;3f9h
	mov al,0		;no interupt
	out dx,al

	ret

	db tf,hfset,xsint
	push ax,ds,dx,di

	mov dx,cs:[com_base]	;RX buffer
	in al,dx

	mov ds,cs:[ser_seg]
	mov di,cs:[ser_hed]
	mov [di],al		;Put byte.
	inc di
	cmp di,cs:[ser_siz]
	jb xsinb
	sub di,di
xsinb	mov cs:[ser_hed],di	;Update head.

	mov al,20h
	out 20h,al		;Interupt is over.
	pop di,dx,ds,ax
	iret
	db tf,tend

;== 32*32 = 64 bit multiply ================================================

hmul64	db tf,hfset,xmul64		;cx:bx * dx:ax = @ds:di
	db tf,ptitle,'64 Bit Multiply',0
	mov [hitw1],dx
	mov [hitw2],ax
	mul bx		;create partial products
	mov [hitd1],dx
	mov [di],ax	;hitd2
	mov ax,[hitw1]
	mul bx
	mov [lastx],dx
	mov [lasty],ax
	mov ax,[hitw2]
	mul cx
	mov [hitc],dx
	mov [and1],ax
	mov ax,[hitw1]
	mul cx
	mov [newx],dx
	mov [newy],ax

	;mov ax,[hitd2] ;[di] already
	mov bx,[hitd1]	;add partial products
	add bx,[lasty]
	mov cx,[lastx]
	adc cx,[hitc]
	mov dx,[newx]
	adc dx,0
	adc bx,[and1]
	adc cx,[newy]
	adc dx,0

;	mov [di],ax	;store result (ax done already)
	mov [di+2],bx
	mov [di+4],cx
	mov [di+6],dx
	ret
	db tf,tend

;== 64 bit division ========================================================

hdiv64	db tf,hfset,xdiv64
	db tf,ptitle,'64 Bit Divide',0
	mov cx,dx
	mov bx,ax
	lea di,newx
	movw [di],0
	movw [di+2],0
	movw [di+4],0
	movw [di+6],0

	cmpw [si+4],0
	jnz fulldiv
	cmpw [si+6],0
	jnz fulldiv

	cmpw [si+2],0
	jnz halfdiv
	or cx,cx
	jnz halfdiv
	or bx,bx
	jz div3ret

	mov ax,[si]
	xor dx,dx

	div bx			;dx:ax = dx:ax / bx
	mov [si],ax
	mov [di],dx		;remainder
div3ret ret

halfdiv
	#if i386
	mov eax,[si]

	push cx
	push bx
	pop ebx

	or ebx,ebx
	jnz sd386

	mov [si],ebx
	mov [di],ebx
	jmps sz3next

sd386	div ebx

	mov [si],eax		;quotient
	mov [di],edx		;remainder
sz3next
	#endif

	#if i86

	push si
	push cx
	push bx
	push [si+2]
	push [si]
	db tf,hcall,xdiv32
	pop si
	mov [si],ax		;result
	mov [si+2],dx
	mov [di],si		;remainder
	mov [di+2],di

	#endif

	ret

fulldiv movb [rotate],64	;bit counter
	clc
d64s	rclw [si],1
	rclw [si+2],1
	rclw [si+4],1
	rclw [si+6],1
	mov ax,[di]
	adc [di],ax
	mov ax,[di+2]
	adc [di+2],ax
	mov ax,[di+4]
	adc [di+4],ax
	mov ax,[di+6]
	adc [di+6],ax
	push [di]
	push [di+2]
	push [di+4]
	push [di+6]

	sub [di+2],cx
	jnc d642
	subw [di+4],1
	sbbw [di+6],0
	jc d64x

d642	sub [di],bx
	jnc d64y

	subw [di+2],1
	sbbw [di+4],0
	sbbw [di+6],0
	jc d64x

d64y	pop bp
	pop bp
	pop bp
	pop bp
	stc
d64dec	decb [rotate]
	jnz d64s
	rclw [si],1
	rclw [si+2],1
	rclw [si+4],1
	rclw [si+6],1
	ret

d64x	pop [di+6]
	pop [di+4]
	pop [di+2]
	pop [di]
	clc
	jmps d64dec
	db tf,tend

;== Virus Warning ==========================================================

hprotek db tf,hfset,xprotek
	db tf,ptitle,'Virus Protection',0
	mov ax,[101h]
	inc ah
	db 0bfh,tf,hfuse,xprotek	;MOV DI,offset(HPROTEK)
	cmp ax,di
	jb hnprone

	db tf,hcall,xbpmess
	db 'Warning, this program''s size has been altered!',7,7,1ah

hnprone ret
	db tf,tend

;== 64 bit print number =======================================================

;ax=length
;di=print address
;bp=64 bit number address

hprn64	db tf,hfset,xprn64
	db tf,ptitle,'Print 64 bit Number',0

	cld
	push ds
	pop es

	mov cx,ax
	mov al,' '
	repne stosb		;fill work area with spaces

p64lp	cmpw [bp+6],0		;is number > 9?
	jnz p64big
	cmpw [bp+4],0
	jnz p64big
	cmpw [bp+2],0
	jnz p64big
	cmpw [bp],9
	ja p64big
	jmps p64end

	#if i386
p64big	push di

	mov si,bp
	xor dx,dx
	mov ax,10
	db tf,hcall,xdiv64	;n=n/10
	mov al,[di]		;remainder

p64rr	pop di
	#endif

	#if i86
p64big	push di
	mov si,bp
	mov di,offset(hitw1)
	mov cx,4
	rep movsw		;store original number

	mov si,bp
	xor dx,dx
	mov ax,10
	push bp
	db tf,hcall,xdiv64	;n=n/10
	pop bp

	mov si,offset(newx)	;clear work area
	movw [si],0
	movw [si+2],0
	movw [si+4],0
	movw [si+6],0

	mov bx,10		;work=n*10
	mov ax,[bp]
	mul bx
	mov [si],ax
	mov [si+2],dx

	mov ax,[bp+2]
	mul bx
	add [si+2],ax
	adc [si+4],dx

	mov ax,[bp+4]
	mul bx
	add [si+4],ax
	adc [si+6],dx

	mov ax,[bp+6]
	mul bx
	add [si+6],ax

	mov ax,[hitw1]		;original - (n/10*10)
	mov dx,[hitw1+2]
	sub ax,[newx]
	sbb dx,[newx+2]
	jnc p64rr
	xor ax,0ffffh
	xor dx,0ffffh

p64rr	pop di
	#endif

	call p64one

	jmp p64lp

p64end	mov al,[bp]		;end - print last digit
p64one	dec di			;print digit AL
	add al,'0'
	mov [di],al
	ret
	db tf,tend

;== quick_sort ================================================================

;requires 80386 !!!

;hitw1 = compare function
;hitw2 = exchange function

hqsort		db tf,hfset,xqsort
		db tf,ptitle,'Quick_Sort',0

		pop si		;return address

		pop di		;compare function
		pop ecx 	;number of elements

		push si 	;return address!!!

		mov hitw1,di	;compare
		mov hitw2,ax	;ax=exchange function

		xor edx,edx	;pivotp=0
;--------------
qsorthelp	mov [pivot32],edx
		mov [nelem32],ecx

tailrecursion	db 66h,81h,0f9h,2,0,0,0 ;cmp ecx,2	;ecx,edx!!!
		ja qsortlarge

		jne qs2_return

		mov [qs_left],edx	;pivot
		inc edx
		mov [qs_right],edx	;right
		call [hitw1]		;compare(pivot,right)

		cmp ax,1
		jne qs2_return

		jmp [hitw2]		;exchange(pivot,right)

qs2_return	ret
;--------------
qsortlarge	mov ebx,ecx		;right=nelem+pivot-1
		add ebx,edx
		dec ebx
		mov [right32],ebx

		mov eax,ecx		;left=nelem/2+pivot
		shr eax,1
		add eax,edx
		mov [left32],eax

		mov [qs_left],eax
		mov [qs_right],ebx

		call [hitw1]		;compare(left,right)
		cmp ax,1
		jne dont_exchange1
		call [hitw2]		;exchange

dont_exchange1	mov eax,[pivot32]
		mov [qs_right],eax
		call [hitw1]		;compare(left,pivot)
		cmp ax,1
		jne dont_exchange2
		call [hitw2]		;exchange
		jmps dont_exchange3

dont_exchange2	mov eax,[pivot32]
		mov [qs_left],eax
		mov eax,[right32]
		mov [qs_right],eax
		call [hitw1]		;compare(pivot,right)
		cmp ax,1
		jne dont_exchange3
		call [hitw2]		;exchange

dont_exchange3; mov ecx,[nelem32]
	      ; db 66h,81h,0f9h,3,0,0,0 ;cmp ecx,3
	      ; jne qs_hoare

	      ; mov eax,[pivot32]
	      ; mov [qs_left],eax
	      ; mov eax,[left32]
	      ; mov [qs_right],eax
	      ; jmp [hitw2]		;exchange(pivot,left)

;-------------- classic Hoare algorithm ---------------

qs_hoare	mov eax,[pivot32]
		inc eax
		mov [left32],eax	;left=pivot+1
		mov [pivotend32],eax	;pivotend=left

qs_do_while
qs_while1	mov eax,[left32]
		mov [qs_left],eax

		mov eax,[pivot32]
		mov [qs_right],eax

		call [hitw1]		;compare(left,pivot)

		cmp ax,1
		je qs_while2

		or ax,ax
		jnz qs_ship_xcgh1

		mov eax,[pivotend32]
		mov [qs_right],eax
		call [hitw2]		;exchange(left,pivotend)
		db 66h
		incw [pivotend32]	;pivotend++

qs_ship_xcgh1	mov eax,[left32]
		cmp eax,[right32]
		jae qs_break		;if left<right
		inc eax
		mov [left32],eax	;left++
		jmps qs_while1

;---------------
qs_while2	mov eax,[left32]
		cmp eax,[right32]
		jae while_break 	;if left<right

		mov eax,[pivot32]
		mov [qs_left],eax
		mov ebx,[right32]
		mov [qs_right],ebx
		call [hitw1]		;compare(pivot,right)

		cmp ax,-1
		jne qs_not_right

		db 66h
		decw [right32]		;right--
		jmps qs_while2

qs_not_right	push ax
		mov eax,[left32]
		mov [qs_left],eax
		call [hitw2]		;exchange(left,right)
		pop ax
		or ax,ax
		jz while_break

		db 66h
		incw [left32]		;left++

		db 66h
		decw [right32]		;right--

		;... while_break

;---------------
while_break	mov eax,[left32]
		cmp eax,[right32]
		jae qs_break

		jmp qs_do_while 	;if left<right

;---------------
qs_break	mov eax,[left32]
		mov [qs_left],eax
		mov eax,[pivot32]
		mov [qs_right],eax
		call [hitw1]		;compare(left,pivot)
		cmp ax,1
		je qs_skip_left 	;<>1

		db 66h
		incw [left32]		;left++

qs_skip_left	mov eax,[left32]
		dec eax
		mov [lefttemp32],eax	;lefttemp=left-1

		mov eax,[pivot32]
		mov [pivottemp32],eax	;pivottemp=pivot

;---------------
qs_while3	mov eax,[pivottemp32]
		mov ebx,[pivotend32]
		mov ecx,[lefttemp32]

		cmp eax,ebx
		jae qs_ship_while3

		cmp ecx,ebx
		jb qs_ship_while3

		mov [qs_left],eax
		mov [qs_right],ecx
		call [hitw2]		;exchange(pivottemp,lefttemp)

		db 66h
		incw [pivottemp32]	;pivottemp++
		db 66h
		decw [lefttemp32]	;pivottemp++

		jmps qs_while3

;---------------
qs_ship_while3	mov eax,[left32]
		sub eax,[pivotend32]
		mov [lnum32],eax	;lnum=left-pivotend

		mov ecx,[nelem32]
		add ecx,[pivot32]
		sub ecx,[left32]
		mov [nelem32],ecx	;nelem+=pivot-left

		;sort smaller portion first to reduce stack usage

		cmp ecx,eax
		jae qs_sort_pl

		mov edx,[left32]
		mov ecx,[nelem32]

		db 66h
		push [lnum32]
		db 66h
		push [pivotend32]
		db 66h
		push [right32]
		db 66h
		push [left32]
		db 66h
		push [pivot32]

		call qsorthelp

		db 66h
		pop [pivot32]
		db 66h
		pop [left32]
		db 66h
		pop [right32]
		db 66h
		pop [pivotend32]
		db 66h
		pop [lnum32]

		mov ecx,[lnum32]
		mov [nelem32],ecx

		mov edx,[pivot32]
		jmp tailrecursion	;edx=pivot, ecx=nelem

;---------------
qs_sort_pl	mov edx,[pivot32]
		mov ecx,[lnum32]

		db 66h
		push [lnum32]
		db 66h
		push [pivotend32]
		db 66h
		push [right32]
		db 66h
		push [left32]
		db 66h
		push [nelem32]

		call qsorthelp

		db 66h
		pop [nelem32]
		db 66h
		pop [left32]
		db 66h
		pop [right32]
		db 66h
		pop [pivotend32]
		db 66h
		pop [lnum32]

		mov ecx,[nelem32]
		mov edx,[left32]
		mov [pivot32],edx
		jmp tailrecursion	;edx=pivot, ecx=nelem

		db tf,tend

;==============================================================================

h386		db tf,hfset,x386
		db tf,ptitle,'Test 80386+ Processor',0

;excerpt of code taken from PC-INTERN book.
;only used this much code because wanted to test if 32 bit processor or not!

;test if 80286 or lower?

		pushf			;save flags
		xor ax,ax
		push ax 		;push 0
		popf			;pop, sets 12-15 bit if 8086?
		pushf
		pop ax
		and ax,0f000h
		cmp ax,0f000h
		jne t386b
not386		xor ax,ax
		popf
		ret

;test if 486,386,286?

t386b		mov ax,7000h
		push ax
		popf
		pushf
		pop ax
		and ax,7000h
		je not386

		mov ax,1		;return 1 - 386!
		popf
		ret

		db tf,tend

;==============================================================================

;vga_line x1,y1 x2,y2

hvga_line	db tf,hfset,xvga_line
		db tf,ptitle,'VGA Line Drawing',0

		pop si

		mov bx,ax	;DX,AX to CX,BX
		pop cx
		pop ax
		pop dx

		push si 	;return address

		mov si,0a000h	;vga 320*200 screen
		mov es,si

		cmp dx,cx
		jbe vga_xok
		xchg dx,cx
		xchg ax,bx	;Swap coordinates over?

vga_xok 	cmp ax,bx	;Which is higher - Y1 or Y2?
		xor bp,bp	;incy=0

		mov di,cx
		sub di,dx	;Difference between x's.

		mov si,bx
		sub si,ax
		jns vga_igy
		neg si		;Difference between y's.
		dec bp		;incy=-1

vga_igy 	movb lastx,255	;Make up default XINC and YINC.
		push ax,bx,dx
		mov ax,si
		mov bx,255
		mul bx
		or di,di
		jz vga_dy0
		div di
vga_dy0 	xor bp,ax	;xor incy,ax

		cmp di,si
		jae vga_drawit

		xor bp,ax	;Restore incy to signed only.
		xorw bp,255
		mov ax,di
		mov bx,255
		mul bx
		or si,si
		jz vga_dx0
		div si
vga_dx0 	mov lastx,al

vga_drawit	pop dx,bx,ax

		push ax,dx
		mov si,dx		;SI=dx+ax*320
		mov di,320
		mul di
		add si,ax
		pop dx,ax

		mov di,bp		;newy=starting increment

		mov ah,[scrcol]

		sub cx,dx		;y2-y1
		xor bh,bh
		sub bl,al
		jnc vga_counter
		neg bl

vga_counter	cmp cx,bx
		jge vga_incc
		mov cx,bx

vga_incc	inc cx
;		cmp cx,128
;		jl vga_drawl
;		inc cx

		mov bh,[lastx]		;starting BH

;==line loop ==================================================================

vga_drawl	movb es:[si],ah 	;plot dx-x,ax-y

		add bh,[lastx]		;newx+=lastx
		jnc vga_dpx
		inc dx
		inc si

vga_dpx 	add di,bp	;newy+=incy
		cmp di,255
		jg vga_da1
		cmp di,-255
		jg vga_dpop
		dec al		;Y1--
		sub si,320
		add di,256
		jmps vga_dpop

vga_da1 	inc al		;Y1++
		add si,320
		sub di,256

vga_dpop	dec cx
		jnz vga_drawl

		ret
		db tf,tend
