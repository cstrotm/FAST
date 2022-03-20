
;== FAST FUNCTIONS =========================================================

ffindb	db tf,tchr,'('
	db tf,texp
	mov [hitc],ax
	db tf,tcomma
	db tf,texp
	push ax
	db tf,tcomma
	db tf,texp
	push ax
	db tf,tcomma
	db tf,texp
	mov [hitd1],ax
	db tf,tcomma
	db tf,texp
	mov [hitd2],ax
	db tf,tcomma
	db tf,texp
	mov cx,ax
	pop di
	pop es
	db tf,tchr,')'
	db tf,hcall,xfind
	db tf,tend

fcdisk	mov ah,19h
	int 21h 	;Current disk?
	sub ah,ah
	db tf,tend

fpsp	mov ah,51h
	int 21h 	;Get current PSP
	mov ax,bx
	db tf,tend

findoss mov ah,34h
	int 21h
	mov ax,es	;INDOS segment.
	db tf,tend

findoso mov ah,34h
	int 21h
	mov ax,bx	;INDOS offset.
	db tf,tend

fprintm db tf,tseg,tf,tcomma,tf,tnum
	push ax
	db 0b9h,tf,pnum8	;MOV CX,len + CH=flag
	db tf,tcomp,',0',0
	db 88h			;MOV ES:[DI],AL
	db tf,hcall,xprm

	db tf,stop
	db 89h			;MOV ES:[DI],AX
	db tf,hcall,xprm
	db tf,tend

fcarry	mov ax,1
	jc fcarry1
	dec ax
fcarry1 db tf,tend

fdigits db tf,texp1
	mov bl,5
	cmp ax,9999
	ja fdigb
	dec bl
	cmp ax,999
	ja fdigb
	dec bl
	cmp ax,99
	ja fdigb
	dec bl
	cmp ax,9
	ja fdigb
	dec bl
fdigb	sub ah,ah
	mov al,bl
	db tf,tend

fread	push ds
	db tf,thandle,tf,tcomma
	push bx
	db tf,texp,tf,tword,3
	push ax
	db tf,tseg,tf,tpopds
	mov dx,ax
	pop cx
	pop bx
	mov ah,3fh
	int 21h
	pop ds
	db tf,terrs
	db tf,tend

fbase	db tf,trun
	call decima1	;Get @ address.
	jc baseabs
	xor ax,0ffffh
	mov es,labseg
	mov di,assdi
	mov es:2[di],ax ;Set new address
	clc
baseabs ret

assvar	dw 0	;Address of the variable to be assigned.
assdi	dw 0	;Address of the variable in LABSEG.
letcont mov ax,es:[di+2]
	mov assvar,ax
	mov assdi,di
	mov di,offset(opers)
letnas	call findnc
	jc baseabs
	mov di,1[di]
	jmp loopz	;Now use loop to finish off compiling the program.


;Registers which can be altered or read using REG.
regcf	db 'axbxcxdxdisi'
	db 'bpspdsessscs'

regvec	db 0,0,0,0

	mov bx,ax
	mov ax,bx

	mov cx,ax
	mov ax,cx

	mov dx,ax
	mov ax,dx

	mov di,ax
	mov ax,di

	mov si,ax
	mov ax,si

	mov bp,ax
	mov ax,bp

	mov sp,ax
	mov ax,sp

	mov ds,ax
	mov ax,ds

	mov es,ax
	mov ax,es

	mov ss,ax
	mov ax,ss

	dw -1
	mov ax,cs

	db 255


ccomp	push ds
	db tf,texp
	push ax
	db tf,tword,5
	db tf,tseg
	push ax
	db tf,tword,6
	db tf,tseg,tf,tpopds
	mov si,ax
	pop di
	pop es
	pop cx		;Compare CX words at ES:DI with DS:SI
	cld
	rep cmpsw
	mov ax,cx
	je cmpnone
	mov ax,di	;Return address where words differ.
	dec ax
	dec ax
cmpnone pop ds
	db tf,tend

ccompb	push ds
	db tf,texp
	push ax
	db tf,tword,5
	db tf,tseg
	push ax
	db tf,tword,6
	db tf,tseg,tf,tpopds
	mov si,ax
	pop di
	pop es
	pop cx		;Compare CX bytes at ES:DI with DS:SI
	cld
	rep cmpsb
	mov ax,cx
	je cpbnone
	mov ax,di	;Return address where bytes differ.
	dec ax
cpbnone pop ds
	db tf,tend

fhit	db tf,texp,tf,tcomma
	push ax
	db tf,texp,tf,tcomma
	push ax
	db tf,texp
	push ax
	db tf,tword,6
	db tf,texp,tf,tcomma
	push ax
	db tf,texp,tf,tcomma
	push ax
	db tf,texp
	push ax
	db tf,texp
	pop di,cx,bx,si,dx,bp
	db tf,hcall,xrthit,tf,tend

endnas	db tf,texp,tf,tchr,']',tf,trun
	mov si,spoint
	sub bh,bh
	mov bl,[si]
	or bl,[bx+lows]
	lea di,nasop
	cmp bl,'b'
	jne setnas
	incw spoint
	lea di,nasopb
setnas	jmp letnas	;Do same as LET but use different OPERATOR table.
vlpase	stc
	ret

fpage	mov ah,15
	int 10h
	mov al,bh
	mov [spage],al
	sub ah,ah
	db tf,tend

fmode	mov ah,15
	int 10h
	sub ah,ah
	db tf,tend

fhandle db tf,thandle
	mov ax,bx
	db tf,tend

fleft	db tf,texp1
	rol ax,1
	db tf,tend

fleftz	db tf,texp1
	sal ax,1
	db tf,tend

fright	db tf,texp1
	ror ax,1
	db tf,tend

frightz db tf,texp1
	shr ax,1
	db tf,tend

fsearch db tf,texp,tf,tword,7
	push ax
	db tf,tseg,tf,tword,8
	push ax
	db tf,texp
	pop di
	pop es
	pop cx
	cld
	repne scasw
	mov ax,cx
	jne fsno
	mov ax,di
	dec ax
	dec ax
fsno	db tf,tend

fsearcb db tf,texp,tf,tword,7
	push ax
	db tf,tseg,tf,tword,8
	push ax
	db tf,texp
	pop di
	pop es
	pop cx
	cld
	repne scasb
	mov ax,cx
	jne fsbno
	mov ax,di
	dec ax
fsbno	db tf,tend

fdta	db tf,tcomp,'offset',0
	mov ah,2fh
	int 21h
	mov ax,bx
	db tf,stop

	db tf,tcomp,'segment',0
	mov ah,2fh
	int 21h
	mov ax,es
	db tf,tend

fscreen db tf,tifvar,tf,tcomma,tf,trun
	mov ax,wvar
	push ax
	call loopifv
	pop dx
	jc slocnv
	cmp al,99
	je slocnv
	movw [bp],268ah
	add bp,2
	mov ax,dx
	call usv
	movb [bp],0a0h
	inc bp
	mov ax,wvar
	call usv
	jmps slc	;Put the call in.
slocnv	call restsp
	mov di,offset(slocn2)
	jmp loopz
	db tf,tend
slocn2	db tf,tifnum,tf,tcomma,tf,tifnum
	mov di,0
	db tf,trun

	mov dl,wnum
	mov dh,wnum2
	mov al,160	;Locate AX.
	mul dh
	sub dh,dh
	add ax,dx
	add ax,dx

	mov -2[bp],ax

	mov di,offset(sclcode)
	jmp loopz
sclc2d	mov di,ax
sclcode mov es,[scrseg]
	mov ax,es:[di]
	db tf,tend

slc	mov cl,xhfloc
	call ccwlib
	lea di,sclc2d
	jmp loopz

	db tf,tend,tf,treset
	db tf,texp,tf,tcomma
	db tf,tifnum
	mov ah,al
	db 0b0h 	;MOV AL,n
	db tf,pnum8,tf,trun
	jmps slc
	db tf,tend
	push ax
	db tf,texp
	pop bx
	mov ah,bl
	db tf,trun
	jmps slc


fselect db tf,texp,tf,tcomma
	db tf,tifvar
	db 8ah,36h,tf,pvar,tf,trun
fsel2	mov cl,xexmsel
	jmp ccwlib
	db tf,tend
	push ax
	db tf,texp
	mov dh,al
	pop ax
	db tf,trun
	jmps fsel2

frnd	db tf,hcall,xgetrnd,tf,tend

frnd32	db tf,hcall,xgetrnd
	push ax
	db tf,hcall,xgetrnd
	pop dx
	db tf,tend

fi	db tf,hcall,xinpax,tf,tend
fih	db tf,hcall,xhinpax,tf,tend

fib	db tf,hcall,xinpax
	sub ah,ah
	db tf,tend

fihb	db tf,hcall,xhinpax
	sub ah,ah
	db tf,tend

fhigh	db tf,trun
	call lvar32
	lea di,fhi2
	jc fhldi
	movb [bp],0a1h
	inc bp
	mov ax,wvar
	inc ax		;MOV AX,[32V+2]
	inc ax
	jmp usv

fhi2	db tf,tifvar
	db 0a0h,tf,trun ;MOV AL,[VAR+1]
	mov ax,wvar
	inc ax		;High byte of variable.
	call usv
	movw [bp],0e428h
	add bp,2
	ret
	db tf,tend
	db tf,texp1
	mov al,ah
	sub ah,ah
	db tf,tend

fhldi	jmp loopz

flow	db tf,trun
	call lvar32
	lea di,flo2
	jc fhldi
	movb [bp],0a1h
	inc bp
	mov ax,wvar	;MOV AX,[32V]
	jmp usv

flo2	db tf,tifvar
	db 0a0h,tf,pvar ;MOV AL,[VAR]
	sub ah,ah	;SUB AH,AH
	db tf,tend
	db tf,texp1
	sub ah,ah
	db tf,tend

fupper	db tf,texp1
	cmp al,'a'
	jb fupp
	cmp al,'z'
	ja fupp
	and al,223
fupp	db tf,tend

flower	db tf,texp1
	cmp al,'A'
	jb flowe
	cmp al,'Z'
	ja flowe
	or al,32
flowe	db tf,tend

fsub	db tf,tifnd,1,tf,trun
	mov ax,wnum
	cmp ax,1
	jne fsubn1
	cmp defd,1
	jne fsubn1
	movb [bp],48h
	inc bp
	ret
fsubn1	movb [bp],2dh
	mov [bp+1],ax
	add bp,3
	ret
	db tf,tend
	db tf,tifvar
	db 2bh,6,tf,pvar,tf,tend
	push ax
	db tf,texp1
	pop bx
	sub bx,ax
	mov ax,bx
	db tf,tend

fnot	db tf,texp
	or ax,ax
	mov ax,0
	jnz fnot0
	inc ax		;Make 0 if was not zero else 1.
fnot0	db tf,tend

fnot32	db tf,texp
	or ax,dx
	mov ax,0
	mov dx,ax
	jnz fnx32	;Make 0 if was not zero else 1.
	inc ax
fnx32	db tf,tend

fkped	db tf,hcall,xtakchk,tf,tend

fks	db tf,hcall,xtakey,tf,tend

fkey	db tf,hcall,xtakey
	sub ah,ah
	db tf,tend

fscan	db tf,hcall,xtakey
	mov al,ah
	sub ah,ah
	db tf,tend

fce	db tf,tifnd,1
	db 3dh,tf,pnum
	mov ax,1
	je fcen1
	dec ax
fcen1	db tf,tend,tf,tifvar
	db 3bh,6,tf,pvar
	mov ax,1
	je fcev1
	dec ax
fcev1	db tf,tend
	push ax
	db tf,texp
	pop bx
	cmp bx,ax
	mov ax,1
	je fce2
	dec ax
fce2	db tf,tend

;== 32 bit comparisons ========================================================

fcne32	push ax,dx
	db tf,texp32
	pop cx,bx
	xor ax,bx
	jnz fne32y
	xor dx,cx
	jz fne32e	;dx:ax = 0

fne32y	xor ax,ax
	inc ax

fne32n	xor dx,dx
fne32e	db tf,tend

;==============================================================================

fce32	push ax,dx
	db tf,texp32
	pop cx,bx
	xor ax,bx
	jnz fe32n
	xor dx,cx
	jz fe32y

fe32n	xor ax,ax
	xor dx,dx
	jmps fe32e

fe32y	inc ax
fe32e	db tf,tend

;==============================================================================

fcg32	push ax,dx
	db tf,texp32
	pop cx,bx	;cx:bx > dx:ax
	sub cx,dx
	jc fg32n
	jnz fg32y
	sub bx,ax
	jc fg32n
	jz fg32n

fg32y	xor ax,ax
	inc ax
	jmps fg32e

fg32n	xor ax,ax
fg32e	xor dx,dx
	db tf,tend

fcl32	push ax,dx
	db tf,texp32
	pop cx,bx	;cx:bx < dx:ax
	sub dx,cx
	jc fl32n
	jnz fl32y
	sub ax,bx
	jc fl32n
	jz fl32n

fl32y	xor ax,ax
	inc ax
	jmps fl32e

fl32n	xor ax,ax
fl32e	xor dx,dx
	db tf,tend

;==============================================================================

fcge32	push ax,dx
	db tf,texp32
	pop cx,bx	;cx:bx >= dx:ax
	sub cx,dx
	jc fge32n
	jnz fge32y
	sub bx,ax
	jc fge32n

fge32y	xor ax,ax
	inc ax
	jmps fge32e

fge32n	xor ax,ax
fge32e	xor dx,dx
	db tf,tend

fcle32	push ax,dx
	db tf,texp32
	pop cx,bx	;cx:bx <= dx:ax
	sub dx,cx
	jc fle32n
	jnz fle32y
	sub ax,bx
	jc fle32n

fle32y	xor ax,ax
	inc ax
	jmps fle32e

fle32n	xor ax,ax
fle32e	xor dx,dx
	db tf,tend

;==============================================================================

fcne	db tf,tifnd,1
	db 3dh,tf,pnum
	mov ax,1
	jne fcnen1
	dec ax
fcnen1	db tf,tend,tf,tifvar
	db 3bh,6,tf,pvar
	mov ax,1
	jne fcnev1
	dec ax
fcnev1	db tf,tend
	push ax
	db tf,texp
	pop bx
	cmp bx,ax
	mov ax,1
	jne fcne2
	dec ax
fcne2	db tf,tend

fcl	db tf,tifnd,1
	db 3dh,tf,pnum
	mov ax,1
	db tf,tfsign,7ch,72h
	db tf,tend,tf,tifvar
	db 3bh,6,tf,pvar
	mov ax,1
	db tf,tfsign,7ch,72h
	db tf,tend
	push ax
	db tf,texp
	pop bx
	cmp bx,ax
	mov ax,1
	db tf,tfsign,7ch,72h
fcl2	db tf,tend

ifabove push ax
	db tf,texp
	pop bx
	cmp bx,ax
	mov ax,1
	ja fab2
	dec ax
fab2	db tf,tend

ifbelow push ax
	db tf,texp
	pop bx
	cmp bx,ax
	mov ax,0
	adc ax,ax	;If carry then equals 1.
	db tf,tend

fcg	db tf,tifnd,1
	db 3dh,tf,pnum
	mov ax,1
	db tf,tfsign,7fh,77h
	db tf,tend,tf,tifvar
	db 3bh,6,tf,pvar
	mov ax,1
	db tf,tfsign,7fh,77h
	db tf,tend
	push ax
	db tf,texp
	pop bx
	cmp bx,ax
	mov ax,1
	db tf,tfsign,7fh,77h
fcg2	db tf,tend

fcge	db tf,tifnd,1
	db 3dh,tf,pnum
	mov ax,1
	db tf,tfsign,7dh,73h
	db tf,tend,tf,tifvar
	db 3bh,6,tf,pvar
	mov ax,1
	db tf,tfsign,7dh,73h
	db tf,tend
	push ax
	db tf,texp
	pop bx
	cmp bx,ax
	mov ax,1
	db tf,tfsign,7dh,73h
fcge2	db tf,tend

fcle	db tf,tifnd,1
	db 3dh,tf,pnum
	mov ax,1
	db tf,tfsign,7eh,76h
	db tf,tend,tf,tifvar
	db 3bh,6,tf,pvar
	mov ax,1
	db tf,tfsign,7eh,76h
	db tf,tend
	push ax
	db tf,texp
	pop bx
	cmp bx,ax
	mov ax,1
	db tf,tfsign,7eh,76h
fcle2	db tf,tend

falloc	db tf,tifnd,1
	db 0bbh,tf,pnum
	mov ah,48h
	int 21h 	;Leaves carry flag intact for IF ERROR THEN checks.
	db tf,terrs
	db tf,tend
	db tf,texp
	mov bx,ax
	mov ah,48h
	int 21h
	db tf,terrs
	db tf,tend

fpoint	db tf,texp
	push ax
	db tf,tcomma,tf,texp
	mov bx,ax
	pop dx
	db tf,hcall,xpoint
	db tf,tend

fcurpos sub bh,bh
	mov ah,3
	int 10h
	mov ax,dx
	db tf,tend

fequal	db tf,tifnd,4
	db 0c7h,6,tf,passvar,tf,pnum,tf,tend
	db tf,tifvar
	db tf,trun
	mov ax,wvar
	cmp ax,assvar
	jne feqput		;wnum=assvar - no need!!!
	ret

feqput	movb [bp],0a1h		;mov ax,[n]
	inc bp
	call usv		;put wvar!!!
	movb [bp],0a3h		;mov [n],ax
	inc bp
	mov ax,assvar
	jmp usv

	db tf,tend
	db tf,texp
	db 0a3h,tf,passvar
	db tf,tend

equal32 db tf,texp32,tf,put32,tf,tend

nequal	db tf,tifnum
	mov di,ax
	db 8eh,6,tf,passvar
	db 26h,0c7h,5,tf,pnum	;MOVW ES:[DI],nn
	db tf,tend

	db tf,tifvar
	mov di,ax
	db 0a1h,tf,pvar
	db 8eh,6,tf,passvar
	mov es:[di],ax
	db tf,tend

	push ax
	db tf,texp
	pop di
	db 8eh,6,tf,passvar
	mov es:[di],ax
	db tf,tend

nbequal push ax
	db tf,texp
	pop di
	db 8eh,6,tf,passvar
	mov es:[di],al
	db tf,tend

fadde	db tf,tifnd,4
	db 081h,6,tf,passvar,tf,pnum,tf,tend
	db tf,texp
	db 1,6,tf,passvar
	db tf,tend

adde32	db tf,texp32
	db 1,6,tf,passvar	;ADD [LOW],AX
	db 11h,16h,tf,passp	  ;ADC [HIGH],DX
	db tf,tend

sube32	db tf,texp32
	db 29h,6,tf,passvar	;SUB [LOW],AX
	db 19h,16h,tf,passp	;SBB [HIGH],DX
	db tf,tend

nadde	push ax
	db tf,texp
	db 8eh,6,tf,passvar
	pop di
	add es:[di],ax
	db tf,tend

nbadde	push ax
	db tf,texp
	db 8eh,6,tf,passvar
	pop di
	add es:[di],al
	db tf,tend

fadd32 ;db tf,tifnum
       ;db 05h,tf,pnum		;add ax,number-low
       ;db 81h,0d2h,tf,pnuml	;adc dx,number-high
       ;db tf,stop

	push ax,dx
	db tf,texp32
	pop cx,bx
	add ax,bx
	adc dx,cx
	db tf,tend

fsub32	push ax,dx
	db tf,texp32
	pop cx,bx
	xchg dx,cx
	xchg ax,bx
	sub ax,bx
	sbb dx,cx
	db tf,tend

fadd	db tf,tifnd,1,tf,trun
	mov ax,wnum
	cmp ax,1
	jne faddn1
	cmp defd,1
	jne faddn1
	movb [bp],40h
	inc bp
	ret
faddn1	movb [bp],5
	mov [bp+1],ax
	add bp,3
	ret
	db tf,tend
	db tf,tifvar,3h,6h,tf,pvar,tf,tend
	push ax
	db tf,texp
	pop bx
	add ax,bx
	db tf,tend

fsube	db tf,tifnd,4
	db 081h,2eh,tf,passvar,tf,pnum,tf,tend
	db tf,texp
	db 29h,6,tf,passvar
	db tf,tend

nsube	push ax
	db tf,texp
	pop di
	db 8eh,6,tf,passvar
	sub es:[di],ax
	db tf,tend

nbsube	push ax
	db tf,texp
	pop di
	db 8eh,6,tf,passvar
	sub es:[di],al
	db tf,tend

fmult32 db tf,trun
	push spoint
	call dec32
	mov si,spoint
	pop spoint
	lea di,fmc32
	jc md32lp
	cmp ax,256
	je fmshb
	cmp dx,1
	jne md32lp
	or ax,ax
	jnz md32lp

	mov spoint,si
	movw [bp],0c289h
	movw [bp+2],0c029h
	add bp,4
	ret

fmshb	or dx,dx
	jnz md32lp
	mov spoint,si
	movw [bp],0d688h
	movw [bp+2],0e288h
	movw [bp+4],0c488h
	movw [bp+6],0c028h
	add bp,8
	ret

fmc32	push ax,dx
	db tf,texp32
	pop cx,bx
	db tf,hcall,xmul32,tf,tend

md32lp	jmp loopz

fdiv32	db tf,trun
	push spoint
	call dec32
	mov si,spoint
	pop spoint
	lea di,fdc32
	jc md32lp
	cmp ax,256
	je fdshb
	cmp dx,1
	jne md32lp
	or ax,ax
	jnz md32lp

	mov spoint,si
	movw [bp],0d089h
	movw [bp+2],0d229h
	add bp,4
	ret

fdshb	or dx,dx
	jnz md32lp
	mov spoint,si
	movw [bp],0e088h
	movw [bp+2],0d488h
	movw [bp+4],0f288h
	movw [bp+6],0f628h
	add bp,8
	ret

fdc32	push ax,dx
	db tf,texp32
	pop cx,bx
	push dx,ax,cx,bx
	db tf,hcall,xdiv32,tf,tend

fmult	db tf,trun
	call decmal
	jc fmuni
	mov wnum,ax
	lea di,fmulp
	cmp ax,1
	jne okom
	ret		;Do nothing to multiply by 1.
okom	cmp ax,2
	jb fmulp1
	cmp ax,10
	jbe fmulp10
	cmp ax,256
	jne fmulp1
	mov ax,11	;use the 11th entry!
fmulp10 lea di,multab
	sub ax,2
	add ax,ax
	add di,ax
	mov di,[di]
fmulp1	jmp loopz
fmulp	db 0bbh,tf,pnum
	mul bx
	db tf,tend

fmuni	lea di,fmunil
	jmp loop
fmunil	db tf,tifvar
	db 0f7h,26h,tf,pvar
	db tf,tend
	push ax
	db tf,texp1
	pop bx
	mul bx
	db tf,tend

multab	dw offset(fm2),offset(fm3)
	dw offset(fm4),offset(fm5)
	dw offset(fm6),offset(fm7)
	dw offset(fm8),offset(fm9)
	dw offset(fm10),offset(fm256)

fm2	add ax,ax
	db tf,tend

fm3	mov bx,ax
	add ax,ax
	add ax,bx
	db tf,tend

fm4	add ax,ax
	add ax,ax
	db tf,tend

fm5	mov bx,ax
	add ax,ax
	add ax,ax
	add ax,bx
	db tf,tend

fm6	add ax,ax
	mov bx,ax
	add ax,ax
	add ax,bx
	db tf,tend

fm7	mov cx,ax
	add ax,ax
	mov bx,ax
	add ax,ax
	add ax,bx
	add ax,cx
	db tf,tend

fm8	add ax,ax
	add ax,ax
	add ax,ax
	db tf,tend

fm9	mov bx,ax
	add ax,ax
	add ax,ax
	add ax,ax
	add ax,bx
	db tf,tend

fm10	mov bx,ax
	add ax,ax
	add ax,ax
	add ax,bx
	add ax,ax
	db tf,tend

fm256	mov ah,al
	sub al,al
	db tf,tend

fdiv	db tf,tifnum1
	db 0bbh,tf,pnum
	sub dx,dx
	div bx
	db tf,tend
	db tf,tifvar
	sub dx,dx
	db 0f7h,36h,tf,pvar
	db tf,tend
	push ax
	db tf,texp1
	mov bx,ax
	pop ax
	sub dx,dx
	div bx
	db tf,tend

fmod	db tf,tifnum1
	db 0bbh,tf,pnum
	sub dx,dx
	div bx
	mov ax,dx
	db tf,tend
	db tf,tifvar
	sub dx,dx
	db 0f7h,36h,tf,pvar
	mov ax,dx
	db tf,tend
	push ax
	db tf,texp1
	mov bx,ax
	pop ax
	sub dx,dx
	div bx
	mov ax,dx
	db tf,tend

fmulte	db tf,texp
	mov bx,ax
	db 0a1h,tf,passvar
	mul bx
	db 0a3h,tf,passvar,tf,tend

mule32	db tf,texp32
	db tf,get32
	db tf,hcall,xmul32
	db tf,put32
	db tf,tend

dive32	db tf,texp32
	db tf,get32
	push dx,ax,cx,bx
	db tf,hcall,xdiv32
	db tf,put32
	db tf,tend

nmulte	push ax
	db tf,texp
	mov bx,ax
	pop di
	db 8eh,6,tf,passvar
	mov ax,es:[di]
	mul bx
	mov es:[di],ax
	db tf,tend

nbmulte push ax
	db tf,texp
	mov bx,ax
	pop di
	db 8eh,6,tf,passvar
	mov al,es:[di]
	mul bl
	mov es:[di],al
	db tf,tend

fstack	mov ax,sp
	db tf,tend

ftimer	sub ah,ah
	int 1ah
	mov ax,dx
	db tf,tend

ftim32	sub ah,ah
	int 1ah
	mov ax,dx
	mov dx,cx
	db tf,tend

fabs	db tf,texp1	;Two's complement.
	cmp ax,32768
	jb fabsp
	neg ax		;Negate if negative number.
fabsp	db tf,tend

fdive	db tf,texp
	mov bx,ax
	db 0a1h,tf,passvar
	sub dx,dx
	div bx
	db 0a3h,tf,passvar,tf,tend

ndive	push ax
	db tf,texp
	mov bx,ax
	pop di
	db 8eh,6,tf,passvar
	mov ax,es:[di]
	sub dx,dx
	div bx
	mov es:[di],ax
	db tf,tend

nbdive	push ax
	db tf,texp
	mov bx,ax
	pop di
	db 8eh,6,tf,passvar
	mov al,es:[di]
	sub ah,ah
	div bx
	mov es:[di],al
	db tf,tend

finc	db 0ffh,6,tf,passvar,tf,tend

iic32	db 0ffh,06h,tf,passvar
	jnz nin32
	db 0ffh,06h,tf,passp
nin32	db tf,tend

ddc32	db 0ffh,0eh,tf,passvar
	db 83h,3eh,tf,passvar,-1
	jne nde32
	db 0ffh,0eh,tf,passp
nde32	db tf,tend

ninc	mov di,ax
	db 8eh,6,tf,passvar
	incw es:[di]
	db tf,tend

nbinc	mov di,ax
	db 8eh,6,tf,passvar
	incb es:[di]
	db tf,tend

fdec	db 0ffh,0eh,tf,passvar,tf,tend

ndec	mov di,ax
	db 8eh,6,tf,passvar
	decw es:[di]
	db tf,tend

nbdec	mov di,ax
	db 8eh,6,tf,passvar
	decb es:[di]
	db tf,tend

fin	db tf,tifnum1
	db 0bah,tf,pnum
	sub ah,ah
	in al,dx
	db tf,tend
	db tf,tifvar
	db 08bh,16h,tf,pvar
	sub ah,ah
	in al,dx
	db tf,tend
	db tf,texp1
	mov dx,ax
	sub ah,ah
	in al,dx
	db tf,tend

freg	db tf,trun
	call findreg
	jc noreg
	mov ax,[di+2]	;MOV AX,reg is the second word in the table.
	or al,al
	jz noreg	;NC (do nothing for register AX).
	jmp lpute2
noreg	ret
findreg lea di,regcf
	mov cx,12
	call qureg
	mov al,2
	jc noreg
	lea di,regvec
	add cx,cx
	add di,cx
	add di,cx	;DI+=CX*4
	ret

fand	db tf,tifnd,1
	db 25h,tf,pnum,tf,tend
	db tf,tifvar
	db 23h,6,tf,pvar,tf,tend
	push ax
	db tf,texp1
	pop bx
	and ax,bx
	db tf,tend

and32	push ax,dx
	db tf,texp32
	pop cx,bx
	and dx,cx
	and ax,bx
	db tf,tend

or32	push ax,dx
	db tf,texp32
	pop cx,bx
	or dx,cx
	or ax,bx
	db tf,tend

xor32	push ax,dx
	db tf,texp32
	pop cx,bx
	xor dx,cx
	xor ax,bx
	db tf,tend

for	db tf,tifnd,1
	db 0dh,tf,pnum,tf,tend
	db tf,tifvar
	db 0bh,6,tf,pvar,tf,tend
	push ax
	db tf,texp1
	pop bx
	or ax,bx
	db tf,tend

fxor	db tf,tifnd,1
	db 35h,tf,pnum,tf,tend
	db tf,tifvar
	db 33h,6,tf,pvar,tf,tend
	push ax
	db tf,texp1
	pop bx
	xor ax,bx
	db tf,tend

nextq	call skip2	;Carry = seg:off
	cmp al,pipe	;Must be an absolute address.
	jne nqnc
	incw spoint
	stc
	ret
nqnc	clc
	ret

fpeek	db tf,tsetp,tf,trun
	mov dmust,0
	inc bp
	call decmal
	dec bp
	jc pq2
	mov wnum,ax
	call nextq
	jc fpc
	lea di,pdec
fpusd	jmp loopz

pq2	call restsp
	call eword
	jc fpc
	cmp al,variabl
	jne fpc
	mov wvar,bx
	call nextq
	jc fpc
	lea di,ppvar
	jmps fpusd

pdec	db 0a1h,tf,pnum,tf,tend

ppvar	db 08bh,3eh,tf,pvar
	mov ax,[di]
	db tf,tend

fpc	call restsp
	call oneexp
	jc fpkerr
	call nextq
	jnc fpoff
	mov di,offset(fpsegc)
	jmp loopz
fpsegc	push ax
	db tf,texp1
	pop es
	mov di,ax
	mov ax,es:[di]
	db tf,tend
fpoff	mov di,offset(fpoffc)
	jmp loopz
fpoffc	mov di,ax
	mov ax,[di]
	db tf,tend

fpkerr	ret
fpeekb	db tf,tsetp,tf,trun
	mov dmust,0
	add bp,3
	call decmal
	dec bp
	dec bp
	dec bp
	jc pbq2
	mov wnum,ax
	call nextq
	jc fpbc
	lea di,pbdec
fpbusd	jmp loopz

pbq2	call restsp
	call eword
	jc fpbc
	cmp al,variabl
	jne fpbc
	mov wvar,bx
	call nextq
	jc fpbc
	lea di,pbvar
	jmps fpbusd

pbdec	sub ah,ah
	db 0a0h,tf,pnum,tf,tend

pbvar	db 08bh,3eh,tf,pvar
	sub ah,ah
	mov al,[di]
	db tf,tend

fpbc	call restsp
	call skip2
	cmpb [si],'('
	jne fbfullj
	inc spoint
	add bp,3
	call decis1
	sub bp,3
	jc fbfullj
	mov si,spoint
	cmpb [si],')'
	jne fpbc2
	inc spoint
	mov cl,pipe
	cmp [si],cl
	je fbfullj

	movw [bp],0e428h
	movb [bp+2],0a0h
	mov [bp+3],ax
	add bp,5
	ret

fbfullj jmps fbfull

fpbc2	call restsp
	call skip2
	cmpb [si],'('
	jne fbfull
	inc spoint
	add bp,8
	call decis1
	sub bp,8
	jc fbfull
	mov si,spoint
	cmpb [si],'+'
	jne fbfull
	inc spoint
	push ax
	call getvar
	pop dx
	jc fbfull
	cmpb [si],')'
	jne fbfull
	inc spoint
	mov cl,pipe
	cmp [si],cl
	je fbfull

	movw [bp],3e8bh
	add bp,2
	call usv
	movw [bp],0e428h
	movw [bp+2],858ah
	mov [bp+4],dx
	add bp,6
fpkerr2 ret

fbfull	call restsp
	call oneexp
	jc fpkerr2
	call nextq
	jnc fpboff
	mov di,offset(fpbsegc)
	jmp loopz
fpbsegc push ax
	db tf,texp1
	pop es
	mov di,ax
	sub ah,ah
	mov al,es:[di]
	db tf,tend
fpboff	mov di,offset(fpboffc)
	jmp loopz
fpboffc mov di,ax
	sub ah,ah
	mov al,[di]
	db tf,tend

openb	call loopexp	;One number can be a complicated expression inside ()
opcom1	jnc opcom
	cmp al,30
	jne fbre
opcom	mov si,spoint	;brackets.
	cmpb [si],')'
	jne fbrake
	incw spoint
pnoth	clc
	ret
fbrake	mov al,8
fbre	stc
	ret

fope32	call lexp32	;One number can be a complicated expression inside.
	jmps opcom1

fgvect	db tf,thex
	db 0b8h,tf,pnum8,35h
	int 21h
	mov dx,es
	mov ax,bx
	db tf,tend

	db tf,texp
	mov ah,35h
	int 21h
	mov dx,es
	mov ax,bx
	db tf,tend

fpek32	db tf,tsetp,tf,trun
	mov dmust,0
	inc bp
	call decmal
	dec bp
	jc pq2d
	or ax,ax
	jz pq2d 		;undefined type?
	mov wnum,ax
	call nextq
	jc fpcd
	lea di,pdecd
fpusdd	jmp loopz

pq2d	call restsp
	call eword
	jc fpcd
	cmp al,variabl
	jne fpcd
	mov wvar,bx
	call nextq
	jc fpcd
	lea di,ppvard
	jmps fpusdd

pdecd	db 0a1h,tf,pnum,tf,trun 	;mov ax,[num]
	addw wnum,2
	lea di,pdec2d
	jmps fpusdd

pdec2d	db 08bh,016h,tf,pnum,tf,tend	;mov dx,[num+2]

ppvard	db 08bh,3eh,tf,pvar
	mov ax,[di]
	mov dx,[di+2]
	db tf,tend


fpcd	call restsp
	call oneexp
	jnc fbx32
	ret

fbx32	call nextq
	jnc fpoff32
	mov di,offset(fpseg32)
	jmp loopz
fpseg32 push ax
	db tf,texp1
	pop es
	mov di,ax
	mov ax,es:[di]
	mov dx,es:[di+2]
	db tf,tend
fpoff32 mov di,offset(fpof32)
	jmp loopz
fpof32	mov di,ax
	mov ax,[di]
	mov dx,[di+2]
	db tf,tend

mod32	push ax,dx
	db tf,texp32
	pop cx,bx
	push dx,ax,cx,bx
	db tf,hcall,xdiv32
	mov ax,si
	mov dx,di
	db tf,tend

feof	db tf,thandle
	push bx
	mov ah,3fh
	mov dx,offset(hitw1)	;Spare byte.
	mov cx,1
	int 21h 		;Try to read one byte.
	pop bx
	xor ax,1		;If 0 then EOF=1 else EOF=0.
	jnz feofr

	mov ax,4201h
	mov dx,-1
	mov cx,dx
	int 21h 		;Go back 1 byte (that we read).
	sub ax,ax		;AX=0, not EOF.

feofr	db tf,terrs
	db tf,tend

fssend ;db tf,texp
       ;mov ah,1	;send character
       ;mov dx,[com_port]
       ;int 14h 	;bit 7 of AH = 1 if error
       ;db tf,tend

	#if 1
	mov dx,[com_base]
	add dl,6	;3feh
	in al,dx	;Modem status?
	and al,30h
	jz fssend

	dec dx
fsn2	in al,dx	;Line status?
	and al,20h
	jz fsn2

	db tf,texp
	mov dx,[com_base]
	out dx,al
	db tf,tend
	#endif

fsstat	mov ah,3
	mov dx,[com_port]
	int 14h 	;Return line status.
	db tf,tend

;==============================================================================

fproc32 db tf,hcall,x386
	db tf,tend

;==============================================================================

fbcd2	db tf,texp1
	mov bl,al
	mov bh,10
	mov cl,4
	shr al,cl	;al=al/16
	mul bh		;al=al*10

	and bl,0fh	;ah=ah and 0fh
	add al,bl	;al=al+ah
	sub ah,ah	;ah=0
	db tf,tend

;==============================================================================

fimul	db tf,tchr,'('  ;multiply signed integers
	db tf,texp
	push ax
	db tf,tcomma
	db tf,texp
	db tf,tchr,')'

	pop dx
	imul dx 	;AX or DX:AX
	db tf,tend

;==============================================================================

fidiv	db tf,tchr,'('  ;divide signed integers
	db tf,texp
	push ax
	db tf,tcomma
	db tf,texp
	db tf,tchr,')'

	mov cx,ax
	pop ax
	cwd
	idiv cx 	;DX:AX / CX
	db tf,tend
