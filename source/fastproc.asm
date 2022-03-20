
;== FAST PROCEDURES ========================================================

clocal	db tf,trun
	movb [localm],1 	;set local variables true!
	clc
	ret

cdiv64	db tf,texp,tf,tcomma	;div64 address,32
	push ax 		;divides [address:64] / 32 = [address:64]
	db tf,texp32
	pop si
	db tf,hcall,xdiv64
	db tf,tend

cmul64	db tf,texp32		;mul64 32,32 address
	push dx,ax		;multiplies 32*32 answer @ address
	db tf,tcomma
	db tf,texp32
	push dx,ax
	db tf,texp
	mov di,ax
	pop ax,dx
	pop bx,cx
	db tf,hcall,xmul64
	db tf,tend

cprotek db tf,hcall,xprotek
	db tf,tend

cpsp	db tf,texp
	mov bx,ax
	mov ah,50h
	int 21h 	;Set PSP
	db tf,tend

onidle	mov ax,3528h
	int 21h
	mov [idleo],bx
	mov [idles],es
	mov dx,1000
	mov ax,2528h	;Set idle interupt.
	int 21h
	jmp start	;Jump over it.
	db tf,trun
	mov [bp-10],bp	;Put address of the break code in program.
	mov al,0cfh	;IRET instruction.
	jmp offax2

stidle	push ds
	mov ax,2528h
	mov dx,[idleo]
	mov ds,[idles]	;Reset IDLE interupt.
	int 21h
	pop ds
	db tf,tend

ciret	iret
	db tf,tend

cretf	retf
	db tf,tend

csetdos db tf,trun
	call skip2
	mov cl,[si]
	cmp cl,'0'
	jb dosnot
	inc si
	cmpb [si],'.'
	jne dosnot
	inc si
	mov ch,[si]
	cmp ch,'0'
	jb dosnot
	inc si
	mov dl,[si]
	cmp dl,'0'
	jb dosnot
	inc si
	mov spoint,si
	mov al,cl	;AX=major.
	sub al,'0'
	mov ah,ch
	sub ah,'0'
	add ah,ah
	mov bl,ah
	add ah,ah
	add ah,ah
	add ah,bl	;+minor1*10*256
	add ah,dl	;AX=major+minor*256
	sub ah,'0'
;	mov bx,chkdos
	xchg bl,bh
	xchg al,ah
	cmp ax,bx
	jbe dosig	;Ignore - too low.
	xchg al,ah
;	mov chkdos,ax
;	lea di,dostext
;	mov [di],cl
;	mov [di+2],ch
;	mov [di+3],dl
dosig	clc
	ret

dosnot	mov al,156	;SETDOS error.
	stc
	ret

clong	db tf,trun
	mov irange,0	;Set long jumps in IF statements.
	clc
	ret

cshort	db tf,trun	;Set short jumps in IF statements.
	mov irange,1
	clc
	ret

centry	db tf,trun
	mov entry,bp	;Set new entry point.
	ret

cftrace db tf,tcomp,'on',0,tf,trun
	orb doption,1
	mov t_start,1	;Say started using trace.
	ret
	db tf,stop
	db tf,tcomp,'off',0,tf,trun
	andb doption,254
	ret
	db tf,stop,tf,trun
	mov al,152		;Expected ON or OFF.
	stc
	ret

cfdebug db tf,tcomp,'on',0,tf,trun
	orb doption,2
	ret
	db tf,stop
	db tf,tcomp,'off',0,tf,trun
	andb doption,253
	ret
	db tf,stop,tf,trun
	mov al,152		;Expected ON or OFF.
	stc
	ret

cendfor jmp $
	db tf,trcode,11 	;FOREVER statement-(endfor's)
	db tf,tend

chtext	mov ax,0007h
	int 10h
cnull	db tf,tend		;This end is used by NULL to do nothing.

chgraph db tf,hcall,xherc
	db tf,tend

cprints db tf,tseg,tf,tcomma,tf,tnum	;PRINTS address,eom
	db 0b1h,tf,pnum8	;MOV CL,eom
	pop es
	mov si,ax
cpsl	mov al,es:[si]
	cmp al,cl
	je cpsend
	inc si
	push es,si,cx
	db tf,hcall,xprint
	pop cx,si,es
	jmps cpsl
cpsend	db tf,tend

csvect	db tf,thex,tf,tword,3	;SETINT b TO n
	db 0b8h,tf,pnum8,25h	;MOV AX,25BBh
	db tf,tifnum
	db 0bah,tf,pnum 	;MOV DX,nnnn
	int 21h
	db tf,tend

	push ax
	db tf,texp
	mov dx,ax
	pop ax
	mov ah,25h
	int 21h
	db tf,tend

cdos	db tf,thex,tf,trun
	mov si,spoint
	cmpb [si],'('
	jne dosah
	incw spoint
	push wnum
	call lthex
	pop bx
	jc cdose
cduse2	cmpb [si],')'
	mov al,16
	jne cdose
	mov bh,bl
	mov bl,wnum
	incw spoint
	movb [bp],0b8h
	mov [bp+1],bx
	add bp,3
doscom	mov ax,21cdh
	jmp lpute2

dosah	movb [bp],0b4h
	mov al,wnum
	mov [bp+1],al
	add bp,2
	jmps doscom
cdose	stc
	ret

cshell	db tf,tend

cerrors db tf,tcomp,'on',0,tf,trun
	mov errstat,1
	clc
	ret
	db tf,stop

	db tf,tcomp,'off',0,tf,trun
	mov errstat,0
	clc
	ret
	db tf,stop

	db tf,trun
	mov al,152	;Error, expected either on or off.
	stc
	ret

cline	db tf,texp	;LINE x1,y1 TO x2,y2
	push ax
	db tf,tcomma
	db tf,texp
	push ax
	db tf,tword,3
	db tf,texp
	push ax
	db tf,tcomma
	db tf,texp
	db tf,hcall,xline
	db tf,tend

cctl	mov ah,3
	mov bh,[spage]
	int 10h 	;Get cursor position.

	mov al,[fscreen_cols1]
	mul dh		;AX=row*160
	add ax,ax
	sub dh,dh
	add ax,dx
	add ax,dx	;AX=row*160+col*2

	mov [scrpos],ax
	db tf,tend

cltc	mov ax,[scrpos]
	mov bl,[fscreen_cols1]
	shr ax,1
	div bl
	xchg al,ah
	mov dx,ax
	mov ah,2
	mov bh,[spage]
;	shr dl,1
	int 10h
	db tf,tend

cocrit	mov dx,1000	;Offset for break put here.
	mov ax,2524h
	int 21h 	;Normal procedure for setting interupt.
	jmp start	;Jump over it.
	db tf,trun
	mov -10[bp],bp	;Put address of the break code in program.
	mov al,0cfh	;IRET instruction.
	jmp offax

cscrit	push ds
	lds dx,[12h]
	mov ax,2524h
	int 21h
	pop ds
	db tf,tend

cpalett db tf,texp
	mov bx,ax
	mov ah,0bh
	int 10h
	db tf,tend

cfunct	jmp start
	db tf,trcode,10 ;Both funcs and procs have PRCODE=10.
	db tf,trun
	cmpb [inproc],0
	jz outproc
	mov al,168	;can't nest procedures or functions
	stc
	ret
outproc movb [inproc],1
	call aunique
	mov es,labseg
	jnc cfpent
	jmp cfperr
cfpent	movb es:1[di],functio	;Function type.
pfcomn	mov si,pfmoff
	mov es:2[di],si
	mov es:4[di],bp 	;Setup table.

;	mov es,pfmseg		;see below - put end marker
;	movb es:[si],ect	;Put end character for PFM table.
;	jmps pfpcp2

pfpcp2	call skip2
	cmp al,'('
	jne cfcont
	incw spoint
	call skip2
	cmp al,')'   ;Check for ().
	jne pfcpb2
	incw spoint
	jmps cfcont

pfcpb2	mov stpfm,si	;Store start for variables text.

pfvars	mov ah,1
	call avarv	;Just add variable into table, no code.
	jc cfperr
	call lpcomma
	jnc pfvars

	call skip2
	cmp al,')'
	mov al,26
	jne cfperr
	inc si
	mov spoint,si

	mov es,pfmseg
	mov di,pfmoff
	mov dx,si	;End of parameter list.
	mov si,stpfm
pfvlp	mov al,[si]
	cmp al,')'
	je pfvl2
	mov es:[di],al	;Put the byte in the appropriate segment for PFMs.
	inc si
	inc di
	cmp si,dx
	jb pfvlp

pfvl2	movb es:[di],ect
	inc di
	mov pfmoff,di	;Update table entry.

cfcont	call lpstat
	jc cfperr

	movb [inproc],0 	;finished proc/func declaration.

	mov si,pfmoff
	mov es,pfmseg
	movb es:[si],ect	;Put end character for PFM table.
	inc si
	mov pfmoff,si

	mov ax,lastpf		;check if can make last call a jmp?
	add ax,3
	cmp ax,bp
	jne retinst
	movb [bp-3],0e9h	;JMP funt/gosub
	jmps retskip

retinst movb [bp],0c3h
	inc bp		;The RET instruction to return to the caller.

retskip mov cl,10	;Get address of the 'skip' jump.
	call getclp
	jc cfperr
	mov bx,bp
	mov di,ax
	sub bx,ax
	mov ss:-2[di],bx	;Put address for jump.

	clc
	ret

cfperr	stc
	ret
stpfm	dw 0

cpushal push ax,bx,cx,dx,bp,si,di,es,ds
	db tf,tend

cpopal	pop ds,es,di,si,bp,dx,cx,bx,ax
	db tf,tend

con	db tf,trun
	lea di,ontab
	call fentry2
	mov si,di
	lea di,congg
	jc oloop
	mov di,[si+1]
oloop	jmp loop
ontab	db 3,'break',13
	dw offset(conbrk)
	db 2,'int',13
	dw offset(conint)
	db 2,'key',13
	dw offset(ckey)
	db 2,'scan'
	dw offset(cscan)
	db 4,'critical'
	dw offset(cocrit)
	db 3,'error',13
	dw offset(cone)
	db 3,'trace',13
	dw offset(ontrace)
	db 3,'debug',13
	dw offset(ondebug)
	db 2,'idle'
	dw offset(onidle)
	db 255

cstop	db tf,trun
	lea di,stoptab
	call fentry2
	mov si,di
	lea di,cterm
	jc sloop
	mov di,[si+1]
sloop	jmp loop
stoptab db 4,'resident'
	dw offset(csres)
	db 2,'int',13
	dw offset(cstint)
	db 2,'key',13
	dw offset(cstkey)
	db 2,'scan'
	dw offset(cstscan)
	db 4,'critical'
	dw offset(cscrit)
	db 3,'error',13
	dw offset(cserr)
	db 3,'trace',13
	dw offset(sttrace)
	db 3,'debug',13
	dw offset(stdebug)
	db 2,'idle'
	dw offset(stidle)
	db 255

sttrace movw [tracev],0 	;Clear trace vector.
	db tf,tend

stdebug mov ax,debugi
	mov [debugv],ax 	;Set debug to IRET.
	db tf,tend

congg	db tf,treset
	db tf,texp
	jmps conge	;Make any short jump
	db tf,trcode,6,tf,trun
	call rlook
	jc conge
	cmp ah,15	;GOTO.
	je congt
	cmp ah,16	;GOSUB.
	je congt
	mov al,21
conge	stc
	ret
congst	db 0
congt	mov congst,ah
conglp	call decis1
	jc conge
	mov [bp],ax
	add bp,2
	call lpcomma
	jc congend
	jmps conglp
congend mov cl,6
	call getclp
	jc conge
	mov di,ax

	mov wnum,ax	;Set the number to the address of the table so the
			;table for GOTO/GOSUB can use it as a pointer.
	mov bx,bp
	sub bx,ax
	mov ss:-1[di],bl	;Put JMPS address beyond the ADDRESS table.
	lea di,ongoto
	cmpb congst,15
	je conloop
	lea di,ongosub
conloop jmp loop
ongoto	add ax,ax
	db 0bfh,tf,pnum 	;MOV DI,table.
	add di,ax
	jmp [di]
	db tf,tend
ongosub add ax,ax
	db 0bfh,tf,pnum 	;MOV DI,table.
	add di,ax
	call [di]
	db tf,tend

cshape	db tf,trun
	call skip2
	mov al,4
	cmpb [si],'"'
	jne csherr
	inc si
cshlp1	mov cl,1	;Set count to say bit 1.
cshlp	mov al,[si]
	inc si
	cmp al,'"'      ;Is it end quote yet?
	je cshend
	cmp al,' '
	je csb	;NC
	cmp al,9
	jne shastc
	push cx,si
	dec si
	call spctab
	pop si,bx
	jc csherr
	mov cl,ch
	sub ch,ch
	sub bp,cx	;Reset CODEADD.
	mov bh,cl
	mov cl,bl	;Restore put bits.
shtlp	shl cl,1
	jnc shtsp
	mov [bp],cl
	inc bp
	mov cl,1
shtsp	dec bh
	jnz shtlp
	jmps cshlp
shastc	stc
csb	rcl cl,1	;Add new bit into shape byte.
	jnc cshlp
	mov [bp],cl
	inc bp
	jmps cshlp1	;Start new byte.
cshend	mov spoint,si
	cmp cl,1	;Any bits set so far?
	je cshret
cshblnk clc
	rcl cl,1	;Setup new byte so it is left adjusted.
	jnc cshblnk
	mov al,cl
	jmp lput	;Put last byte.
csherr	stc
cshret	ret

cpage	db tf,tifnum
	db 0b8h,tf,pnum8,05
	mov [spage],al
	int 10h
	db tf,tend
	db tf,texp
	mov ah,5
	mov [spage],al
	int 10h
	db tf,tend

cvar	db tf,trun
cvar2	mov ah,1	;Signed.
	call avarv	;Just add variable into table, no code.
	jc cvcret
	call lpcomma
	jnc cvar2
	clc
cvcret	ret

cunsign db tf,trun
cuns2	mov ax,variabl	;AH=0, AL=variabl.
	call avar	;Just add variable into table, no code.
	jc cvcret
	call lpcomma
	jnc cuns2
	clc
	ret

cvar32	db tf,trun	;Add 32 bit variables.
cvar32a mov al,var32
	mov ah,1	;Signed 32 bit (future use?).
	call avar	;Just add variable into table, no code.
	jc cvcret
	addw varadd,2	;32 bits = 4 bytes.
cv32x	call lpcomma
	jnc cvar32a
cvcnc	clc
	ret

condi	dw 0
cconst	db tf,trun
cconst2 call aunique	;Add constant into table.
	jc cvcret
	mov condi,di
	call lpequal
	jc cvcret
	call decima1
	jc cvcret
	mov es,labseg
	mov di,condi
	movb es:1[di],1 ;Type label, treat in same way as label.
	cmpb specf,1
	jne cspecax
	mov si,ax
	mov ax,[si]	;Get the FAST variable concerned.
cspecax mov es:2[di],ax ;The value.
	call lpcomma
	jc cvcnc
	jmps cconst2

ccon32	db tf,trun
ccon32a call aunique	;Add constant into table.
	jc cvcret
	mov condi,di
	call lpequal
	jc cvcret
	call dec32
	jc cvcret
	mov es,labseg
	mov di,condi
	movb es:1[di],7 ;Type label, treat in same way as label.
	mov es:2[di],ax ;LOW
	mov es:4[di],dx ;HIGH
	call lpcomma
	jc cvcnc
	jmps ccon32a

cscroll db tf,tcomp,'down',0
	db tf,trun
	call skip2
	lea di,dscrdef
	jc csdefj
	cmp al,':'
	je csdefj
	cmp al,'}'
	je csdefj
	call yelse
	jc csdefj
	lea di,dscrful
csdefj	jmp csdef
dscrdef sub cx,cx
	mov dx,184fh
	mov ax,0701h
	mov bh,[scrcol]
	int 10h
	db tf,trun
	clc
	ret
dscrful db tf,texp,tf,tcomma	;SCROLL X1,Y1,X2,Y2,LINES
	push ax
	db tf,texp,tf,tcomma
	push ax
	db tf,texp,tf,tcomma
	push ax
	db tf,texp,tf,tcomma
	push ax
	db tf,texp
	pop dx
	pop bx
	mov dh,dl
	mov dl,bl
	pop bx
	pop cx
	mov ch,bl
	mov ah,7
	mov bh,[scrcol]
	int 10h
	db tf,stop

	db tf,trun
	call skip2
	lea di,scrdef
	jc csdef
	cmp al,':'
	je csdef
	cmp al,'}'
	je csdef
	call yelse
	jc csdef
	lea di,scrfull
csdef	jmp loop
scrdef	sub cx,cx
	mov dx,184fh
	mov ax,0601h
	mov bh,[scrcol]
	int 10h
	db tf,tend
scrfull db tf,texp,tf,tcomma	;SCROLL X1,Y1,X2,Y2,LINES
	push ax
	db tf,texp,tf,tcomma
	push ax
	db tf,texp,tf,tcomma
	push ax
	db tf,texp,tf,tcomma
	push ax
	db tf,texp
	pop dx
	pop bx
	mov dh,dl
	mov dl,bl
	pop bx
	pop cx
	mov ch,bl
	mov ah,6
	mov bh,[scrcol]
	int 10h
	db tf,tend

cdir	push ds 	;DIR path,buffer [,attribute]
	db tf,tname,tf,tcomma
	push dx
	db tf,tseg,tf,trun
	call lpcomma
	lea di,cdirax
	jnc cdatx	;Use default attribute.
	lea di,cdira0
cdatx	jmp loop
cdira0	sub cx,cx
	pop es
	pop dx
	db tf,hcall,xhdir
	pop ds
	db tf,tend

cdirax	push ax
	db tf,texp
	mov cx,ax
	pop ax
	pop es
	pop dx
	db tf,hcall,xhdir
	pop ds
	db tf,tend


cdim	db tf,tend

cdimb	db tf,tend

cmove	db tf,texp,tf,tword,7
	push ax
	db tf,tseg,tf,tword,3
	push ax
	db tf,tseg
	stc
	db tf,hcall,xmove
	db tf,tend

csppos	mov bp,[scrpos]
	db tf,tend

cmoveb	db tf,texp,tf,tword,7
	push ax
	db tf,tseg,tf,tword,3
	push ax
	db tf,tseg
	clc
	db tf,hcall,xmove
	db tf,tend

cfill
	db tf,tifnum,tf,tword,7
	db tf,savenum
	db tf,tseg
	db tf,tword,6
	db tf,loadnum
	db 0b9h,tf,pnum 	;mov cx,count
	db tf,tifnum
	pop es
	mov di,ax
	db 0b8h,tf,pnum 	;mov ax,number
	cld
	rep stosw
	db tf,tend

	db tf,treset
	db tf,texp,tf,tword,7
	push ax
	db tf,tseg
	push ax
	db tf,tword,6,tf,texp
	pop di
	pop es
	pop cx
	cld
	rep stosw
	db tf,tend

cfillb
	db tf,tifnum,tf,tword,7
	db tf,savenum
	db tf,tseg
	db tf,tword,6
	db tf,loadnum
	db 0b9h,tf,pnum 	;mov cx,count
	db tf,tifnum
	pop es
	mov di,ax
	db 0b0h,tf,pnum8	;mov al,number
	cld
	rep stosb
	db tf,tend

	db tf,treset
	db tf,texp,tf,tword,7
	push ax
	db tf,tseg
	push ax
	db tf,tword,6,tf,texp
	pop di
	pop es
	pop cx
	cld
	rep stosb
	db tf,tend

cexec	push ds
	db tf,tname
	push dx
	db tf,tcomma,tf,tseg
	mov bx,ax
	pop es
	pop dx
	mov [ssss],ss
	mov [sssp],sp
	mov ax,4b00h
	int 21h
	cli
	mov ss,[ssss]
	mov sp,[sssp]
	sti
	pop ds
	db tf,terrs
	db tf,tend

cload	db tf,tname,tf,tcomma
	mov ax,3d00h
	int 21h
	db tf,terrj
	push ds
	push ax
	db tf,tseg
	push ax
	db tf,trun
	call lpcomma
	lea di,ld1
	jc loadlp
	call loopexp	;Load optional number of bytes.
	jc cloadf
	movw [bp],0c189h	;MOV CX,AX
	add bp,2
	lea di,ld2
loadlp	jmp loop
cloadf	ret
ld1	mov cx,-1	;Load maximum of 64k-1 bytes.
ld2	pop dx
	pop ds
	pop bx
	mov ah,3fh
	int 21h
	pop ds
	db tf,terrj
	mov ah,3eh
	int 21h
	db tf,terrs
	db tf,tend

csave	db tf,tname,tf,tcomma
	sub cx,cx
	mov ah,3ch
	int 21h
	db tf,terrj
	push ds
	push ax
	db tf,tseg,tf,tcomma
	push ax
	db tf,texp
	mov cx,ax
	pop dx
	pop ds
	pop bx
	mov ah,40h
	int 21h
	pop ds
	db tf,terrj
	mov ah,3eh
	int 21h
	db tf,terrs
	db tf,tend


cmodify db tf,texp	;modify segment to new_paragraphs.
	db tf,tword,3
	push ax
	db tf,texp
	mov bx,ax
	pop es
	mov ah,4ah
	int 21h
	db tf,terrs
	db tf,tend

cdeall	db tf,texp
	mov es,ax
	mov ah,49h
	int 21h
	db tf,terrs
	db tf,tend

cswap	db tf,tvar,tf,tcomma,tf,trun
	mov ax,wvar
	mov wnum,ax
	call loopvar
	jc cswerc
	movb [bp],0a1h	;MOV AX,[V1]
	inc bp
	mov ax,wnum
	call usv
	movw [bp],0687h
	add bp,2
	mov ax,wvar
	call usv
	movb [bp],0a3h
	inc bp
	mov ax,wnum
	jmp usv
cswerc	ret

cwindow db tf,tcomp,'memory',0
	db tf,tnum,tf,trun
	mov ax,wnum
	mov windmem,ax
	clc
	ret
	db tf,tend

crandm	db tf,texp
	mov [rndn1],ax
	mov [rndn2],dx
	mov [rndn3],ax
	db tf,tend

cinpend db tf,tequal,tf,tnum,tf,trun
	mov al,wnum
	mov inpnnn,al	;Set new input end character.
	clc
	ret

copenm	db tf,tnum,tf,trun	;#open n
	mov al,wnum
	mov open1+1,al		;set current open mode value
	clc
	ret

chstack db tf,tcomp,'memory',0
	db tf,tnum,tf,trun
	mov ax,wnum
	mov stamem,ax
	clc
	ret
	db tf,tend

cstack	db tf,tequal,tf,texp
	mov sp,ax
	db tf,tend

ctimer	db tf,tequal,tf,texp
	mov dx,ax
	mov ah,1
	int 1ah
	db tf,tend

cone	movw [runerra],0
	jmp start
	db tf,trun
	mov -5[bp],bp
	mov al,0c3h
	jmp offax

ontrace movw [tracev],0
	jmp start
	db tf,trun
	mov -5[bp],bp
	mov al,0cbh	;RETF (to FASTT)
	mov bl,doption
	push bx
	movb doption,0	;Turn trace and debug off.
	call offax
	pop bx
	mov doption,bl
	ret

ondebug movw [debugv],0
	jmp start
	db tf,trun
	mov -5[bp],bp
	mov al,0c3h	;RET
	mov bl,doption
	push bx
	movb doption,0	;Turn trace and debug off.
	call offax
	pop bx
	mov doption,bl
	ret

cserr	movw [runerra],0
	db tf,trun
	sub bp,2
	mov cl,xwclose
	mov al,onelibo
	jmp hfncsu	;Find address of main procedure.

cwait	db tf,tcomp,'for',0
	db tf,trcode,5,tf,texp
	or ax,ax
	db tf,trun
	mov cl,5
waitf3	call getclp
	jc cwerr
	sub ax,bp
	cmp ax,-127
	jb wlong
	movb [bp],74h
	inc bp
	sub al,2
	jmp lput

wlong	movw [bp],0375h
	movb [bp+2],0e9h
	add bp,3
	sub ax,5
	jmp lpute2
cwerr	stc
	ret
	db tf,tend

cwhile	db tf,trcode,3,tf,texp
	or ax,ax
	jnz cwhile2
	db 0e9h,tf,trcode
cwhile2 db 3,0,0,tf,tstat,0e9h,tf,trun
	mov cl,3
	call getclp
	jc cwerr
	push ax

	mov cl,3
	call getclp
	pop dx
	jc cwerr
	sub ax,bp
	cmp ax,-127
	jb whlong
	movb [bp-1],0ebh
	dec al
	mov [bp],al
	inc bp
	jmps cwhs

whlong	dec ax
	dec ax
	call lpute2

cwhs	mov ax,dx
	jmp noel2

cforev	db tf,trcode,7
	db tf,tstat,tf,trun
forend	mov cl,7
	call getclp
	jnc cnodf
	cmp ch,11	;ENDFOR?
	jne cwerr
	mov bx,[di]
	mov ax,bp
	sub ax,bx
	add ax,3		;Skip over the jump back to start of forever.
	mov ss:[bx-2],ax	;Endfor jump.
	jmps forend
cnodf	mov defd,1		;defined!
	jmp nodret

csound	db tf,texp,tf,tcomma
	mov [count],ax
	db tf,texp
	out 42h,al
	mov al,ah
	out 42h,al	;Set frequency.
	in al,61h
	or al,3
	out 61h,al	;Start the speaker.
	db tf,trun
	movb [hflags+xhfint],1
	ret

cnoise	db tf,tcomp,'off',0
	in al,61h
	and al,252
	out 61h,al
	db tf,stop

	db tf,texp,tf,tcomma
	push ax
	db tf,texp
	out 42h,al
	mov al,ah
	out 42h,al
	in al,61h
	or al,3
	out 61h,al
	pop cx
noiselp mov al,10
noisel2 dec al
	jnz noisel2
	loop noiselp	;Delay of 10000th of a second.
	db tf,tend

cout	db tf,tifnum,tf,tcomma,tf,trun
	push spoint
	push wnum
	call loopnum
	pop dx
	pop bx
	jc cout2
	mov di,offset(out2)
	jmp loop
cout2	mov spoint,bx
	push dx
	call loopexp
	pop dx
	jc outerr
	movb [bp],0bah
	mov [bp+1],dx
	movb [bp+3],0eeh
	add bp,4
	ret
out2	db 0bah,tf,pnum2
	db 0b0h,tf,pnum8
	out dx,al

	db tf,tend,tf,texp
	push ax
	db tf,tcomma,tf,texp
	pop dx
	out dx,al
	db tf,tend
outerr	ret

csprite db tf,texp,tf,tcomma
	push ax
	db tf,texp,tf,tcomma
	push ax
	db tf,tseg
	db tf,hcall,xspent
	db tf,tend

cmode	movb [modew],0
	movb [modeop],0
	db tf,trun
	call decima1
	jc modee
	mov bx,ax
	sub al,al
	cmp bx,640
	je modsz
	inc al
	cmp bx,320
	je modsz
	mov al,6
	cmp bx,720
	jne modee
	mov al,2
modsz	mov -6[bp],al
	call lpcomma
	jc modee
	call rlook
	jc modee
	mov al,6
	cmp ah,9
	jb modee
	cmp ah,11
	ja modee
	sub ah,9
	mov -1[bp],ah
modek	clc
	ret
modee	stc
modeo	ret

cinputs db tf,texp
	mov di,ax
	db tf,hcall,xgetiii,tf,tend

cerror	db tf,tcomp,'@',0,tf,trun
	jmp colerrj
	db tf,stop

	db tf,tcomp,'msg',0
	db tf,tname,tf,trun
	call lpcomma
	lea di,erm_def	;See details after CERROR.
	jc erm_lp
	movb [bp],52h	;PUSH DX
	inc bp
	call loopexp
	jc modeo
	movb [bp],5ah	;POP DX
	inc bp
	lea di,erm_ax
erm_lp	jmp loop
	db tf,stop

	db tf,trun
	movb [hflags+xhfrun],1	;Say using the ERROR library.
	call loopexp
	lea di,err_n
	jnc errlpc
	cmp al,30
	jne modee
	lea di,err_def	;Just use default error variable.
errlpc	jmp loop
	stc	;Use error.
	ret
err_n	mov [errorv],ax
err_def call [runerra]
	db tf,tend

erm_ax	mov [errorv],ax
erm_def db tf,hcall,xermsg
	db tf,tend

cread	db tf,trun
crnrd	call rdvar
	jc crxret
	call lpcomma
	jc crxnc
	jmps crnrd
rdvar	mov ah,1
	call avarv
	jc crxret
	mov di,offset(read2)
	jmp loopz
read2	les di,[restoff]
	mov ax,es:[di]
	db 0a3h,tf,pvar
	addw [restoff],2
	db tf,tend

crxnc	clc
crxret	ret
creadb	db tf,trun
crnrdb	call rdbvar
	jc crxret
	call lpcomma
	jc crxnc
	jmps crnrdb
rdbvar	mov ah,1
	call avarv
	jc crxret
	mov di,offset(read3)
	jmp loopz
read3	les di,[restoff]
	mov al,es:[di]
	sub ah,ah
	incw [restoff]
	db 0a3h,tf,pvar
	db tf,tend

crestor db tf,texp,tf,trun
	call skip2
	cmp al,pipe
	jne restofs
	push bp
	mov ax,spoint
	incw spoint	;Remember original SPOINT.
	push ax
	call loopexp
	pop bx
	pop bp
	mov spoint,bx
	jc restofs
	incw spoint	;Increment past colon.
	mov di,offset(restor2)
	jmp loopz
restor2 mov [restseg],ax
	db tf,texp
	mov [restoff],ax
	db tf,tend
restofs mov di,offset(restor1)
	jmp loopz
restor1 mov [restoff],ax
	mov [restseg],ds
	db tf,tend

creg	db tf,trun
creglp	call findreg	;Is a valid register given?
	jc cregerr
	call lpequal
	jc cregerr

	push di 	;Save register address.
	call loopexp	;Get expression.
	pop di
	jc cregerr

	mov ax,[di]	;Get transferr instruction.
	or al,al
	jz cregn	;Ignore if dealing with AX.
	cmp al,0ffh
	jne cregput
	mov al,20	;Cannot set register CS!!!
	stc
	ret
cregput mov [bp],ax
	add bp,2
cregn	call lpcomma
	jc cregok
	jmps creglp
cregok	clc
cregerr ret

csres	db 0bah 	;MOV DX,last address.
	db tf,trun
	mov al,onetot
	call hfncsu
	movw [bp],27cdh
	add bp,2
	ret
totale	dw 0		;Allow for program plus variables data.

cclocks mov al,36h	;Set clocks timer for INT 08.
	out 43h,al
	jmp clklow
clklow	db tf,texp
	out 40h,al
	jmp clkhigh
clkhigh mov al,ah
	out 40h,al
	db tf,tend

cbeep	mov ah,6
	mov dl,7
	int 21h
	db tf,tend

cfind	db tf,tcomp,'first',0
	db tf,tname
	mov ah,4eh
	sub cx,cx
	int 21h
	db tf,terrs
	db tf,stop

	db tf,tcomp,'next',0
	mov ah,4fh
	int 21h
	db tf,terrs
	db tf,tend

cchange db tf,tcomp,'dir',0,tf,tname
	mov ah,3bh
	int 21h
	db tf,terrs
	db tf,stop

	db tf,tcomp,'disk',0,tf,texp
	mov dl,al
	mov ah,0eh	;change disk b (0 - ).
	int 21h
	db tf,tend

cmake	db tf,tcomp,'dir',0,tf,tname
	mov ah,39h
	int 21h
	db tf,terrs
	db tf,tend

cremove db tf,tcomp,'dir',0
	db tf,tname
	mov ah,3ah
	int 21h
	db tf,terrs
	db tf,tend

cdelete db tf,tname
	mov ah,41h
	int 21h
	db tf,terrs
	db tf,tend

crename db tf,tname,tf,tword,3	;name TO name
	push dx
	db tf,tname
	mov di,dx
	mov ax,cs
	mov es,ax
	pop dx
	mov ah,56h
	int 21h
	db tf,terrs
	db tf,tend

cwrite	push ds
	db tf,thandle,tf,tcomma
	push bx
	db tf,texp,tf,tword,7
	push ax
	db tf,tseg,tf,tpopds
	mov dx,ax
	pop cx
	pop bx
	mov ah,40h
	int 21h
	pop ds
	db tf,terrs
	db tf,tend

chandle db tf,thget,tf,tcomma
	push di
	db tf,texp
	pop di
	mov [di],ax
	db tf,tend

copen	db tf,tcomp,'window',0
cow	db tf,texp,tf,hcall,xwopen,tf,terrs,tf,stop

	db tf,thget,tf,tcomma
	push di
	db tf,trun
	call name
	jc openrr
	call lpcomma
	lea di,open1
	jc opend1
	lea di,open2
opend1	jmp loop
open1	mov ax,3d02h
	int 21h
	pop di
	mov [di],ax
	movw [di+2],1
	db tf,terrs
	db tf,tend

open2	push dx
	db tf,texp
	pop dx
	push ax
	mov ax,3d02h
	int 21h
	pop di
	mov [di],ax
	pop [di+2]
	db tf,terrs
	db tf,tend
openrr	ret

ccreate db tf,thget,tf,tcomma
	push di
	db tf,trun
	call name
	jc openrr
	call lpcomma
	lea di,nopen1
	jc nopend1
	lea di,nopen2
nopend1 jmp loop
nopen1	sub cx,cx
	mov ah,3ch
	int 21h
	pop di
	mov [di],ax
	movw [di+2],1	;Length = 1.
	db tf,terrs
	db tf,tend

nopen2	push dx
	db tf,texp
	pop dx
	push ax
	mov ah,3ch
	sub cx,cx
	int 21h
	pop di
	mov [di],ax
	pop [di+2]	;Length.
	db tf,terrs
	db tf,tend

ccwlib	movb [bp],0e8h
	inc bp
	mov al,onelibo
	jmp hfncsu	;Find address of main procedure.

cclose	db tf,tcomp,'windows',0
	db tf,hcall,xwclosa,tf,stop

	db tf,tcomp,'window',0
	db tf,hcall,xwclose,tf,terrs,tf,stop

clfile	db tf,thandle
	mov ah,3eh
	int 21h
	db tf,terrs
	db tf,trun
	call lpcomma
	jc nccl
	lea di,clfile
	jmp loop
nccl	clc
	ret

creclen db tf,thget,tf,tcomma
	push di
	db tf,texp
	pop di
	mov 2[di],ax		;Set record length of file.
	db tf,tend

cseek	db tf,thget,tf,tcomma
	db tf,tcomp,'eof',0
	mov bx,[di]		;Handle.
	mov ax,4202h		;Seek end of file.
	sub cx,cx
	mov dx,cx		;plus offset 0.
	int 21h
	db tf,terrs		;Errors?

	db tf,stop
	push [di]
	db tf,texp32
	pop bx
	db tf,hcall,xseek
	db tf,tend

cterm	db tf,hcall,xexit,tf,tend

cscreen db tf,tnum8
	db 0b8h,tf,pnum8,0	;MOV AX,00nnh
	int 10h
	db tf,tend
	db tf,texp
	sub ah,ah
	int 10h
	db tf,tend

cstring db tf,notto
	db tf,trun
	call decima1
	jc csteg
	sub ah,ah
	mov [bp],ax
	add bp,2
	jmp csplps
csteg	ret

conint	db 0c7h,6	;Set MITAB(0).
	db tf,tnum,tf,trun
	mov ax,wnum
	cmp ax,1
	jb conoe
	cmp ax,10
	jbe coninto
conoe	mov al,1
	stc
	ret
coninto dec ax
	add ax,ax
	push ax
	mov cl,xtabint
	mov al,onelib
	call hfncsu
	pop [bp-2]

	mov ax,bp
	add ax,5
	mov [bp],ax
	movb [bp+2],0e9h
	add bp,5		;Leave space for JMP
	mov al,0c3h
	jmp offax		;Finish it.

cstint	movw [2000],0
	db tf,tnum,tf,trun
	mov ax,wnum
	cmp ax,1
	jb conoe
	cmp ax,10
	ja conoe

csttp0	dec ax
	add ax,ax
	mov [bp-4],ax
	mov cl,xtabint
	mov al,onelib
	call hfncsu
	sub bp,2
	subw es:[di+1],4
	clc
	ret


;LPRINT routines

lcp	db tf,trun
	mov cx,xhflp
prt_l	lea bx,ldppp
	lea di,lprttab
	jmp phandle

lcpb	db tf,trun
	mov cx,xlprinb
	jmps prt_l

lcph	db tf,trun
	mov cx,xlprinh
	jmps prt_l

lcphb	db tf,trun
	mov cx,xlpdigh+256
	jmps prt_l

semi	db ?
prt_bx	dw ?
prt_cx	dw ?
prt_di	dw ?
phandle mov prt_bx,bx
	mov prt_cx,cx
	mov prt_di,di
lpcom2	mov semi,0
lpcom3	mov di,prt_di
	call findnc
	jc lpcome
	mov di,[di+1]		;Use print sub-command.
	call loopz
	jnc lpcom2
lpret	ret
lpcome	call skip2
	jc ldefend
	cmp al,':'              ;Either end of line or statement?
	jne lpcomc
ldefend cmpb [si],';'           ;Was last character a semi-colon?
	jne ldownln
	mov semi,1
	incw spoint
	jmps lpcom3
ldownln cmp semi,1
	je lpret
	mov di,prt_bx	;NEWLINE!
	jmp loopz
lpcomc	call resvet
	jnc ldownln
	call loopexp	;Check expression.
	jc lpret
	mov cx,prt_cx
	or ch,ch
	jz lpco
	movw [bp],0e428h
	add bp,2
lpco	call ccwlib
	jmps lpcom2

ldppp	mov ax,13
	sub dx,dx
	int 17h
	mov ax,10
	int 17h
	db tf,tend


;FAST SCREEN handling routines

cp	db tf,tcomp,'bios',0
	db tf,trun
dos1	mov cx,xhfpb
prt_b	lea bx,scrbios
	lea di,bprttab
	jmp phandle

	db tf,stop,tf,tcomp,'dos',0,tf,trun
	jmps dos1

	db tf,stop
	db tf,trun
	mov cx,xhfp
prt_p	lea bx,prtlib
	lea di,prttab
	jmp phandle

cpb	db tf,tcomp,'bios',0
	db tf,trun
dos2	mov cx,xbpb
	jmps prt_b

	db tf,stop,tf,tcomp,'dos',0,tf,trun
	jmps dos2

	db tf,stop
	db tf,trun
	mov cx,xprintb
	jmps prt_p

cph	db tf,tcomp,'bios',0
	db tf,trun
dos3	mov cx,xbprinh
	jmps prt_b

	db tf,stop,tf,tcomp,'dos',0,tf,trun
	jmps dos3

	db tf,stop
	db tf,trun
	mov cx,xprinth
	jmps prt_p

cphb	db tf,tcomp,'bios',0
	db tf,trun
dos4	mov cx,xbpdigh+256
	jmps prt_b

	db tf,stop,tf,tcomp,'dos',0,tf,trun
	jmps dos4

	db tf,stop
	db tf,trun
	mov cx,xpdigh+256
	jmps prt_p

prtlib	db tf,trun
	mov cl,xtestsc
	jmp ccwlib


;SUPER FAST SCREEN handling routines

scp	db tf,trun
	mov cx,xhfsp
prt_s	lea bx,scfl
	lea di,sprttab
	jmp phandle

scpb	db tf,trun
	mov cx,xsprinb
	jmps prt_s

scph	db tf,trun
	mov cx,xsprinh
	jmps prt_s

scphb	db tf,trun
	mov cx,xspdigh+256
	jmps prt_s

scfl	db tf,trun
	mov cl,xsptest
	jmp ccwlib

scrbios mov ah,6
	mov dl,13
	int 21h
	mov ah,6
	mov dl,10
	int 21h
	db tf,tend

ccls	mov ax,0600h
	sub cx,cx
	mov dh,[fscreen_rows1]
	dec dh
	mov dl,[fscreen_cols1]
	dec dl
	mov bh,[scrcol]
	int 10h
	db tf,tend

ccolour db tf,tcomp,'@',0
	db tf,trun
colerrj mov si,colerr
	mov spoint,si
	jmp dofin2
	db tf,stop

	db tf,tifnum,0c6h,6
	dw scrcol
	db tf,pnum8,tf,tend
	db tf,texp
	mov [scrcol],al
	db tf,tend

cplot	db tf,tifvar,tf,trun
	call lpcomma
	jc cpltc
	mov ax,wvar
	push ax
	call loopifv
	jc novv
	cmp al,99
	je novv
	movw [bp],168bh
	add bp,2
	pop ax
	call usv
	movw [bp],1e8bh       ;MOV BX,[y]
	add bp,2
	call lpvar
	mov di,offset(plot)
	jmp loop
novv	pop ax
	lea di,cpcomp
	jmp loop
	db tf,tend
cpcomp	db tf,treset,tf,texp
	push ax
	db tf,tcomma,tf,texp
	mov bx,ax
	pop dx
plot	db tf,hcall,xplot
	db tf,tend
cpltc	ret

clocate db tf,tifvar,tf,tcomma,tf,trun
	push wvar
	call loopifv
	pop dx
	jc clocnv
	cmp al,99
	je clocnv
	movw [bp],268ah
	add bp,2
	mov ax,dx
	call usv
	movb [bp],0a0h
	inc bp
	mov ax,wvar
	call usv
	jmps cclc	;Put the call in.
clocnv	call restsp
	mov di,offset(clocn2)
	jmp loop
	db tf,tend
clocn2	db tf,tifnum,tf,tcomma,tf,tifnum
	movw [scrpos],0
	db tf,trun

	mov dl,wnum
	mov dh,wnum2
	mov al,160	;Locate AX.
	mul dh
	sub dh,dh
	add ax,dx
	add ax,dx

	mov -2[bp],ax
	clc
	ret

cclc	mov cl,xhfloc
	jmp ccwlib

	db tf,tend,tf,treset
	db tf,texp,tf,tcomma
	db tf,tifnum
	mov ah,al
	db 0b0h 	;MOV AL,n
	db tf,pnum8,tf,trun
	jmps cclc
	db tf,tend
	push ax
	db tf,texp
	pop bx
	mov ah,bl
	db tf,trun
	jmps cclc

ccursor db tf,tcomp,'size',0
	db tf,texp,tf,tcomma
	push ax
	db tf,texp
	pop cx
	mov ch,cl
	mov cl,al
	mov ah,1
	int 10h
	db tf,stop

	db tf,tifvar,tf,tcomma,tf,trun
	mov ax,wvar
	push ax
	call loopvar
	pop dx
	jc ccurnv
	push dx
	call endxp
	pop dx
	jc ccurnv
	cmp al,99
	je ccurnv
	movw [bp],268ah
	add bp,2
	mov ax,dx
	call usv
	movb [bp],0a0h
	inc bp
	mov ax,wvar
	call usv
	jmps ccc	;Put the call in.
ccurnv	call restsp
	mov di,offset(ccurn2)
	jmp loop
	db tf,tend
ccurn2	db tf,tifnum,tf,tcomma,tf,tifnum,tf,trun
	movb [bp],0b8h
	mov al,wnum
	mov ah,wnum2
	mov [bp+1],ax
	add bp,3
ccc	mov cl,xcpos
	jmp ccwlib

	db tf,tend,tf,treset
	db tf,texp,tf,tcomma
	db tf,tifnum
	mov ah,al
	db 0b0h,tf,pnum8,tf,trun
	jmps ccc
	db tf,tend
	push ax
	db tf,texp
	pop bx
	mov ah,bl
	db tf,trun
	jmps ccc

ccurpos db tf,tequal,tf,texp
	db tf,trun
	jmps ccc

cint	db tf,thex
	db tf,trun
	mov al,wnum
	cmp al,3
	jnz ciuse
	movw [bp],11001100b
	inc bp
	ret
ciuse	movb [bp],0cdh
	mov [bp+1],al
	add bp,2
	ret

yelse	push spoint
	call rlook
	pop spoint
	jc nelse
	cmp al,2	;ELSE?
	jne nelse
ciose	stc	;Carry means found (Hence end of statement).
ciosr	ret
nelse	clc		;Not found.
	ret

creturn db tf,tcomp,'error',0
	db tf,trun
	cmpb hflags+xhfint,0	;Timer tick interupt used?
	lea di,creren
	jz crnlp
	lea di,creres
crnlp	jmp loop
creres	lds dx,[oldoff]
	mov ax,2508h
	int 21h 	;Carry on to use cstop table.
creren	db tf,texp
	mov ah,4ch
	int 21h
	db tf,stop

	db tf,trun
	call skip2
	jc retnexp
	cmp al,':'
	je retnexp
	cmp al,'}'
	je retnexp
	call yelse
	jc retnexp

	call loopexp	;Must be expression or error.
	jc ciosr
	mov al,0c3h	;RET
	jmp lput

retnexp mov ax,lastpf
	add ax,3
	cmp ax,bp
	jne retnorm
	movb [bp-3],0e9h	;JMP funt/gosub
	ret

retnorm mov al,0c3h	;RET
	jmp lput
	db tf,tend

repnest dw 0
rnest	ds 150		;Allow for 50 levels of nesting.

crepcr	stc
	ret
crepeat db tf,texp
	mov [0],ax
	mov ax,0
	db tf,trcode,1,tf,tstat,0ffh,0eh,tf,trun
	mov cl,1
	call getclp
	jc crepcr
	mov assvar,ax
	mov es,codeseg
	mov di,ax
	dec ax
	dec ax
	mov es:[di-5],ax	;Start value address.
	mov [bp],ax
	add bp,2

	mov ax,bp
	sub ax,assvar
	cmp ax,126
	ja relong
	movb [bp],75h
	inc bp
	mov ah,254
	sub ah,al
	mov al,ah
	jmp lput
relong	movw [bp],0374h ;JZ 3
	add bp,2
	mov defd,1		;defined!
	mov ax,assvar
	jmp nodret

cfor	db tf,trun	;Syntax is FOR var = nn TO nn [STEP nn]
	call skip2
	call dofin2
	jnc cforset
	cmp al,9	;If excess characters then stopped at the "TO"
	jne cforc1

cforset call rlook	;Must find the word "TO" (in AH).
	jc cforc
	cmp ah,3
	je cfs
	mov al,21
	jmps cforc1
cfs	call loopexp	;End value.
	jc cforc
	movb [bp],0a3h	;MOV [nn],AX
	inc bp
	mov cl,2
	call ltcc0	;Address within code of address of end value.
	mov [bp],ax
	add bp,2	;The address nn.

	call rlook	;STEP?
	jc fstep1
	cmp ah,4
	je fstepx
	mov al,21
	jmps cforc1
fstep1	mov ax,1
fcomn	mov cl,2
	call ltcc1
	jc cforc
	mov cl,2
	jmp ltcc0	;Store the address to jump back to.
fstepx	call loopexp	;MOV AX,step
	movb [bp],0a3h
	inc bp
	mov cl,2
	call ltcc0	;Address within code of address of step.
	mov [bp],ax
	add bp,2	;The address nn.
	mov ax,2
	jmps fcomn
cforc1	stc
cforc	ret

cnext	db tf,trun
ncomma	call loopvar
	jc cforc1
	mov cl,2
	call getclp
	jc cforc
	mov assvar,ax	;Store the JMP address.
	mov cl,2
	call getclp
	jc cforc
	cmp ax,1
	jne nextznc
	jmp nextinc
nextznc movw [bp],0681h
	add bp,2
	mov ax,wvar
	call usv
	mov cl,2
	call getclp
	jc cforc
	mov di,ax
	mov ss:[di],bp	;	    step address
	mov [bp],ax
	add bp,2	;The address nn.
nxcomn	movw [bp],03e81h	;CMPW [var],end value
	add bp,2
	mov ax,wvar
	call usv
	mov cl,2
	call getclp
	jc cforc
	mov di,ax
	mov ss:[di],bp	;end value addres.
	mov [bp],ax
	add bp,2	;The address nn.

	mov ax,bp
	sub ax,assvar
	cmp ax,126
	ja nxlong
	movb [bp],76h
	inc bp
	mov ah,254
	sub ah,al
	mov [bp],ah
	inc bp
	jmps nxcomc

nxlong	movw [bp],0377h ;JA 3
	add bp,2
	mov defd,1		;defined!
	mov ax,assvar
	call nodret

nxcomc	call lpcomma	;Another variable?
	jc nonnc
	jmp ncomma
nonnc	clc
	ret
nextinc movw [bp],06ffh
	add bp,2
	mov ax,wvar
	call usv
	jmps nxcomn

chalt	db 0f4h,tf,tend

cobrace call lpstat
	jnc cobrace	;Error or EOF?
	cmp al,7
	je cobra

	or al,al	;syntax?
	jnz cofuck
	mov si,spoint
	cmpb [si],'}'	;Close brace?
	je cobra	;Fucked - if syntax error then it's ignored!

cofuck	stc
	jmps cbcerc	;call comperr	;Display 'real' error.
cobra	call nextch
	jc cbcerr
	mov si,spoint
	cmpb [si],'}'   ;Close brace?
	je cbcdok
	mov al,16
cbcerc	stc
cbcerr	ret
cbcdok	incw spoint	;Next character.
	ret

cgoto	db tf,trun
	inc bp		;Say will be used in next byte.
	mov dmust,0
	mov omust,oneadd
	movb defd,1	;defined = default
	call getlab
	dec bp
	jnc nodret
cg14	mov al,14
	ret
nodret	mov dx,ax
	mov al,0e9h
cglab	mov [bp],al
	inc bp
	mov ax,dx
	mov dx,bp
	inc dx
	inc dx		;Point to two bytes after call/jump instruction.
	sub ax,dx	;Label address - next instruction address.

	cmp defd,1	;defined?
	jne lpuj2
	cmp ax,-127	;can we do a short jump?
	jl lpuj2
	cmp ax,126
	jg lpuj2
	inc ax
	movb [bp-1],0ebh	;jmps nn
	mov [bp],al
	inc bp
	clc
	ret

lpuj2	jmp lpute2

cgosub	db tf,trun
	inc bp
	mov dmust,0
	mov omust,oneadd
	call getlab
	dec bp
	jc cg14
	mov dx,ax
	mov al,14
	jc cbcerr
cgcall	mov al,0e8h
	mov cs:lastpf,bp
	mov defd,0		;UNdefined!
	jmps cglab

ccall	db tf,texp
	db tf,tchr
pipem1	db '|'
	mov [hitw2],ax	;Segment.
	db tf,texp
	mov [hitw1],ax	;Offset.
	callf [hitw1]
	db tf,stop
	call ax
	db tf,tend

cjump	db tf,texp
	db tf,tchr
pipem2	db '|'
	mov [hitw2],ax	;Segment.
	db tf,texp
	mov [hitw1],ax	;Offset.
	jmpf [hitw1]
	db tf,stop
	jmp ax
	db tf,tend

cbrk	int 23h
	db tf,tend	;Invoke the break handler.

cenable db tf,tcomp,'break',0
	push ds
	lds dx,[oldboff]
	mov ax,2523h
	int 21h 	;Set break pointer to previous break vector.
	pop ds
	db tf,stop

	db tf,tcomp,'interupts',0
	sti
	db tf,stop

	db tf,tcomp,'serial',0
	db tf,trun
	call lpcomma
	lea di,serlnc
	jc sernoc
	lea di,serlc
sernoc	jmp loop
serlnc	sub al,al
	db tf,hcall,xsinit
	db tf,tend
serlc	db tf,texp
	db tf,hcall,xsinit
	db tf,tend

creset	db tf,tcomp,'interupts',0
	db tf,trun
	sub cl,cl
	cmpb hflags+xhfint,0
	je resc2
	lea di,reso1
	call loop
	mov cl,1
resc2	cmpb hflags+xikeyit,0
	je resr
	lea di,reso2
	call loop
	mov cl,1
resr	or cl,cl
	jz nodset
	movw [bp],0c88ch	;MOV AX,CS
	movw [bp+2],0d88eh	;MOV DS,AX
	add bp,4
nodset	ret
reso1	lds dx,[oldoff]
	mov ax,2508h
	int 21h
	db tf,tend
reso2	lds dx,[ikeyoff]
	mov ax,2509h
;	int 21h 	;Reset keyboard interupt.
	db tf,tend

cdisbrk db tf,tcomp,'break',0
	mov dx,hfbreak
	mov ax,2523h
	int 21h 	;Set break to just iret.
	db tf,stop

	db tf,tcomp,'interupts',0
	cli
	db tf,stop

	db tf,tcomp,'serial',0
	db tf,hcall,xsstop
	db tf,tend

ctest	db tf,tcomp,'break',0
	mov ah,0bh
	int 21h 	;Check break status.
	db tf,tend

conbrk	mov dx,1000	;Offset for break put here.
	mov ax,2523h
	mov [oldboff],dx
	mov [oldbseg],ds
	int 21h 	;Normal procedure for setting interupt.
	jmp start	;Jump over it.
	db tf,trun
	mov [bp-18],bp	;Put address of the break code in program.
	mov al,0cfh	;IRET instruction.
	jmp offax

cdata	db tf,notto
	db tf,trun
	call lpnd
	jc cdret	;Is there a starting valid number?
	call lpnum
	jc cdret
cdlp	call lpcomma
	jc cdreto
	mov si,spoint
	cmpb [si],'?'
	jne cmkc
	incw spoint
	mov [bp],ax
	add bp,2
	jmps cdlp
cmkc	call lpnd
	jc cdret
	call lpnum	;Put if successful number (AX).
	jnc cdlp
cdret	stc
	ret	;Error, no digit valid.
cdreto	clc
	ret

cfname	db tf,notto
	db tf,trun
	call cdatabj
	jc cdret
	sub al,al
	jmp lput	;ASCIIZ

cdatab	db tf,notto
	db tf,trun
cdatabj call cdsin
	jnc cdblp
	call lpnd
	jc cdret	;Is there a starting valid number?
	call lpnum8
	jc cdret
cdblp	call lpcomma
	jc cdreto
	call cdsin
	jnc cdblp
	call lpnd
	jc cdret
	call lpnum8	;Put if successful number (AX).
	jnc cdblp
	jmps cdret

cendc	db tf,trun
	lea di,cterm
	call loopz
	pop ax		;Drop DOLINE call
	jmp compend	;End compile/include file.

cpush	db tf,tifvar
	db 0ffh,36h,tf,pvar
	db tf,trun
	jmps ntpush
	db tf,tend
	db tf,texp
	push ax
	db tf,trun
ntpush	call lpcomma
	jc cppcm
	lea di,cpush
	jmp loop

cpop	db tf,trun
cpopnx	mov ah,1
	call avarv
	jc cpope
	movw [bp],068fh
	add bp,2
	call lpvar
	jc cpope
	call lpcomma
	jnc cpopnx
cppcm	clc
cpope	ret

cflash	db tf,tifnum
	orb [scrcol],128
	db tf,trun
	mov al,wnum
	cmp al,1
	ja erange
	je efin
	mov bx,267fh
efbcom	mov -1[bp],bl
	mov -4[bp],bh
efin	clc
	ret
	db tf,tend,tf,texp
	andb [scrcol],127
	or al,al
	jz cfno
	orb [scrcol],128
cfno
	db tf,tend
erange	mov al,1
	stc
	ret

cbright db tf,tifnum
	orb [scrcol],8
	db tf,trun
	mov al,wnum
	cmp al,1
	ja erange
	je efin
	mov bx,26f7h
	jmps efbcom

	db tf,tend,tf,texp
	andb [scrcol],248
	or al,al
	jz cbno
	orb [scrcol],8
cbno
	db tf,tend

cink	db tf,tifnum
	andb [scrcol],248
	db 80h,0eh
	dw scrcol
	db tf,pnum8

	db tf,tend,tf,texp
	andb [scrcol],248
	or [scrcol],al
	db tf,tend

cpaper	db tf,tifnum
	andb [scrcol],143
	orb [scrcol],0
	db tf,trun
	mov al,wnum
	shl al,1
	shl al,1
	shl al,1
	shl al,1
	mov -1[bp],al
	clc
	ret

	db tf,tend,tf,texp
	andb [scrcol],143
	shl al,1
	shl al,1
	shl al,1
	shl al,1
	or [scrcol],al
	db tf,tend

casciiz db tf,tifnd,2
	db 8ah,1eh,tf,pnum,tf,trun
	addw [bp-2],1	;[label+1]
	movw [bp],0ff28h
	add bp,2
	mov si,spps
	mov spoint,si
	lea di,casc2
	jmp loop
casc2	db tf,tifnd,2
	db 0c6h,87h,tf,pnum,0	;MOVB [BX+label+2],0
	db tf,trun
	addw [bp-3],2
	clc
	ret

	db tf,tend
	db tf,texp	;ASCIIZ expression.
	mov di,ax
	mov al,1[di]
	sub ah,ah
	add di,ax
	movb 2[di],0
	db tf,tend

cfree	db tf,tcomp,'memory',0
	mov ax,cs
	mov es,ax
	mov ah,4ah
	mov bx,0
	int 21h
	db tf,trun
	mov al,onetot
	sub bp,4
	call hfncsu
	add bp,2	;Skip over 'INT 21h'
	movb [bp-4],'f' ;See FASTI-ONELP for ONETOT('f')
	jmp lperrs
	db tf,stop

	db tf,tcomp,'all memory',0
	mov ax,cs
	mov es,ax
	mov ah,49h
	int 21h
	db tf,terrs
	db tf,tend

ifvth	mov al,21
	stc
ifvno	ret

cif	db tf,trun
	call skip2
	mov spps,si

	cmp irange,0
	je iflong
	jmp ifshort

;== #LONG if ===============================================================

iflong	call loopvar
	jnc ciffi
ciff2c	jmp cifc2
ciffi	call skip
	mov ax,[si]
	cmp al,'='
	je cifee
	cmp ax,256*'>'+'<'
	jne cifnez
	jmp cifne
cifnez	cmp al,'<'
	je cifll
	cmp al,'>'
	jne ciff2c
	incw spoint
	call loopifn
	cmp al,99
	je ciff2c
	lea di,cifgg2
	jmp loop
	db tf,tend
cifgg2	db 81h,3eh,tf,pvar,tf,pnum
	db tf,tisign,7fh,77h
	db 0e9h,tf,trcode
cifggd	db 4
	dw 0
	db tf,trun
	jmp ifcome

cifee	incw spoint
	call loopifn
	cmp al,99
	je cifc2
	lea di,cifee2
	jmp loop
cifee2	db 81h,3eh,tf,pvar,tf,pnum
	je cifeed
	db 0e9h,tf,trcode
cifeed	db 4
	dw 0
	db tf,trun
	jmp ifcome

cifll	incw spoint
	call loopifn
	cmp al,99
	je cifc2
	lea di,cifll2
	jmp loop
cifll2	db 81h,3eh,tf,pvar,tf,pnum
	db tf,tisign,7ch,72h
	db 0e9h,tf,trcode
ciflld	db 4
	dw 0
	db tf,trun
	jmps ifcome

cifne	addw spoint,2
	call loopifn
	cmp al,99
	je cifc2
	lea di,cifne2
	jmp loop
cifne2	db 81h,3eh,tf,pvar,tf,pnum
	jne cifned
	db 0e9h,tf,trcode
cifned	db 4
	dw 0
	db tf,trun
	jmps ifcome


cifc2	lea di,cif2
	jmp loop
cif2	db tf,treset,tf,texp
	or ax,ax
	jnz cifdo		;Expression is not zero (TRUE).
	db 0e9h,tf,trcode	;Just remember code address to put jump.
cifdo	;3 bytes above for jump.
	db 4
	dw 0		;Jump offset put here.
	db tf,trun
ifcome	call nextch
	jc cifenj
	call rlook
	jc cifenj
	cmp ah,1
	stc
	mov al,21
	jne cifen

	call lpstat
cifenj	jc cifen
	movb ifuse,1

	call nextch
	jc noelse
	push spoint
	call rlook
	mov cx,spoint
	pop spoint
	jc noelse
	cmp ah,2	;ELSE?
	mov al,21
	jne noelse
	mov spoint,cx
	movb [bp],0e9h	;JMP
	add bp,3
	call noelse	;If expression was false then use ELSE.
	jc cifen
	sub bp,2
	mov cl,4
	call ltcc0
	jc cifen
	mov [bp],ax
	add bp,2
	call lpstat	;Compile next statment and set jumps
	jc cifen

noelse	mov cl,4
	call getclp	;Get AX as the address saved on stack (to put JMP).
	jnc noel2
	cmp ch,9
	jne longc
	mov ax,[di]
	jmp soel2
longc	stc
	ret
noel2	mov di,ax
	mov bx,bp
	sub bx,ax
	dec bx
	dec bx
	mov ss:[di],bx
	mov lastpf,0	;reset last CALL to 0.
	clc
cifen	ret

;== #SHORT if ==============================================================

ifshort call loopvar
	jnc siffi
siff2c	jmp sifc2
siffi	call skip
	mov ax,[si]
	cmp al,'='
	je sifee
	cmp ax,256*'>'+'<'
	jne sifnez
	jmp sifne
sifnez	cmp al,'<'
	je sifll
	cmp al,'>'
	jne siff2c
	incw spoint
	call loopifn
	cmp al,99
	je siff2c
	lea di,sifgg2
	jmp loop
	db tf,tend
sifgg2	db 81h,3eh,tf,pvar,tf,pnum
	db tf,tisign,7eh,76h
	db tf,trcode,9
	db tf,trun
	jmp sfcome

sifee	incw spoint
	call loopifn
	cmp al,99
	je sifc2
	lea di,sifee2
	jmp loop
sifee2	db 81h,3eh,tf,pvar,tf,pnum
	jne $
	db tf,trcode,9
	db tf,trun
	jmp sfcome

sifll	incw spoint
	call loopifn
	cmp al,99
	je sifc2
	lea di,sifll2
	jmp loop
sifll2	db 81h,3eh,tf,pvar,tf,pnum
	db tf,tisign,7dh,73h
	db tf,trcode,9
	db tf,trun
	jmp sfcome

sifne	addw spoint,2
	call loopifn
	cmp al,99
	je sifc2
	lea di,sifne2
	jmp loop
sifne2	db 81h,3eh,tf,pvar,tf,pnum
	je $
	db tf,trcode,9
	db tf,trun
	jmps sfcome

sifc2	lea di,sif2
	jmp loop
sif2	db tf,treset,tf,texp
	or ax,ax
	jz $			;True? Jump if false.
	db tf,trcode,9		;Address for jump.
	db tf,trun
sfcome	call nextch
	jc sifen
	call rlook
	jc sifen
	cmp ah,1
	stc
	mov al,21
	jne sifen

	call lpstat
	jc sifen
	movb ifuse,1

	call nextch
	jc soelse
	push spoint
	call rlook
	mov cx,spoint
	pop spoint
	jc soelse
	cmp ah,2	;ELSE?
	mov al,21
	jne soelse
	mov spoint,cx
	movb [bp],0ebh	;JMPS (short)!
	add bp,2
	call soelse	;If expression was false then use ELSE.
	jc sifen
	mov cl,9
	call ltcc0
	jc sifen
	call lpstat	;Compile next statment and set jumps
	jc sifen

soelse	mov cl,9
	call getclp	;Get AX as the address saved on stack (to put JMP).
	jnc soel2
	cmp ch,4
	jne shortc
	mov ax,[di]
	jmp noel2
shortc	stc
	ret
soel2	mov di,ax
	mov bx,bp
	sub bx,ax
	cmp bx,127
	ja so155
	mov ss:[di-1],bl
	mov lastpf,0	;reset last CALL to 0.
	clc
sifen	ret

so155	mov al,155	;Code exceeds short jump range!!!
	stc
	ret

;== if32 ======================================================================

cif32	db tf,texp32
	or ax,dx
	jnz cifd32		;Expression is not zero (TRUE).
	db 0e9h,tf,trcode	;Just remember code address to put jump.
cifd32	;3 bytes above for jump.
	db 4
	dw 0		;Jump offset put here.
	db tf,trun
	jmp ifcome

;==============================================================================

clet	db tf,trun
	call skip2
	jmp dofin2	;Treat as without the LET statement.

cscan	db tf,tifnum,0c7h,6,tf,trun
	mov ax,wnum
csccom	add ax,ax
	push ax
	mov cl,xmitab
	call hfuscn
	pop bx
	mov [bp],bx
	add bp,2
	mov ax,bp
	add ax,5
	mov [bp],ax
	add bp,2
	mov di,offset(cskfin)
	jmp loop

	db tf,tend,tf,texp
	add al,al
	add ax,hfmitab
	mov di,ax
	movw [di],1000	;Put address into MITAB.
cskfin	jmp start	;Skip over routine.
	db tf,trun
cskcom	mov [bp-5],bp	;Put address of routine.
	mov al,0c3h
	jmp offax

ckey	db tf,tifnum,0c7h,6,tf,trun
	mov ax,wnum
	add ax,133	;Make it a number between 133-260 then use scan.
	jmps csccom

	db tf,tend,tf,texp
	add ax,133
	add ax,ax
	add ax,hfmitab
	mov di,ax
	movw [di],1000
	jmp start
	db tf,trun
	jmps cskcom

cstscan db tf,tifnum,0c7h,6,tf,trun
	mov ax,wnum
cstcom	add ax,ax
	add ax,hfmitab
	mov [bp],ax
	add bp,2
	sub ax,ax
	jmp lpute2

	db tf,tend,tf,texp
	add al,al
	add ax,hfmitab
	mov di,ax
	movw [di],0	;Put address into MITAB.
	db tf,tend


cstkey	db tf,tifnum,0c7h,6,tf,trun
	mov ax,wnum
	add ax,133	;Make it a number between 133-260 then use scan.
	jmps cstcom

	db tf,tend,tf,texp
	add ax,133
	add ax,ax
	add ax,hfmitab
	mov di,ax
	movw [di],0
	db tf,tend

tempk	db ?
cpoke	db tf,tifnd,2,tf,trun
	call lpcomma
	jc cpok3j		;Offset within code segment only.
	push oplace
	push wnum
	mov al,defd
	mov tempk,al
	call loopnum
	pop bx
	pop di
	jnc cpokn		;Second value is an expression.

	push di
	push bx
	call loopexp
	pop bx
	pop di
	jc pkret
	movb [bp],0a3h		;MOV [nn],AX
	inc bp
	cmp tempk,0
	jnz nopeu
	mov es,oneseg
	mov es:[di-4],bp	;Change entry in
nopeu	mov [bp],bx
	add bp,2
	ret

cpokn	movw [bp],06c7h
	mov ax,wnum2
	mov [bp+2],ax
	mov ax,wnum
	mov [bp+4],ax
	add bp,6
pkret	ret

	db tf,tend,tf,treset,tf,tifvar,tf,trun
	call lpcomma
	jc cpok3
	call loopnum
cpok3j	jc cpok3
	movw [bp],3e8bh
	add bp,2
	mov ax,wvar
	call usv
	movw [bp],05c7h
	add bp,2
	mov ax,wnum
	jmp lpute2

	db tf,tend,tf,trun
cpok3	call restsp
	call loopexp
	jc notpoke
	call lpcomma
	jnc cpoff
	mov al,27
	mov ah,pipe
	cmpb [si],ah
	jne notpoke
	incw spoint	;Skip past the colon.
	mov di,offset(cpsegc)
	jmp loop
cpsegc	push ax
	db tf,texp,tf,tcomma
	push ax
	db tf,texp
	pop di
	pop es
	mov es:[di],ax
	db tf,tend
cpoff	mov di,offset(cpoffc)
	jmp loop
cpoffc	db tf,tifnd,4
	mov di,ax
	db 0c7h,5,tf,pnum,tf,tend
	db tf,tifvar
	mov di,ax
	db 0a1h,tf,pvar
	mov [di],ax
	db tf,tend
	push ax
	db tf,texp
	pop di
	mov [di],ax
	db tf,tend

notpoke stc
	ret

cpokeb	db tf,tifnd,2,tf,trun
	call lpcomma
	jc cpokb3j		;Offset within code segment only.

	push oplace
	push wnum
	mov al,defd
	mov tempk,al
	call loopnum
	pop bx
	pop di
	jnc cpokbn		;Second value is an expression.

	push di
	push bx
	call loopexp
	pop bx
	pop di
	jc pkretb
	movb [bp],0a2h		;MOV [nn],AL
	inc bp
	cmp tempk,0
	jnz nopeub
	mov es,oneseg
	mov es:[di-4],bp	;Change entry in
nopeub	mov [bp],bx
	add bp,2
pkretb	ret

cpokbn	movw [bp],06c6h
	mov ax,wnum2
	mov [bp+2],ax
	mov al,wnum
	mov [bp+4],al
	add bp,5
	ret

	db tf,tend,tf,treset,tf,tifvar,tf,trun
	call lpcomma
	jc cpokb3
	call loopnum
cpokb3j jc cpokb3
	movw [bp],3e8bh
	add bp,2
	mov ax,wvar
	call usv
	mov al,wnum
	movw [bp],05c6h
	mov [bp+2],al
	add bp,3
	ret

	db tf,tend,tf,trun
cpokb3	call restsp
	call loopexp
	jc notpbk
	call lpcomma
	jnc cpboff
	mov al,15
	mov ah,pipe
	cmpb [si],ah
	jne notpbk
	incw spoint	;Skip past the colon.
	mov di,offset(cpbsegc)
	jmp loop
cpbsegc push ax
	db tf,texp,tf,tcomma
	push ax
	db tf,texp
	pop di
	pop es
	mov es:[di],al
	db tf,tend
cpboff	mov di,offset(cpboffc)
	jmp loop
cpboffc db tf,tifnum
	mov di,ax
	db 0c6h,5,tf,pnum8,tf,tend
	db tf,tifvar
	mov di,ax
	db 0a0h,tf,pvar
	mov [di],al
	db tf,tend
	push ax
	db tf,texp
	pop di
	mov [di],al
	db tf,tend

notpbk	stc
	ret

;==============================================================================

cprn64	db tf,texp,tf,tcomma	;print64 64 bit addr,print addr,length
	push ax
	db tf,texp,tf,tcomma
	push ax
	db tf,texp
	pop di
	pop bp
	db tf,hcall,xprn64	;ax=len, di=print addr, bp=64 bit addr
	db tf,tend

;==============================================================================

cqsort	db tf,texp32		;number of elements
	db tf,tcomma
	push dx,ax
	db tf,texp		;compare function
	db tf,tcomma
	push ax
	db tf,texp		;exchange function
	db tf,hcall,xqsort
	db tf,tend

;==============================================================================

cvga_line	db tf,texp	;VGA_LINE x1,y1 TO x2,y2
	push ax
	db tf,tcomma
	db tf,texp
	push ax
	db tf,tword,3
	db tf,texp
	push ax
	db tf,tcomma
	db tf,texp
	db tf,hcall,xvga_line
	db tf,tend
