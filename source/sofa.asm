
;===========================================================================
;==									  ==
;==		   SOFA - SUPER OPTIMISING FAST ASSEMBLER		  ==
;==									  ==
;==		       WRITTEN BY PETER CAMPBELL 1988			  ==
;==									  ==
;===========================================================================


fast	equ 0
sofa	equ 1

	jmp begini


tf	equ 181
bigfind equ 20


label	equ 0	;Types of user defined labels
nproc	equ 1	;PROC/PROC NEAR
const	equ 3	;EQU
memb	equ 4
memw	equ 5
memd	equ 6

udlab	equ 10
udb	equ 14
udw	equ 15

onejps	equ 0
oneadd	equ 1
onesub	equ 2


trun	equ 1
tnum	equ 2
tcomma	equ 3
treset	equ 4
tdisp	equ 5
tcomp	equ 6
tregw	equ 7
tseg	equ 8
tmxr	equ 9
stop	equ 10
tsept	equ 11
twvmxr	equ 12
twmxr	equ 13
tword	equ 14
tbyte	equ 15
maxlps	equ 15
dwlps	dw looprun,loopnum,lpcomma,restsp
	dw lpdisp,lpcomp,lpregw,lpseg
	dw lpmxri,lpstop,chpok,lpwvmxr
	dw lpwmxr,lptw,lptb


defext1 equ 'A'
defext2 equ 'S'
defext3 equ 'M'

bept	db 0,1,1,1,1,1,1,1 ;0
	db 1,0,0,1,1,0,1,1 ;8
	db 1,1,1,1,1,1,1,1 ;16
	db 1,1,0,0,1,1,1,1 ;24
	db 0,1,0,1,1,1,1,0 ;32
	db 0,0,0,0,0,0,1,0 ;40
	db 1,1,1,1,1,1,1,1 ;48
	db 1,1,0,0,1,0,1,1 ;56
	db 1,1,1,1,1,1,1,1 ;64
	db 1,1,1,1,1,1,1,1 ;72
	db 1,1,1,1,1,1,1,1 ;80
	db 1,1,1,0,1,0,1,1 ;88
	db 1,1,1,1,1,1,1,1 ;96
	db 1,1,1,1,1,1,1,1 ;104
	db 1,1,1,1,1,1,1,1 ;112
	db 1,1,1,1,1,1,1,1 ;120


begini	#include fsi.asm

compile lea dx,filen+2

	mov ax,offset(pnest)
	mov pset,ax

compil2 call prtdxn
	mov ax,3d00h
	int 21h 	;OPEN FILE
	jnc opedok
	jmp serror
opedok	mov handle,ax

	mov ax,-1
	push ax 	;Stack START pointer.

	mov ax,bufsize
	add ax,offset(fsend)+256
	movw buffp,ax

;Once file is open then start assembling in 10k blocks.

	movw line,1
	jmp compstc	;Continue common code to FAST and SOFA for eol.

;The source file has been completely compiled so close it.

wtcmd	db ?
wtreg	db ?
wtmem	db ?
subf	db ?
is32bit db ?

;Compile the line.

doline	mov wtcmd,2
	mov wtreg,2
	mov wtmem,2
	mov is32bit,0	;no 80386 override
	mov defd,1	;Say all defined, MXDISP?
	mov subf,0

	mov si,ifpoint
	cmpb [si],0	;ENDIF check?
	ja dolkfd
	call skip2
	jc dofas
	cmp al,'#'
	jne dofas	;Must be #endif or #if.

	inc spoint
	lea di,csofa
	call fentry2	;Which SOFA command?
	mov al,136	;Unknown directive.
	jc dfcret

	mov di,[di+1]
	cmp di,offset(cifend)
	je efcont	;ENDIF?
	cmp di,offset(ccpif)
	jne dofas	;IF?

	incw ifpoint
	mov si,ifpoint
	movb [si],0	;False, so continue with false.
	jmps dofas

efcont	decw ifpoint
dofas	pop bx
	jmp eolok

dfchasj	jmp dfchash

dolkfd	mov si,spoint
	mov al,[si]
	cmp al,9
	je dfcontj
	cmp al,' '      ;If tab or space then must be for FIND.
	je dfcontj
	cmpb [si],'#'   ;Compiler directive.
	je dfchasj
	call skip2
	jc dofas

	call findbin	;Does word exist?
	mov es,labseg
	mov addf1,si

	jc addwol
	mov bh,es:[di+1]
	cmp bh,udlab
	jae addwolu	;Undefined, set real type - compare it's old type.

	mov al,3	;Double definition.
	stc
dfcret	ret
dfcontj jmp dfcont

addwol	call addfb	;Add word to dictionary.
	jc dfcret
	add di,10
	mov lablast,di
	sub di,10	;Back to correct place for EQU etc.
	sub bh,bh	;0, say wasn't undefined.
addwolu push di 	;BH=old type - if undefined.

	mov si,spoint
	cmpb [si],':'
	jne dttt
	incw spoint	;Skip over colon.

dttt	lea di,typetab
	push spoint
	push bx
	call fentry2
	mov al,0	;Label.
	jc dttl
	mov al,1[di]	;Type.
dttl	pop bx
	pop cx
	pop di

	#if 0
	;Check types? (BH/AL)	20-02-1995	;FUCK TEMPORARY
	pushf
	or al,al
	jnz not_lll

	movb [bp],0cch		;INT 03
	inc bp

not_lll popf					;FUCK TEMPORARY
	#endif

	mov es,labseg
	mov es:1[di],al
	mov es:2[di],bp ;Put code address (overwritten by EQU).

	incw es:[di+8]	;usage counter

	jc dfcont	;Continue assembling other statements in line.

	cmp al,const
	je equcon
	mov spoint,cx
	jmps dfcont
equcon	push di
	call decimal
	pop di
	jc equderr
	cmp wtmem,2
	jne equill
	mov es,labseg
	mov es:2[di],ax 	;Put constant.
	ret			;RET NC
equill	mov al,21
equderr stc
linefin ret

loopchk jc linefin
	jmp loop2	;Continue with loop, no error.

dfcont	call skip2
	jc linefin
	cmp al,'#'
	jne dfcont3

dfchash	inc spoint
	lea di,csofa
	call fentry2	;Which SOFA command?
	mov al,136	;Unknown directive.
	jnc dfcontu
	ret

dfcont3 call find
	mov al,0
	jc linefin
	call pssp
dfcontu mov di,1[di]
	jmp loop	;Process command.

lptw	movb wtcmd,1
	ret
lptb	movb wtcmd,0
	ret

loopnum call decimal
	mov wnum,ax
	ret		;Return with the carry determined by decimal.


;******* EXPRESSION HANDLERS

lpdisp	mov omust,onejps	;Does TF,TEND also.
	call decis
	jc nogetp
	cmp wtmem,2
	je lpwn
	push ax
	mov al,26	;Warning! Jumping to memory pointer.
	call comperr
	pop ax
lpwn	cmp defd,1
	je lpm1
	add ax,bp
	inc ax
lpm1	mov bx,bp
	inc bx
	sub ax,bx
	cmp ax,127	;More than 128 bytes either side?
	jg outerg
	cmp ax,-128
	jl outerg
lpm2	mov [bp],al
	inc bp
	pop bx		;Drop LOOPCHK (from LPCODES).
chpok	clc
	ret
outerg	mov al,17
nogetp	stc
	ret

decis	mov dmust,0
	jmps decmuc
decimal mov dmust,1
decmuc	mov defd,1
	call decmal
	jc decinot	;Must be at least one number.
decima2 call decjoin	;Get a join of some sort.
	jc chpok	;If not valid then ignore, leave as is.
	jmps decima2
decjoin push ax
	call skip2
	jc stopdec
	cmp al,'+'
	je adddeci
	cmp al,'-'
	je subdeci
	cmp al,'*'
	je muldeci
	cmp al,'/'
	je divdeci
stopdec pop ax		;Return with the number.
	stc
decinot ret

adddeci incw spoint
	lea bx,decimal
	cmp dmust,1
	je callai
	lea bx,decis
callai	mov al,defd
	push ax
	call bx
	pop bx
	pushf
	and defd,bl	;If undefined then make that the primary mask.
	popf
	jc stopdec
	pop bx
	add ax,bx
	clc
	ret

subdeci incw spoint
	mov bl,omust
	push bx
	mov omust,onesub
	inc subf	;Say negative.
	call decmal
	dec subf	;Don't negate now.
	pop bx
	mov omust,bl
	jc stopdec
	pop bx
	sub bx,ax
	mov ax,bx
	clc
	ret

muldeci incw spoint
	movb dmust,1	;Must be defined if 'x * undef label'
	call decmal
	jc stopdec
	pop bx	;Get last value.
	mul bx
	clc
	ret

divdeci incw spoint
	movb dmust,1	;Must be defined if 'x * undef label'
	call decmal
	jc stopdec
	pop bx	;Get last value.
	xchg ax,bx
	sub dx,dx
	cmp bx,dx
	jz divzero
	div bx
	clc
	ret
divzero mov al,15	;Divide by zero error.
decshit stc
	ret


decmal	sub dx,dx
dec2	call skip2
	jc decshit
	cmp al,'-'
	jne dec3
	incw spoint
	call decmal
	pushf
	neg ax
	popf
	ret

dec3	call lowq
	jnc declab
	call ifdigit
	jnc decnl
	mov stdec,si
	jmp decloop

decnl	cmp al,'?'
	jne dmchk
	incw spoint
	ret	; Return with a miscellaneous value in AL/AX.

dmchk	cmp al,'+'
	jne decplus	;Ignore +
	incw spoint
	jmp dec2
decplus cmp al,'''
	jne dmdols
	jmp decasc
dmdols	cmp al,'$'
	mov al,0
	jne dmcc
	incw spoint
	mov ax,bp	;$=code address (BP).
	ret
decsyn	sub al,al
dmcc	stc
	ret

declab	mov cx,20
	push si
	lea di,quall
	call qureg
	pop spoint
	jnc dlres

;	cmp ax,'f'*256+'o'
;	je declck
;	cmp ax,'o'*256+'n'
;	jne declmx

declck	lea di,spconst
	call fentry2	;Test offset before labels!!!
	jc declmx
	mov di,1[di]
	jmp loopz

declmx	mov al,udb	;Set type of word.
	cmpb wtcmd,1
	jb dlount
	mov al,udw
	je dlount
	cmpb wtreg,1
	je dlount
	mov al,udb
	jb dlount
	mov al,udlab
dlount	mov ount,al

	call word
	jc exprc
	cmp al,memb
	jne decla1
	mov wtmem,0
	jmps decla2
decla1	cmp al,memw
	jne decla2
	mov wtmem,1
decla2	cmp al,memd
	jne decla3
	mov wtmem,1
decla3	mov bl,al	;255??
	mov ax,dx
	clc
	ret		;Found a label.
dlres	mov al,24	;A reserved word, signal as illegal.
expc	stc
exprc	ret


word	call skip2
	call lowq
	mov al,0
	jc exprc

	call wdlook
	jnc wordu	;Ok?

	cmp dmust,1
	je expc

	call addfb
	jc exprc
	add di,10
	mov lablast,di
	sub di,10	;Back to correct place for EQU etc.
	mov al,udlab

wordu	cmp al,udlab
	jb wordr	;Defined, ok to continue.

	cmp dmust,1	;If undefined label but must be defined then error.
	mov al,5	;Invalid parameter, can't use it here.
	je expc

	mov es,oneseg
	mov si,oplace

	cmp si,onesize
	jae e131

	mov al,omust
	mov es:[si],al
	mov es:[si+1],bp
	mov ax,addf2
	mov es:[si+3],ax
	add oplace,5

	lea bx,tabuc
	sub ah,ah
	mov al,ount
	mov es,labseg
	mov es:[di+1],al
	sub al,udlab
	add bx,ax
	mov al,[bx]
	sub dx,dx	;Assume offset of zero.
	mov defd,dh	;Was/Is undefined.

wordr	clc
	ret


tabuc	db label,0,0,0,memb,memw,memd

e131	mov al,131	;Out of UNDEF space - abort.
	jmp comperr
wdlook	call findbin
	mov es,labseg
	mov addf1,si
	mov al,2
	jc absok
	mov al,es:1[di] ;Give the type of word.
	mov dx,es:2[di] ;DX=word value.
	incw es:[di+8]	;usage counter
absok	ret

decasc	inc si
	sub ah,ah
	mov al,[si]
	inc si
	inc si
	mov spoint,si
	cmpb -1[si],39
	je absok
	sub al,al
	stc
	jmps absok

lpregw	lea di,regw
	call qubw
	jc nextej

	call test386

	mov wtreg,1
lregcom call getal
	or al,cl
	mov wreg,al
	ret		;CLC from OR.

lpregb	lea di,regb
	call qubw
	jc nextej
	mov wtreg,0
	jmp lregcom

nextej	jmp nextend

lpseg	lea di,seg
	mov cx,4
	call qureg
	jc nextej
	call getal
	add cl,cl
	add cl,cl
	add cl,cl	;CL*8
	or al,cl
	mov wreg,al
	mov wtreg,1
	ret

wcheck	mov ah,wtcmd
	mov al,wtreg
	cmp ah,2	;If command sets type then register must be the same.
	je wchkreg
	cmp al,2
	je wchkmem	;Check memory types.
	cmp al,ah
	je wchkmem
	mov al,20	;Different types.
	stc
	ret
wchkz	mov al,wtmem
	cmp al,2
	je wez
	mov wtcmd,al	;Set type to that of the memory type being used.
	clc
	ret
wez	mov al,19	;Error - no type specified.
	stc
wchkr	ret

wchkreg	cmp al,2	;Must have register defined if no command setting.
	je wchkz
	mov wtcmd,al
	mov ah,al	;AH=wtcmd/wtreg.

wchkmem	cmp wtmem,2
	je wchkr	;Memory undefined type so fuck off.
	cmp wtmem,ah
	je wchkr	;If mem=command type then ok.
	mov al,25
	call comperr	;Uses command/register type by default.
	clc
ggmxe	ret

lpwvmxr inc bp
	call lpmxr	;Get operand.
	dec bp
	jc ggmxe
	lea di,wvtab
	call loopz	;Is it ',CL' or ',1' else error.
	jc ggmxe
	call wcheck
	jc ggmxe

	call test386

	mov al,11010000b
	or al,wv
	or al,wtcmd
	mov [bp],al
	inc bp
	call getal
	jmp mxrput	;Put the second byte and any displacements.

lpwmxr	inc bp
	call lpmxr
	dec bp
	jc ggmxe
	call wcheck
	jc ggmxe

	call test386

	call getal
	or al,wtcmd
	mov [bp],al
	inc bp
	call getal
	jmp mxrput

wvtab	db tf,tcomp,',cl',0,tf,trun
	movb wv,2
	clc
	ret
	db tf,tsept
	db tf,tcomp,',1',0,tf,trun
	movb wv,0
	clc
	ret
	db tf,tend

lpmxri	inc bp
	call lpmxr	;Offset BP for undefined labels.
	dec bp
	jmp test386

lpmxr	movw wdisp,0
	movb wmod,0
	movb wr_m,6	;Setup defaults.

lpmxr2	call skip2
	jc lpmxrr
	cmp al,'['
	je mxcont
	call lowq
	jc mxdallj

	lea di,quall
	mov cx,20
	call qureg	;Try all registers at once.
	jc mxdallj	;If not then try label.

	cmp cl,8
	mov al,1
	jb regwc
	cmp cl,16
	jb regbc

	sub cl,16
	cmpb [si],':'   ;Override must be SEGMENT:
	je lpmne
	mov al,5
	stc
lpmxrr	ret
mxdallj jmp mxdall
lpmne	mov al,cl
	add al,al
	add al,al
	add al,al
	or al,100110b
	mov [bp-1],al
	inc bp
	incw spoint
	jmp lpmxr2

regbc	sub cl,8
	sub al,al
regwc	mov wtreg,al
	mov wr_m,cl		;The register.
	movb wmod,128+64	;R/M = register field.
	clc
	ret

mxcont	incw spoint
	lea di,mxtab	;Lookup R/M table.
	mov cx,4
	call qureg
	jc mxdonly
	add cl,4
	mov wr_m,cl
	call skip2
	cmp al,']'      ;End of expression?
	je mxendb
	call mxdisp
	jmps mxdcont
mxdonly inc bp
	call mxdisp
	dec bp
	jc mxerr
	call skip2
	cmp al,']'
	mov al,16
	jne mxerr
	incw spoint
mdxon2	movb wmod,0	;Only displacement. rm=6
	ret
mxdcont jc mxerr
	call skip2
	cmp al,']'
	je mxendb
	mov al,16
mxerr	stc
	ret

mxdall	inc bp
	call mxdisp
	dec bp
	jc mxerr
	call skip2
	cmp al,'['
	je mxcontj
	clc
	jmps mdxon2
mxcontj jmp mxcont

mxendb	incw spoint
	cmpb wr_m,6	;Special case [BP]
	jne mxret
	cmpb wmod,0
	jne mxret
	movb wmod,64	;Change it to [BP+0]
mxret	clc
	ret

accw	lea di,axal
	mov cx,2
	call qureg
	mov al,0	;Default syntax error.
	jc nacc
	mov wtreg,cl
nacc	ret
axal	db 'alax'

putlw	cmp wtcmd,1
	ja putz
	jb putal
	mov [bp],ax	;Put word.
	add bp,2
	ret
putal	mov [bp],al	;Put byte.
	inc bp
	clc
	ret
putz	mov al,19
	stc
	ret

immedi	mov al,wtmem
	push ax
	mov wtmem,2
	call decimal
	pop bx
	mov bh,wtmem
	mov wtmem,bl
	jc immer1
	cmp bh,2
	jne immerr
	mov and1,ax
	ret
immerr	mov al,21	;Illegal value.
immer1	stc
	ret


;****** Some compiler data ******

typetab db 2,'equ',13,const,0
	db 2,'proc',nproc,0
	db 1,'db',memb,0
	db 1,'dw',memw,0
	db 1,'dd',memd,0
	db 255

quall	;All registers
regw	db 'axcxdxbxspbpsidi'
regb	db 'alcldlblahchdhbh'
seg	db 'escsssds'

reg32	db 2,'eax',13,0,0
	db 2,'ecx',13,1,0
	db 2,'edx',13,2,0
	db 2,'ebx',13,3,0
	db 2,'esp',13,4,0
	db 2,'ebp',13,5,0
	db 2,'esi',13,6,0
	db 2,'edi',13,7,0
	db 255

mxtab	db 'sidibpbx'

stv	dw 0	;A store used by the TRUN table code.

wnum	dw 0	;Compiler stores variables here.
wdisp	dw 0
wmod	db 0
wr_m	db 0
wreg	db 0
wd	db 0
wdo	db 0
sflag	db 0
wv	db 0
movsto	db 0
storea	db ?

	db 0	;Near procedure type.
pnest	ds 32
pset	dw 0

introm	db 13,10,'!SOFA 8088/80386 COMPILER v2.00 (09/05/94)$'
entern	db 'Source filename [.ASM] $'
memerr	db 'SOFA needs 120K free memory!',13,10,'$'

spconst db 3,'offset'
	dw offset(coffset)
	db 2,'not',13
	dw offset(cinv)
	db 255

cinv	db tf,trun
	call decimal
	jc cinve
	or ax,ax
	mov ax,0ffffh	;If 0 then -> 65535 else 0.
	jz cinvr
	inc ax
cinvr	clc
cinve	ret

coffset db tf,trun
	call skip2
	cmp al,'('
	jne cofnob
	incw spoint
	mov al,dmust
	push ax
	mov dmust,0
	mov ount,udlab

	mov omust,oneadd
	cmp subf,0
	jz offadd
	mov omust,onesub	;Set sign for answer.

offadd	call word	;Get the value, doesn't matter whether word or byte.
	pop bx
	mov dmust,bl
	mov wtmem,2
	jc coferr
	call skip2
	cmp al,')'
	mov al,16	;Must have a closing bracket.
	jne coferr
	incw spoint	;Skip past the bracket.
	mov ax,dx	;Return address of the label/pointer.
	clc
	ret
coferr	stc
	ret

cofnob	mov al,dmust
	push ax
	mov dmust,0
	mov ount,udlab

	mov omust,oneadd
	cmp subf,0
	jz nffadd
	mov omust,onesub	;Set sign for answer.

nffadd	call word	;Get the value, doesn't matter whether word or byte.
	pop bx
	mov dmust,bl
	mov wtmem,2
	jc coferr
	mov ax,dx	;Return address of the label/pointer.
	clc
	ret


;******* SOFA COMPILER TABLES

csofa	db 1,'if'
	dw offset(ccpif)
	db 4,'include',13
	dw offset(cincl)
	db 3,'endif',13
	dw offset(cifend)
	db 2,'para'
	dw offset(cpara)
	db 2,'org',13
	dw offset(corg)
	db 255

hsh001	db 2,'jne',13
	dw offset(cjne)
	db 2,'sub',13
	dw offset(csub)
	db 255

hsh003	db 2,'incb'
	dw offset(cincb)
	db 2,'jng',13
	dw offset(cjle)
	db 255

hsh005	db 2,'jna',13
	dw offset(cjbe)
	db 2,'jnge'
	dw offset(cjl)
	db 2,'sarw'
	dw offset(csarw)
	db 255

hsh006	db 2,'jnb',13
	dw offset(cjnb)
	db 255

hsh007	db 2,'cwd',13
	dw offset(ccwd)
	db 2,'jnc',13
	dw offset(cjnb)
	db 255

hsh008	db 2,'jnl',13
	dw offset(cjnl)
	db 255

hsh011	db 2,'inc',13
	dw offset(cinc)
	db 2,'jno',13
	dw offset(cjno)
	db 2,'sbbw'
	dw offset(csbbw)
	db 255

hsh012	db 3,'xchgw',13
	dw offset(cxchgw)
	db 255

hsh015	db 3,'imulw',13
	dw offset(cimulw)
	db 3,'scasb',13
	dw offset(cscasb)
	db 255

hsh016	db 2,'sarb'
	dw offset(csarb)
	db 255

hsh017	db 2,'ret',13
	dw offset(cret)
	db 255

hsh020	db 2,'jnp',13
	dw offset(cjnp)
	db 255

hsh021	db 2,'rep',13
	dw offset(crep)
	db 255

hsh022	db 2,'incw'
	dw offset(cincw)
	db 2,'loop'
	dw offset(cloop)
	db 255

hsh023	db 2,'jns',13
	dw offset(cjns)
	db 255

hsh025	db 3,'xchgb',13
	dw offset(cxchgb)
	db 255

hsh026	db 3,'imulb',13
	dw offset(cimulb)
	db 3,'scasw',13
	dw offset(cscasw)
	db 255

hsh027	db 3,'pushf',13
	dw offset(cpushf)
	db 255

hsh028	db 2,'int',13
	dw offset(cint)
	db 255

hsh030	db 2,'jnz',13
	dw offset(cjne)
	db 2,'sbbb'
	dw offset(csbbb)
	db 255

hsh032	db 2,'jge',13
	dw offset(cjnl)
	db 2,'not',13
	dw offset(cnot)
	db 2,'stc',13
	dw offset(cstc)
	db 255

hsh033	db 1,'jl'
	dw offset(cjl)
	db 2,'shrw'
	dw offset(cshrw)
	db 255

hsh034	db 1,'jo'
	dw offset(cjo)
	db 255

hsh036	db 2,'nop',13
	dw offset(cnop)
	db 255

hsh039	db 2,'std',13
	dw offset(cstd)
	db 255

hsh040	db 1,'je'
	dw offset(cje)
	db 255

hsh042	db 1,'jg'
	dw offset(cjnle)
	db 2,'sti',13
	dw offset(csti)
	db 255

hsh044	db 2,'and',13
	dw offset(cand)
	db 1,'ja'
	dw offset(cjnbe)
	db 2,'negb'
	dw offset(cnegb)
	db 255

hsh046	db 2,'mov',13
	dw offset(cmov)
	db 2,'divw'
	dw offset(cdivw)
	db 1,'jc'
	dw offset(cjb)
	db 255

hsh047	db 2,'adcb'
	dw offset(cadcb)
	db 1,'jb'
	dw offset(cjb)
	db 255

hsh052	db 2,'shrb'
	dw offset(cshrb)
	db 255

hsh055	db 1,'jz'
	dw offset(cje)
	db 255

hsh057	db 2,'negw'
	dw offset(cnegw)
	db 255

hsh058	db 2,'adcw'
	dw offset(cadcw)
	db 255

hsh059	db 2,'divb'
	dw offset(cdivb)
	db 255

hsh061	db 1,'jp'
	dw offset(cjp)
	db 255

hsh062	db 1,'js'
	dw offset(cjs)
	db 255

hsh065	db 2,'jle',13
	dw offset(cjle)
	db 255

hsh066	db 2,'subb'
	dw offset(csubb)
	db 255

hsh067	db 2,'cmc',13
	dw offset(ccmc)
	db 1,'in'
	dw offset(cin)
	db 255

hsh068	db 2,'retf'
	dw offset(cretf)
	db 255

hsh072	db 2,'rol',13
	dw offset(crol)
	db 255

hsh077	db 3,'cmpsb',13
	dw offset(ccmpsb)
	db 255

hsh078	db 2,'lds',13
	dw offset(clds)
	db 2,'rclw'
	dw offset(crclw)
	db 255

hsh080	db 2,'cmp',13
	dw offset(ccmp)
	db 255

hsh086	db 2,'ror',13
	dw offset(cror)
	db 255

hsh087	db 2,'sahf'
	dw offset(csahf)
	db 2,'subw'
	dw offset(csubw)
	db 255

hsh088	db 3,'cmpsw',13
	dw offset(ccmpsw)
	db 2,'hlt',13
	dw offset(chlt)
	db 255

hsh091	db 2,'rclb'
	dw offset(crclb)
	db 255

hsh092	db 2,'pop',13
	dw offset(cpop)
	db 255

hsh094	db 2,'dec',13
	dw offset(cdec)
	db 255

hsh099	db 2,'clc',13
	dw offset(cclc)
	db 255

hsh100	db 2,'cld',13
	dw offset(ccld)
	db 2,'jnle'
	dw offset(cjnle)
	db 255

hsh102	db 2,'notb'
	dw offset(cnotb)
	db 255

hsh103	db 2,'out',13
	dw offset(cout)
	db 2,'wait'
	dw offset(cwait)
	db 255

hsh104	db 2,'cmpb'
	dw offset(ccmpb)
	db 255

hsh105	db 2,'cli',13
	dw offset(ccli)
	db 255

hsh106	db 2,'adc',13
	dw offset(cadc)
	db 255

hsh107	db 2,'rolb'
	dw offset(crolb)
	db 255

hsh109	db 2,'add',13
	dw offset(cadd)
	db 255

hsh110	db 2,'les',13
	dw offset(cles)
	db 255

hsh114	db 2,'neg',13
	dw offset(cneg)
	db 255

hsh115	db 2,'notw'
	dw offset(cnotw)
	db 255

hsh116	db 2,'jmp',13
	dw offset(cjmp)
	db 255

hsh119	db 2,'mul',13
	dw offset(cmul)
	db 255

hsh124	db 2,'lea',13
	dw offset(clea)
	db 255

hsh125	db 2,'cmpw'
	dw offset(ccmpw)
	db 255

hsh126	db 2,'idiv'
	dw offset(cidiv)
	db 2,'rolw'
	dw offset(crolw)
	db 2,'xor',13
	dw offset(cxor)
	db 255

hsh127	db 2,'proc'
	dw offset(cprocn)
	db 255

hsh128	db 2,'jbe',13
	dw offset(cjbe)
	db 255

hsh132	db 2,'orw',13
	dw offset(corw)
	db 255

hsh140	db 2,'lock'
	dw offset(clock)
	db 2,'mulb'
	dw offset(cmulb)
	db 255

hsh141	db 2,'rcrw'
	dw offset(crcrw)
	db 2,'sal',13
	dw offset(cshl)
	db 255

hsh145	db 2,'orb',13
	dw offset(corb)
	db 255

hsh147	db 2,'sar',13
	dw offset(csar)
	db 255

hsh152	db 2,'esc',13
	dw offset(cesc)
	db 2,'rcrb'
	dw offset(crcrb)
	db 255

hsh153	db 2,'mulw'
	dw offset(cmulw)
	db 255

hsh159	db 1,'or'
	dw offset(cor)
	db 255

hsh161	db 3,'movsw',13
	dw offset(cmovsw)
	db 255

hsh165	db 2,'iret'
	dw offset(ciret)
	db 2,'jnbe'
	dw offset(cjnbe)
	db 255

hsh167	db 3,'loope',13
	dw offset(cloope)
	db 2,'movb'
	dw offset(cmovb)
	db 255

hsh168	db 2,'rorb'
	dw offset(crorb)
	db 255

hsh169	db 2,'decb'
	dw offset(cdecb)
	db 3,'stosw',13
	dw offset(cstosw)
	db 255

hsh172	db 2,'shl',13
	dw offset(cshl)
	db 255

hsh173	db 3,'idivb',13
	dw offset(cidivb)
	db 2,'xorb'
	dw offset(cxorb)
	db 255

hsh178	db 2,'movw'
	dw offset(cmovw)
	db 2,'shr',13
	dw offset(cshr)
	db 255

hsh180	db 3,'movsb',13
	dw offset(cmovsb)
	db 255

hsh181	db 2,'test'
	dw offset(ctest)
	db 255

hsh182	db 2,'cbw',13
	dw offset(ccbw)
	db 255

hsh184	db 3,'idivw',13
	dw offset(cidivw)
	db 3,'loopz',13
	dw offset(cloop)
	db 2,'xorw'
	dw offset(cxorw)
	db 255

hsh188	db 2,'decw'
	dw offset(cdecw)
	db 3,'stosb',13
	dw offset(cstosb)
	db 255

hsh189	db 2,'rorw'
	dw offset(crorw)
	db 255

hsh193	db 3,'testw',13
	dw offset(ctestw)
	db 255

hsh194	db 2,'jpe',13
	dw offset(cjp)
	db 255

hsh195	db 2,'imul'
	dw offset(cimul)
	db 255

hsh196	db 2,'aam',13
	dw offset(caam)
	db 255

hsh197	db 2,'jnae'
	dw offset(cjb)
	db 255

hsh198	db 2,'salw'
	dw offset(cshlw)
	db 255

hsh199	db 2,'repe'
	dw offset(crep)
	db 255

hsh200	db 2,'aaa',13
	dw offset(caaa)
	db 255

hsh201	db 2,'rcl',13
	dw offset(crcl)
	db 255

hsh202	db 2,'div',13
	dw offset(cdiv)
	db 255

hsh205	db 2,'aad',13
	dw offset(caad)
	db 2,'jcxz'
	dw offset(cjcxz)
	db 255

hsh206	db 2,'equ',13
	dw offset(cequ)
	db 2,'das',13
	dw offset(cdas)
	db 255

hsh207	db 2,'addb'
	dw offset(caddb)
	db 255

hsh211	db 2,'salb'
	dw offset(cshlb)
	db 255

hsh212	db 3,'testb',13
	dw offset(ctestb)
	db 255

hsh213	db 2,'call'
	dw offset(ccall)
	db 2,'xlat'
	dw offset(cxlat)
	db 255

hsh215	db 2,'rcr',13
	dw offset(crcr)
	db 255

hsh216	db 2,'lahf'
	dw offset(clahf)
	db 2,'repz'
	dw offset(crep)
	db 255

hsh218	db 2,'aas',13
	dw offset(caas)
	db 2,'addw'
	dw offset(caddw)
	db 255

hsh219	db 2,'xchg'
	dw offset(cxchg)
	db 255

hsh220	db 3,'callf',13
	dw offset(ccallf)
	db 2,'daa',13
	dw offset(cdaa)
	db 255

hsh224	db 2,'jae',13
	dw offset(cjnb)
	db 255

hsh226	db 2,'shlw'
	dw offset(cshlw)
	db 255

hsh227	db 3,'repnz',13
	dw offset(crepne)
	db 2,'sbb',13
	dw offset(csbb)
	db 255

hsh231	db 2,'andb'
	dw offset(candb)
	db 255

hsh232	db 2,'jmpf'
	dw offset(cjmpf)
	db 1,'dd'
	dw offset(cdd)
	db 255

hsh235	db 2,'push'
	dw offset(cpush)
	db 255

hsh236	db 2,'into'
	dw offset(cinto)
	db 3,'lodsb',13
	dw offset(clodsb)
	db 255

hsh237	db 2,'popf'
	dw offset(cpopf)
	db 255

hsh238	db 1,'db'
	dw offset(cdb)
	db 255

hsh239	db 3,'loopnz'
	dw offset(cloopne)
	db 255

hsh240	db 3,'loopne'
	dw offset(cloopne)
	db 255

hsh242	db 2,'andw'
	dw offset(candw)
	db 255

hsh247	db 2,'endp'
	dw offset(cendp)
	db 2,'shlb'
	dw offset(cshlb)
	db 255

hsh249	db 3,'lodsw',13
	dw offset(clodsw)
	db 255

hsh251	db 1,'dw'
	dw offset(cdw)
	db 255

hsh252	db 3,'repne',13
	dw offset(crepne)
	db 255

hsh253	db 2,'jmps'
	dw offset(cjmps)
	db 255

hsh255	db 1,'ds'
	dw offset(cds)

tabtab	dw tabnul,hsh001,tabnul,hsh003,tabnul,hsh005,hsh006,hsh007
	dw hsh008,tabnul,tabnul,hsh011,hsh012,tabnul,tabnul,hsh015
	dw hsh016,hsh017,tabnul,tabnul,hsh020,hsh021,hsh022,hsh023
	dw tabnul,hsh025,hsh026,hsh027,hsh028,tabnul,hsh030,tabnul
	dw hsh032,hsh033,hsh034,tabnul,hsh036,tabnul,tabnul,hsh039
	dw hsh040,tabnul,hsh042,tabnul,hsh044,tabnul,hsh046,hsh047
	dw tabnul,tabnul,tabnul,tabnul,hsh052,tabnul,tabnul,hsh055
	dw tabnul,hsh057,hsh058,hsh059,tabnul,hsh061,hsh062,tabnul
	dw tabnul,hsh065,hsh066,hsh067,hsh068,tabnul,tabnul,tabnul
	dw hsh072,tabnul,tabnul,tabnul,tabnul,hsh077,hsh078,tabnul
	dw hsh080,tabnul,tabnul,tabnul,tabnul,tabnul,hsh086,hsh087
	dw hsh088,tabnul,tabnul,hsh091,hsh092,tabnul,hsh094,tabnul
	dw tabnul,tabnul,tabnul,hsh099,hsh100,tabnul,hsh102,hsh103
	dw hsh104,hsh105,hsh106,hsh107,tabnul,hsh109,hsh110,tabnul
	dw tabnul,tabnul,hsh114,hsh115,hsh116,tabnul,tabnul,hsh119
	dw tabnul,tabnul,tabnul,tabnul,hsh124,hsh125,hsh126,hsh127
	dw hsh128,tabnul,tabnul,tabnul,hsh132,tabnul,tabnul,tabnul
	dw tabnul,tabnul,tabnul,tabnul,hsh140,hsh141,tabnul,tabnul
	dw tabnul,hsh145,tabnul,hsh147,tabnul,tabnul,tabnul,tabnul
	dw hsh152,hsh153,tabnul,tabnul,tabnul,tabnul,tabnul,hsh159
	dw tabnul,hsh161,tabnul,tabnul,tabnul,hsh165,tabnul,hsh167
	dw hsh168,hsh169,tabnul,tabnul,hsh172,hsh173,tabnul,tabnul
	dw tabnul,tabnul,hsh178,tabnul,hsh180,hsh181,hsh182,tabnul
	dw hsh184,tabnul,tabnul,tabnul,hsh188,hsh189,tabnul,tabnul
	dw tabnul,hsh193,hsh194,hsh195,hsh196,hsh197,hsh198,hsh199
	dw hsh200,hsh201,hsh202,tabnul,tabnul,hsh205,hsh206,hsh207
	dw tabnul,tabnul,tabnul,hsh211,hsh212,hsh213,tabnul,hsh215
	dw hsh216,tabnul,hsh218,hsh219,hsh220,tabnul,tabnul,tabnul
	dw hsh224,tabnul,hsh226,hsh227,tabnul,tabnul,tabnul,hsh231
	dw hsh232,tabnul,tabnul,hsh235,hsh236,hsh237,hsh238,hsh239
	dw hsh240,tabnul,hsh242,tabnul,tabnul,tabnul,tabnul,hsh247
	dw tabnul,hsh249,tabnul,hsh251,hsh252,hsh253,tabnul,hsh255

;****** Main machine code data tables. ******

cxlat	db 11010111b,tf,tend
clahf	db 10011111b,tf,tend
csahf	db 10011110b,tf,tend
cpushf	db 10011100b,tf,tend
cpopf	db 10011101b,tf,tend
caaa	db 00110111b,tf,tend
cdaa	db 00100111b,tf,tend
caas	db 00111111b,tf,tend
cdas	db 00101111b,tf,tend
caam	db 11010100b,00001010b,tf,tend
caad	db 11010101b,00001010b,tf,tend
ccbw	db 10011000b,tf,tend
ccwd	db 10011001b,tf,tend
cinto	db 11001110b,tf,tend
ciret	db 11001111b,tf,tend
cclc	db 11111000b,tf,tend
cstc	db 11111001b,tf,tend
ccmc	db 11110101b,tf,tend
cnop	db 10010000b,tf,tend
ccld	db 11111100b,tf,tend
cstd	db 11111101b,tf,tend
ccli	db 11111010b,tf,tend
csti	db 11111011b,tf,tend
chlt	db 11110100b,tf,tend
cwait	db 10011011b,tf,tend
clock	db 11110000b,tf,tend

corg	db tf,trun
	call decimal
	jc corge
	mov org,ax	;Set new code origin.
	mov bp,ax
corge	ret

cint	db tf,tnum,tf,trun
	cmpw wnum,3
	jne cintn
	mov al,11001100b
ciput	jmp lput
cintn	movb [bp],11001101b
	inc bp
	mov al,wnum
	jmps ciput

;==============================================================================

cdw	db tf,trun
cdlp	mov omust,oneadd
	call decis
	jc cdret
	mov [bp],ax	;Put if successful number (AX).
	add bp,2
	call lpcomma
	jnc cdlp
	clc
	ret
cdret	stc
	ret	;Error, no digit valid.

;==============================================================================

cdb	db tf,trun
cdblp	call cdsin
	jnc cdbcom
	call decimal
	jc cdret
	mov [bp],al
	inc bp
cdbcom	call lpcomma
	jnc cdblp
	clc
	ret

;==============================================================================

cdd	db tf,trun
cddlp	mov omust,oneadd
	call decis
	jc cdret
	mov [bp],ax	;Put if successful number (AX).
	mov [bp+2],dx	;high data
	add bp,4
	call lpcomma
	jnc cddlp
	clc
	ret

;==============================================================================

crep	db 11110011b,tf,trun
repcont jmp doline

crepne	db 11110010b,tf,trun
	jmps repcont

cmovsb	db 10100100b,tf,tend
cmovsw	db 10100101b,tf,tend

ccmpsb	db 10100110b,tf,tend
ccmpsw	db 10100111b,tf,tend

cscasb	db 10101110b,tf,tend
cscasw	db 10101111b,tf,tend

clodsb	db 10101100b,tf,tend
clodsw	db 10101101b,tf,tend

cstosb	db 10101010b,tf,tend
cstosw	db 10101011b,tf,tend

cje	db 01110100b,tf,tdisp
cjl	db 01111100b,tf,tdisp
cjle	db 01111110b,tf,tdisp
cjb	db 01110010b,tf,tdisp
cjbe	db 01110110b,tf,tdisp
cjp	db 01111010b,tf,tdisp
cjo	db 01110000b,tf,tdisp
cjs	db 01111000b,tf,tdisp
cjne	db 01110101b,tf,tdisp
cjnl	db 01111101b,tf,tdisp
cjnle	db 01111111b,tf,tdisp
cjnb	db 01110011b,tf,tdisp
cjnbe	db 01110111b,tf,tdisp
cjnp	db 01111011b,tf,tdisp
cjno	db 01110001b,tf,tdisp
cjns	db 01111001b,tf,tdisp
cjmps	db 11101011b,tf,tdisp
cloop	db 11100010b,tf,tdisp
cloope	db 11100001b,tf,tdisp
cloopne db 11100000b,tf,tdisp
cjcxz	db 11100011b,tf,tdisp

cin	db tf,trun
	call accw
	jc ioerro
	call lpcomma
	jc ioerro
	lea di,cin22
	jmp loop
ioerro	ret
cin22	db tf,tcomp,'dx',0
	db tf,trun
	mov al,wtreg
	or al,0ech
	jmp lput
	db tf,tsept,tf,trun
	mov al,wtreg
	or al,11100100b
	mov [bp],al
	inc bp
	call decimal
	jc ioerro
	jmp lput

cout	db tf,tcomp,'dx',0,tf,tcomma,tf,trun
	call accw
	jc ioerro
	mov al,wtreg
	or al,0eeh
	jmp lput
	db tf,tsept,tf,tnum,tf,tcomma,tf,trun
	call accw
	jc ioerro
	mov al,11100110b
	or al,wtreg
	mov [bp],al
	inc bp
	mov al,wnum
	jmp lput

cpop	db tf,trun
cpoplp	lea di,cpopom
	call loop
	jc mxpc
	call lpcomma
	jc mxpret
	call pssp
	jmps cpoplp

cpopom	db tf,tregw,01011000b,tf,trun
cput	mov al,wreg
	jmp lput
	db tf,tsept
	db tf,tseg,111b,tf,trun
	call skip2
	cmp al,':'
	jne cput
	lea di,cpop2
	jmp loop
	db tf,tsept
cpop2	db tf,treset,tf,tmxr,10001111b,tf,trun
	sub al,al
mxrput	or al,wmod
	or al,wr_m	;Put the details together.
	mov [bp],al
	inc bp
	cmpb wmod,64	;Put one byte displacement.
	je mxp1
	cmpb wmod,128	;Put one word disp.
	je mxp2
	cmpb wmod,0
	jne mxpret
	cmpb wr_m,6	;If just displacement then put it.
	je mxp2
mxpret	clc
mxpc	ret
mxp1	mov al,wdisp
	jmp lput
mxp2	mov ax,wdisp
	jmp lpute2

cpush	db tf,trun
cpushlp lea di,cpushom
	call loop
	jc mxpc
	call lpcomma
	jc mxpret
	call pssp
	jmps cpushlp

cputj	jmp cput

cpushom db tf,tregw,01010000b,tf,trun
	jmps cput
	db tf,tsept
	db tf,tseg,110b,tf,trun
	call skip2
	cmp al,':'
	jne cputj
	lea di,cpush2
	jmp loop
	db tf,tsept
cpush2	db tf,treset,tf,tmxr,11111111b,tf,trun
	mov al,110000b
	jmps mxrput


clea	db tf,trun
	mov cl,10001101b
cclll	mov stv,cl
	lea di,regw
	call qubw
	jc lllerr
	mov wtreg,1
	mov al,cl
	add al,al
	add al,al
	add al,al	;AL*8
	mov storea,al
	call lpcomma
	jc lllerr
	inc bp
	call lpmxr
	dec bp
	jc lllerr

	call test386

	mov al,stv	;Put the independent byte.
	mov [bp],al
	inc bp
	mov al,storea
	jmp mxrput     ;Put MOD REG R/M
lllerr	stc
	ret

clds	db tf,trun
	mov cl,11000101b
	jmps cclll

cles	db tf,trun
	mov cl,11000100b
	jmps cclll

cesc	db tf,tmxr,11011000b,tf,trun
	jmp mxrput

cshl	db tf,twvmxr,100000b,tf,tend
cshr	db tf,twvmxr,101000b,tf,tend
csar	db tf,twvmxr,111000b,tf,tend
crcl	db tf,twvmxr,010000b,tf,tend
crcr	db tf,twvmxr,011000b,tf,tend
crol	db tf,twvmxr,000000b,tf,tend
cror	db tf,twvmxr,001000b,tf,tend
cshlb	db tf,tbyte,tf,twvmxr,100000b,tf,tend
cshrb	db tf,tbyte,tf,twvmxr,101000b,tf,tend
csarb	db tf,tbyte,tf,twvmxr,111000b,tf,tend
crclb	db tf,tbyte,tf,twvmxr,010000b,tf,tend
crcrb	db tf,tbyte,tf,twvmxr,011000b,tf,tend
crolb	db tf,tbyte,tf,twvmxr,000000b,tf,tend
crorb	db tf,tbyte,tf,twvmxr,001000b,tf,tend
cshlw	db tf,tword,tf,twvmxr,100000b,tf,tend
cshrw	db tf,tword,tf,twvmxr,101000b,tf,tend
csarw	db tf,tword,tf,twvmxr,111000b,tf,tend
crclw	db tf,tword,tf,twvmxr,010000b,tf,tend
crcrw	db tf,tword,tf,twvmxr,011000b,tf,tend
crolw	db tf,tword,tf,twvmxr,000000b,tf,tend
crorw	db tf,tword,tf,twvmxr,001000b,tf,tend

cinc	db tf,tregw,01000000b,tf,trun
cid	mov al,wreg
	jmp lput
	db tf,tsept
	db tf,twmxr,11111110b,000000b,tf,tend

cdec	db tf,tregw,01001000b,tf,trun
	jmps cid
	db tf,tsept
	db tf,twmxr,11111110b,001000b,tf,tend

cincb	db tf,tbyte,tf,trun
cibw	lea di,cinc
	jmp loop

cincw	db tf,tword,tf,trun
	jmps cibw

cdecb	db tf,tbyte,tf,trun
cdbw	lea di,cdec
	jmp loop

cdecw	db tf,tword,tf,trun
	jmps cdbw

cnot	db tf,twmxr,11110110b,010000b,tf,tend
cneg	db tf,twmxr,11110110b,011000b,tf,tend
cmul	db tf,twmxr,11110110b,100000b,tf,tend
cimul	db tf,twmxr,11110110b,101000b,tf,tend
cdiv	db tf,twmxr,11110110b,110000b,tf,tend
cidiv	db tf,twmxr,11110110b,111000b,tf,tend
cnotb	db tf,tbyte,tf,twmxr,11110110b,010000b,tf,tend
cnegb	db tf,tbyte,tf,twmxr,11110110b,011000b,tf,tend
cmulb	db tf,tbyte,tf,twmxr,11110110b,100000b,tf,tend
cimulb	db tf,tbyte,tf,twmxr,11110110b,101000b,tf,tend
cdivb	db tf,tbyte,tf,twmxr,11110110b,110000b,tf,tend
cidivb	db tf,tbyte,tf,twmxr,11110110b,111000b,tf,tend
cnotw	db tf,tword,tf,twmxr,11110110b,010000b,tf,tend
cnegw	db tf,tword,tf,twmxr,11110110b,011000b,tf,tend
cmulw	db tf,tword,tf,twmxr,11110110b,100000b,tf,tend
cimulw	db tf,tword,tf,twmxr,11110110b,101000b,tf,tend
cdivw	db tf,tword,tf,twmxr,11110110b,110000b,tf,tend
cidivw	db tf,tword,tf,twmxr,11110110b,111000b,tf,tend

cprocn	db tf,tcomp,'near',0
	db tf,trun
	sub cl,cl
	jmps cpcnear
	db tf,tsept
	db tf,tcomp,'far',0,tf,trun
	mov cl,1
	jmps cpcomn
	db tf,tsept,tf,trun
cpcnear sub cl,cl
cpcomn	mov di,pset
	cmp di,offset(pnest)+32 ;Any room in table left?
	jb cpcset
	mov al,22
	stc
	ret
cpcset	mov [di],cl	;Put type (0=near,1=far).
	incw pset
	clc
	ret

cendp	db tf,trun
	mov di,pset
	cmp di,offset(pnest)
	jne cendget		;Any PROCs?
	mov al,23	;Error - none.
	stc
	ret
cendget decw pset	;Decrease proc list by one.
	clc
	ret

cret	db tf,trun
	call decimal		;Expression?
	mov di,pset		;Get current proc address
	jnc retmore
	mov al,11000011b	;Within segment.
	cmpb -1[di],0		;Near?
	je cretput
	mov al,11001011b	;Intersegment.
cretput jmp lput
retmore mov bl,11000010b
	cmpb -1[di],0
	je crmorep
	mov bl,11001010b
crmorep mov [bp],bl
	mov [bp+1],ax
	add bp,3
	ret

fcomma	mov al,10	;Error default = missing comma.
	mov si,spoint
fclp1	cmpb [si],13
	je fcnret	;No comma?
	cmpb [si],','
	je fcret
	inc si
	jmps fclp1
fcnret	stc
fcret	ret


aox	db 0,0,0,0
and1	dw 0

andflj	jmp andfull
cand	db tf,trun
candcm	movb aox+0,00100000b
	movb aox+1,10000000b
	movb aox+2,100000b
	movb aox+3,00100100b
maox	movb wdo,1
taox	movb sflag,0
saox	call skip2
	mov cx,16
	lea di,quall
	call qureg	;Is it a register?
	jc andflj

	call test386

	call lpcomma
	jc anderr
	mov al,1
	cmp cl,8
	jb satt
	sub cl,8
	dec al
satt	mov wtreg,al
	push cx 	;Save register.

	push spoint
	inc bp
	call immedi	;Get number? [AND1]
	dec bp
	jc reglpx
	mov si,spoint
	cmpb [si],'['
	je reglpx
	pop cx,cx	;Drop SPOINT and retrieve register.
	or cl,cl
	jnz regci
	mov al,aox+3	;Acc,immediate
	or al,wtreg
regand	mov ah,wtreg
	mov wtcmd,ah
	mov [bp],al
	inc bp
	mov ax,and1
	jmp putlw

regci	mov al,aox+1	;Reg,immediate
	or al,wtreg
	mov [bp],al
	inc bp
	mov al,11000000b
	or al,cl
	or al,aox+2
	mov cx,1
	call idlast
	jmps regand

anderr	ret

reglpx	pop spoint	;REG,MEM/REG
	movb wd,2	;Restore SPOINT - REG is still on stack.
	cmpb wdo,0	;TO reg
	jnz regwdo
	movb wd,0	;Force to zero regardless if WDO=0 (TEST).
regwdo	call wcheck
	pop cx
	jc anderr
	mov and1,cl	;Put register.
	inc bp
	call lpmxr
	dec bp
	jc anderr

	call test386

	mov al,aox
	or al,wtcmd
	or al,wd
	mov [bp],al
	inc bp
	mov al,and1	;Get register*8
	add al,al
	add al,al
	add al,al
	jmp mxrput	;Put the memory details.

andimm	inc bp
	call immedi
	dec bp
	jc anderr
	call wcheck
	jc anderr
	mov al,aox+1
	or al,wtcmd
	mov [bp],al
	inc bp
	mov al,aox+2
	call mxrput
	mov ax,and1
	jmp putlw

andfull inc bp
	call lpmxr
	dec bp
	jc anderr
	call lpcomma
	jc anderr
	mov cx,16
	lea di,quall
	call qureg	;Is it a register?
	jc andimm

	call test386

	mov al,1
	cmp cl,8
	jb fatt
	sub cl,8
	dec al
fatt	mov wtreg,al

	movb wd,0	;To memory.
	call wcheck
	jnc nandf
	ret
nandf	mov and1,cl
	mov al,aox
	or al,wtcmd
	or al,wd
	call lput
	mov al,and1	;Get register*8
	add al,al
	add al,al
	add al,al
	jmp mxrput	;Put the memory details.


candb	db tf,tbyte,tf,trun
	jmp candcm

candw	db tf,tword,tf,trun
	jmp candcm

cxor	db tf,trun
cxorcm	movb aox+0,00110000b
	movb aox+1,10000000b
	movb aox+2,110000b
	movb aox+3,00110100b
	jmp maox

cxorb	db tf,tbyte,tf,trun
	jmps cxorcm

cxorw	db tf,tword,tf,trun
	jmps cxorcm

cor	db tf,trun
corcm	movb aox+0,00001000b
	movb aox+1,10000000b
	movb aox+2,001000b
	movb aox+3,00001100b
	jmp maox

cadc	db tf,trun
cadcm	movb aox+0,00010000b
	movb aox+1,10000000b
	movb aox+2,010000b
	movb aox+3,00010100b
maoxs	movb sflag,1
	movb wdo,1
	jmp saox

cadd	db tf,trun
caddm	movb aox+0,00000000b
	movb aox+1,10000000b
	movb aox+2,000000b
	movb aox+3,00000100b
	jmp maoxs

csbb	db tf,trun
csbbm	movb aox+0,00011000b
	movb aox+1,10000000b
	movb aox+2,011000b
	movb aox+3,00011100b
	jmp maoxs

csub	db tf,trun
csubm	movb aox+0,00101000b
	movb aox+1,10000000b
	movb aox+2,101000b
	movb aox+3,00101100b
	jmp maoxs

ccmp	db tf,trun
ccmpm	movb aox+0,00111000b
	movb aox+1,10000000b
	movb aox+2,111000b
	movb aox+3,00111100b
	jmp maoxs

corb	db tf,tbyte,tf,trun
	jmp corcm

corw	db tf,tword,tf,trun
	jmp corcm

cadcb	db tf,tbyte,tf,trun
	jmp cadcm

cadcw	db tf,tword,tf,trun
	jmp cadcm

caddb	db tf,tbyte,tf,trun
	jmp caddm

caddw	db tf,tword,tf,trun
	jmp caddm

csbbb	db tf,tbyte,tf,trun
	jmp csbbm

csbbw	db tf,tword,tf,trun
	jmp csbbm

csubb	db tf,tbyte,tf,trun
	jmp csubm

csubw	db tf,tword,tf,trun
	jmp csubm

ccmpb	db tf,tbyte,tf,trun
	jmp ccmpm

ccmpw	db tf,tword,tf,trun
	jmp ccmpm

ccall	db tf,trun
	mov omust,oneadd
	inc bp
	call decis	;Direct offset? - memory pointer will have to be [m]
	dec bp
	jc ccalmx
	mov and1,ax
	call skip2
	cmp al,':'      ;Far jump?
	mov al,11101000b
	jne ccpp1
	incw spoint
	add bp,3
	call immedi
	jc ccaerr
	mov [bp],ax
	movb [bp-3],10011010b
	mov ax,and1
	mov [bp-2],ax
	add bp,2
	ret
ccpp1	mov [bp],al
	inc bp
	mov ax,and1
	sub ax,bp
	dec ax
	dec ax
	jmp lpute2
ccalmx	inc bp
	call lpmxr
	jc ccaerr
	movb [bp-1],0ffh
	mov al,010000b
	jmp mxrput
ccaerr	stc
	ret

ccallf	db tf,trun
	inc bp
	call lpmxr
	dec bp
	jc ccaerr

	call test386

	movb [bp],0ffh
	inc bp
	mov al,011000b
	jmp mxrput

cjmp	db tf,trun
	mov omust,oneadd
	inc bp
	call decis
	dec bp
	jc cjmpmx
	mov and1,ax
	call skip2
	cmp al,':'
	mov al,11101001b
	jne ccpp1
	incw spoint
	add bp,3
	call immedi
	jc ccaerr
	mov [bp],ax
	movb [bp-3],11101010b
	mov ax,and1
	mov [bp-2],ax
	add bp,2
	ret
cjmpmx	inc bp
	call lpmxr
	jc ccaerr
	movb [bp-1],0ffh
	mov al,100000b
	jmp mxrput

cjmpf	db tf,trun
	inc bp
	call lpmxr
	dec bp
	jc ccaerr
	movb [bp],0ffh
	inc bp
	mov al,101000b
	jmp mxrput

cxchg	db tf,trun
xchgcom call fcomma
	jc xcerr
	inc si
	mov spoint,si
	call accw	;Accummulator?
	jc xcfull
	cmp wtreg,1	;AX more specifically?
	jne xcfull
	push spoint
	call restsp
	lea di,regw
	call qubw	;Word register?
	pop spoint
	jc xcfull
	mov al,cl
	or al,10010000b
	jmp lput
xcfull	call restsp
	inc bp
	call lpmxr
	dec bp
	jc xcerr
	call lpcomma
	jc xcerr

	mov wtreg,1
	lea di,regw
	call qubw
	jnc xcput
	mov wtreg,0
	lea di,regb
	call qubw
	jc xcerr
xcput	call wcheck
	jc xcerr

	call test386

	mov al,wtcmd
	or al,10000110b
	mov [bp],al
	inc bp
	mov al,cl
	add al,al
	add al,al
	add al,al
	jmp mxrput
xcerr	ret

cxchgb	db tf,tbyte,tf,trun
	jmp xchgcom

cxchgw	db tf,tword,tf,trun
	jmp xchgcom

ctest	db tf,trun
testcom movb aox+0,10000100b
	movb aox+1,11110110b
	movb aox+2,000000b
	movb aox+3,10101000b
	movb wdo,0	;No D setting.
	jmp taox

ctestb	db tf,tbyte,tf,trun
	jmps testcom

ctestw	db tf,tword,tf,trun
	jmps testcom

cmovb	db tf,tbyte,tf,trun
	jmps movcom

cmovw	db tf,tword,tf,trun
	jmps movcom

movlpj	jmp movlpx
moverr0 stc
	ret
cmov	db tf,trun
movcom	call skip2
	jc moverr0
	cmp al,'['
	push spoint
	je movlpj
	lea di,quall
	mov cx,20
	call qureg
	jc movlpj
	cmpb [si],':'   ;Segment override, not a register instruction.
	je movlpj
	pop dx		;Drop old spoint.

	call test386

	call lpcomma
	jc moverr0

	mov al,1
	cmp cl,8
	jb matt
	sub cl,8
	dec al
matt	mov wtreg,al
	mov movsto,cl
	cmp cl,8
	jb mab
	jmp msrm

mab	mov si,spoint	;MOV REG, ?
	mov al,[si]

	push spoint
	call ifdigit
	jc mrij
	inc bp
	call immedi	;If undefined here then must be memw/memb (default).
	dec bp
	jnc mri2j
	pop spoint
micont	push spoint
	mov cx,4
	lea di,seg
	call qureg	;MOV REG,Segment
	jc mrs
	cmpb [si],':'
	jne mrsg
mrs	pop spoint
	inc bp
	call lpmxr
	dec bp
	jc movec

	call test386

	mov al,10001010b	;TO register.
msaput	or al,wtreg
	mov [bp],al
	inc bp
	call wcheck
	jc movec
	mov al,movsto	;Register
msrput	add al,al
	add al,al
	add al,al
	jmp mxrput

mrij	jmps mri
mri2j	jmps mri2
movec	ret

mrsg	pop bx
	mov wtreg,2	;No type.
	movb [bp],10001100b
	mov al,movsto
	add cl,cl
	add cl,cl
	add cl,cl
	or al,cl
	or al,11000000b
	mov [bp+1],al
	add bp,2
	ret

msrm	movb wd,2
	mov wtreg,2
	inc bp
	call lpmxr
	dec bp
	jc movec

mseg2	call test386

	mov al,10001100b
	or al,wd
	mov [bp],al	;MOV seg,rm
	inc bp
	mov al,movsto
	sub al,8
	jmps msrput

mri	inc bp
	call immedi
	dec bp
	pop bx
	jnc mrie
	jmps mrirst
mri2	pop bx
mrie	mov si,spoint
	cmpb [si],'['
	jne micc
mrirst	mov spoint,bx
	jmp micont
micc	mov al,wtreg
	mov wtcmd,al
	add al,al
	add al,al
	add al,al
	or al,10110000b ;MOV reg,imm
	or al,movsto
	mov [bp],al
	inc bp
	mov ax,and1
	jmp putlw

movecr	ret
movlpx	pop spoint
	inc bp
	call lpmxr
	dec bp
movec1	jc movecr
	call lpcomma
	jc movec1

	lea di,quall
	mov cx,20
	call qureg
	jc mrmi

	call test386

	mov al,1
	cmp cl,8
	jb mratt
	sub cl,8
	dec al
mratt	mov wtreg,al
	mov movsto,cl
	cmp cl,8
	jae mrms		;FROM segment?
	mov al,10001000b	;MOV rm,r
	jmp msaput

mrms	movb wd,0
	jmp mseg2

mrmi	mov mrm1,bp
	call immedi
	jc movec1
	call wcheck
	jc movec1
	mov al,11000110b
	or al,wtcmd
	mov [bp],al
	inc bp
	sub al,al
	call mxrput
	mov cx,bp
	sub cx,mrm1	;How many added for immediate value.
	call idlast
	mov ax,and1
	jmp putlw
mrm1	dw ?

cequ	db tf,trun
	jmp dofas	;Drop DOLINE call (ie ignore eol).

cretf	db tf,trun
	call decimal		;Expression?
	jc rf0cb
	movb [bp],0cah		;RETF n
	mov [bp+1],ax
	add bp,3
	ret

rf0cb	movb [bp],0cbh		;RETF
	inc bp
	clc
	ret

mxdisp	mov omust,oneadd
	call decis
	mov wdisp,ax
	movb wmod,128	;Default word displacement.
	jc mxderr
	cmp defd,0
	jz mxdef	;If undefined then MUST assume a word value.
	cmp ax,127
	jg mxdef
	cmp ax,-128
	jl mxdef
	movb wmod,64	;If short offset then MOD=64
mxdef	clc
mxderr	ret

idlast	cmpb defd,0
	jnz idlr
	mov es,oneseg
	mov di,oplace
	add es:[di-4],cx
idlr	ret

;==============================================================================

test386 pushf

	cmpb is32bit,1
	jb not386	;not 32 bit
	ja not386	;already included prefix

	movb [bp],66h	;80386 32 BIT OVERRIDE
	inc bp

	movb is32bit,2	;flag

not386	popf
	ret

;==============================================================================

fsend	endp
