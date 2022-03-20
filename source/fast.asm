
;===========================================================================
;==									  ==
;==		     FAST LANGUAGE SOURCE FILE	F*.ASM			  ==
;==									  ==
;==		    WRITTEN BY PETER CAMPBELL 1987-1990 		  ==
;==									  ==
;===========================================================================


fast	equ 1
sofa	equ 0

i386	equ 0
i86	equ 1

	jmp sfast	;Begin after HFTABLE and other tables.


	ds 140	;Enough for SPECLEN - so some routines called when compiling
		; eg EMUL32 can safely use their variables.


bigfind equ 25
tf	equ 159

variabl equ 0
label	equ 1
array	equ 2
labiv	equ 5

const32 equ 7
var32	equ 8

functio equ 10 ;Not used?

t_fmax	equ 50
t_base	equ t_fmax*64+1

udlab	equ 20	;One undefined type in FAST, labels only.

oneadd	equ 250
onesub	equ 251
onelib	equ 252
onetot	equ 253

onelibo	equ 52


;* LIBRARY INFORMATION *

libs		equ 62
librts		equ 103
speclen 	equ 172 	;Starting position for code.

scrseg		equ 3+256
scrcol		equ 5+256
scrpos		equ 7+256
count		equ 9+256
monof		equ 11+256
errorv		equ 13+256
ihere		equ 15+256
runerra 	equ 17+256
tracev		equ 19+256
debugv		equ 21+256
debugi		equ 23+256

ssss		equ 25+256
sssp		equ 27+256
rndn1		equ 29+256
rndn2		equ 31+256
rndn3		equ 33+256
rndn4		equ 35+256
oldoff		equ 37+256
oldseg		equ 39+256
restoff 	equ 41+256
restseg 	equ 43+256
oldboff 	equ 45+256
oldbseg 	equ 47+256
ikeyoff 	equ 49+256
ikeyseg 	equ 51+256

windows 	equ 53+256
wpos		equ 54+256
options 	equ 56+256
eoff1		equ 57+256
wxy		equ 58+256
modew		equ 60+256
wscrpos 	equ 61+256
wscrcol 	equ 63+256
modeop		equ 65+256
hitw1		equ 66+256
hitw2		equ 68+256
hitd1		equ 70+256
hitd2		equ 72+256
hitc		equ 74+256
hity2		equ 76+256
and1		equ 77+256
and2		equ 78+256
rotate		equ 79+256
width2		equ 81+256
spage		equ 83+256
newx		equ 85+256
newy		equ 87+256
lastx		equ 89+256
lasty		equ 91+256
xstep		equ 92+256
ystep		equ 93+256
dlow		equ 94+256
idec		equ 95+256
bmul		equ 96+256
idleo		equ 97+256
idles		equ 99+256
fbpvar		equ 101+256
ser_seg 	equ 103+256
ser_siz 	equ 105+256
ser_hed 	equ 107+256
com_base	equ 109+256
com_irq 	equ 111+256
com_enable	equ 113+256
com_port	equ 115+256
fscreen_size	equ 117+256
fscreen_cols1	equ 119+256
fscreen_cols2	equ 121+256
fscreen_rows1	equ 123+256
qs_left 	equ 128+256	;free space - 4 byte alignment
qs_right	equ 132+256
left32		equ 136+256
right32 	equ 140+256
pivot32 	equ 144+256
nelem32 	equ 148+256
pivotend32	equ 152+256
pivottemp32	equ 156+256
lefttemp32	equ 160+256
lnum32		equ 164+256
com_seg 	equ 168+256	;unused
com_off 	equ 170+256	;unused

specvar db 'video @ 103h',13
	db 'colour @ 105h',13
	db 'color @ 105h',13
	db 'locpos @ 107h',13
	db 'duration @ 109h',13
	db 'mono @ 10bh',13
	db 'error @ 10dh',13
	db 'ihere @ 10fh',13
	db 'errorv @ 111h',13
	db 'find_bin_put @ 165h',13
	db 'serial_seg @ 167h',13
	db 'serial_size @ 169h',13
	db 'serial_head @ 16bh',13
	db 'com_base @ 16dh',13
	db 'com_irq @ 16fh',13
	db 'com_enable @ 171h',13
	db 'com_port @ 173h',13
	db 'fscreen_size @ 175h',13
	db 'fscreen_cols1 @ 177h',13
	db 'fscreen_cols2 @ 179h',13
	db 'fscreen_rows1 @ 17bh',13
	db 'const qs_left=180h',13
	db 'const qs_right=184h',13
	db 26		;End the compiled sub-code.

colerr	dw ?	;Used by COLOUR and ERROR.
ecurnt	dw ?
ecurst	ds 5*2
ecurend

localm	db 0	;not local variables
inproc	db 0	;defining a procedure or function?

bept	db 1,1,1,1,1,1,1,1 ;0
	db 1,0,0,1,1,0,1,1 ;8
	db 1,1,1,1,1,1,1,1 ;16
	db 1,1,0,0,1,1,1,1 ;24
	db 0,0,0,0,1,1,1,0 ;32
	db 0,0,0,0,0,0,0,0 ;40
	db 1,1,1,1,1,1,1,1 ;48
	db 1,1,0,0,0,0,0,0 ;56
	db 0,1,1,1,1,1,1,1 ;64
	db 1,1,1,1,1,1,1,1 ;72
	db 1,1,1,1,1,1,1,1 ;80
	db 1,1,1,0,1,0,1,1 ;88
	db 1,1,1,1,1,1,1,1 ;96
	db 1,1,1,1,1,1,1,1 ;104
	db 1,1,1,1,1,1,1,1 ;112
	db 1,1,1,0,0,0,1,1 ;120


hftable ;Main codes first followed by sub procedures, order within two groups not important.
xhfmain equ 0
	db xhfmain
xhfrun	equ 1
	db xhfrun
xikeyit equ 2
	db xikeyit
xhfint	equ 3
	db xhfint
xhandle equ 4
	db xhandle
xgetrnd equ 5
	db xgetrnd
xadvanc equ 6
	db xadvanc
;	equ 7
	db ?
xhfloc	equ 8
	db xhfloc
xwlocat equ 9
	db xwlocat
xprint	equ 10
	db xprint
xsprint equ 11
	db xsprint
xhdir	equ 12
	db xhdir
xpmessp equ 13
	db xpmessp
xspmess equ 14
	db xspmess
xbpmess equ 15
	db xbpmess
xlpmess equ 16
	db xlpmess
xprintb	equ 17
	db xprintb
xsprinb	equ 18
	db xsprinb
xlprinb	equ 19
	db xlprinb
xbpb	equ 20
	db xbpb
xprinth equ 21
	db xprinth
xsprinh equ 22
	db xsprinh
xbprinh equ 23
	db xbprinh
xlprinh equ 24
	db xlprinh
xtestsc equ 25
	db xtestsc
xsptest equ 26
	db xsptest
xcpos	equ 27
	db xcpos
xsprite equ 28
	db xsprite
xrthit	equ 29
	db xrthit
xtakey	equ 30
	db xtakey
xinpax	equ 31
	db xinpax
xhinpax equ 32
	db xhinpax
xgetiii equ 33
	db xgetiii
xwdata	equ 34
	db xwdata
xprm	equ 35
	db xprm
xwclose equ 36
	db xwclose
xwopen	equ 37
	db xwopen
xhftrc	equ 38
	db xhftrc
xmove	equ 39
	db xmove
xline	equ 40
	db xline
xhfdbug equ 41
	db xhfdbug
xmul32	equ 42
	db xmul32
xdiv32	equ 43
	db xdiv32
xplot	equ 44
	db xplot
xpoint	equ 45
	db xpoint
xpadd	equ 46
	db xpadd
xermsg	equ 47
	db xermsg
xexit	equ 48
	db xexit
xseek	equ 49
	db xseek
xehon	equ 50
	db xehon
xehoff	equ 51
	db xehoff
xherc	equ 52
	db xherc
xfind	equ 53
	db xfind
xsinit	equ 54
	db xsinit
xprotek equ 55
	db xprotek
xmul64	equ 56
	db xmul64
xdiv64	equ 57
	db xdiv64
xprn64	equ 58
	db xprn64
xqsort	equ 59
	db xqsort
x386	equ 60
	db x386
xvga_line equ 61
	db xvga_line

xbufft	equ libs+0
	db xhftrc
xmitab	equ libs+1
	db xikeyit
xexmsel equ libs+2
	db xwopen
xwclosa equ libs+3
	db xwdata
xtabint equ libs+4
	db xhfint
xhflp	equ libs+5
	db xlprinb
xlpdigh equ libs+6
	db xlprinh
xhfp	equ libs+7
	db xprintb
xpdigh	equ libs+8
	db xprinth
xhfsp	equ libs+9
	db xsprinb
xspdigh equ libs+10
	db xsprinh
xhfpb	equ libs+11
	db xbpb
xbpdigh equ libs+12
	db xbprinh
xigbrek equ libs+13
	db xhfint
xtakchk equ libs+14
	db xtakey
xbprinb equ libs+15
	db xbprinh
xlpchar equ libs+16
	db xlprinh
xinplen equ libs+17
	db xinpax
xtscrl	equ libs+18
	db xtestsc
xlpdigv equ libs+19
	db xhflp
xbpdigv equ libs+20
	db xhfpb
xpdigv	equ libs+21
	db xhfp
xspdigv equ libs+22
	db xhfsp
xdighp	equ libs+23
	db xprinth
xsdighp equ libs+24
	db xsprinh
xbdighp equ libs+25
	db xbprinh
xldighp equ libs+26
	db xlprinh
xhcode	equ libs+27
	db xherc
xwprt	equ libs+28
	db xwlocat
xwpms	equ libs+29
	db xwlocat
xspent	equ libs+30
	db xsprite
xopm1	equ libs+31
	db xsprite
xopm2	equ libs+32
	db xsprite
xopm3	equ libs+33
	db xsprite
xopm1a	equ libs+34
	db xsprite
xopm2a	equ libs+35
	db xsprite
xopm3a	equ libs+36
	db xsprite
xwposdi equ libs+37
	db xwopen
pma1	equ libs+38
	db xprm
pma2	equ libs+39
	db xprm
pma3	equ libs+40
	db xprm
xsstop	equ libs+41
	db xsinit
xsint	equ libs+42
	db xsinit

;End library constants.


trun	equ 1
tnum	equ 2
tnum8	equ 3
texp	equ 4
pnum	equ 5	   ;Prefix of P = put word value into code.
pnum8	equ 6
pvar	equ 7
tstat	equ 8	   ;Compile one statement.
tifnum	equ 9
tifvar	equ 10
tvar	equ 11
passvar equ 12
texp1	equ 13
tcomma	equ 14
treset	equ 15
tifnum1 equ 16
trcode	equ 17
pnum2	equ 18
tequal	equ 19
thandle equ 20
thget	equ 21
tname	equ 22
terrj	equ 23
tseg	equ 24
tword	equ 25
hfuse	equ 26
hfset	equ 27
hcall	equ 28
hjump	equ 29
stop	equ 30
tsept	equ 31
tcomp	equ 32
texp32	equ 33
put32	equ 34
get32	equ 35
passp	equ 36
tnd	equ 37
tifnd	equ 38
tsetp	equ 39
terrs	equ 40
notto	equ 41
tutopia equ 42
tfsign	equ 43
tisign	equ 44
tpopds	equ 45
thex	equ 46
tchr	equ 47
ptitle	equ 48
savenum equ 49
loadnum equ 50
pnuml	equ 51		;put number long
maxlps	equ 51
dwlps	dw looprun,loopnum,loopnm8,loopexp
	dw lpnum,lpnum8,lpvar,lpstat
	dw loopifn,loopifv,loopvar,lpavar
	dw oneexp,lpcomma,restsp,loopin1
	dw ltcode,lpnum2,lpequal,lphand
	dw lphget,name,lperrj,loopseg
	dw lopword,hfuset,hfsett,hfcall
	dw hfjump,lpstop,lpnull,lpcomp
	dw lexp32,lput32,lget32,lpasp
	dw lpnd,lpifnd,pssp,lperrs
	dw lpnull,lutopia,lfsign,lisign
	dw loopds,lthex,ltchr,ltitle
	dw lsaven,lloadn,lpnuml

defext1 equ 'F'
defext2 equ 0
defext3 equ 0


sfast	#include fsi.asm

compile lea di,hflags	;Reset the libs flags.
	mov cx,libs
libbits movb [di],0
	inc di
	loop libbits

	mov ax,offset(rnest)
	mov repnest,ax		;Set REPEAT stack.

	movb [bp],0e9h
	inc bp
	mov al,onelibo
	mov cl,xhfmain
	call hfncsu	;Find address of main procedure.

	add bp,speclen-3
	movb ss:[scrcol],7	;Default colour.
	movb ss:[debugi],0c3h	;RET instruction for DEBUG vector.

	movw ss:[com_port],0		;default COM1
	movw ss:[com_base],03f8h	;default com1 base address
	movw ss:[com_irq],4		;default com1 irq
	movw ss:[com_enable],0efh	;default com1 enable mask

	movw ss:[fscreen_size],4000	;default screen details
	movw ss:[fscreen_cols1],80
	movw ss:[fscreen_cols2],160
	movw ss:[fscreen_rows1],25

	lea dx,filen+2

compil2 call t_anof
	call prtdxn
	mov ax,3d00h
	int 21h 	;OPEN FILE
	jnc opedok
	jmp serror
opedok	mov handle,ax

compspe mov ax,-1
	push ax 	;Stack START pointer.

	cmpb fspec,1
	je bufspec
	mov ax,bufsize
	add ax,offset(fsend)+256
	mov buffp,ax
	mov stab,ax	;Set offset for trace.

	movw strace,0
	movw strace+2,0 ;Rest STRACE to 0.
	movb irange,0	;Default = long for every file.

bufspec movw line,1	;Once file is opened then start reading in each sector and compiling it
	jmp compstc	;a line at a time.


;Compile the line.
doline	movb dmust,1	;Default just in case some routines call DECMAL.
	call skip2
	jc dolskip
	mov si,ifpoint
	cmpb [si],0	;ENDIF check?
	ja dolnorm

	call skip2
	jc dolend
	cmp al,'#'
	jne dolend	;Must be #endif or #if.

	inc spoint
	lea di,cfast
	call fentry2	;Which FAST command?
	mov al,136	;Unknown directive.
	jc dfcret

	mov di,[di+1]
	cmp di,offset(cifend)
	je efcont	;ENDIF?
	cmp di,offset(cdelse)
	je efelse	;ELSE?
	cmp di,offset(ccpif)
	jne dolend	;IF?

	incw ifpoint
	mov si,ifpoint
	movb [si],0	;False, so continue with false.
	jmps dolend

efelse	mov si,ifpoint
	cmp si,offset ifstart
	je else_1	;first else?
	cmpb [si-1],0	;#if false #if xxx #else #else ???
	je else_no
else_1	xorb [si],1	;toggle #if flag
else_no clc
	ret
efcont	decw ifpoint
dolskip clc
dfcret	ret
dolend	mov ax,cs
	mov es,ax
	cld
	mov cx,256
	mov al,13
	mov di,spoint
	repne scasb
	dec di
	mov spoint,di
	clc
	ret

lltrc	dw -1
dolnorm testb doption,1
	jz dnoutt

;== trace ==================================================================

	cmpb fspec,1
	je dnoutt	;Ignore trace if compiling from memory.

	mov si,stab
	cmp si,lltrc	;Don't trace same line twice!
	je dnoutt
	mov lltrc,si

	call skip2
	jc dnot2	;Don't trace blank lines or comments.

	push spoint
	call find
	pop spoint
	jc t_labq

	mov di,[di+1]
	cmpw [di],notto*256+tf
	je dnot2		;Don't trace DATA[B], INLINE, STRING or FNAME.
	jmps truck

t_labq	mov si,spoint
	mov al,[si]
	call lowq
	jc truck
lpptrc	mov al,[si]
	inc si
	call sept
	jnc lpptrc
	cmp al,':'      ;WORD: ?
	je dnot2	;If label then don't trace it!!!

truck	call t_put	;Put breakpoint and setup table.

;== debug ==================================================================

dnoutt	testb doption,2
	jz dnot2
	movb [bp],2eh		;CS:
	movw [bp+1],16ffh	;CALL [0115]
	movw [bp+3],0115h
	add bp,5

;===========================================================================

dnot2	movb ifuse,0
	movb sign,1	;Default to signed expressions.
	mov si,spoint
	cmpb [si],'{'
	jne donobb
	incw spoint	;Get next character.
	call cobrace
	jc adwerr
	jmp doline

donobb	mov si,spoint
	mov colerr,si
	cmpb [si],'#'
	jne dfcont3

	inc spoint
	lea di,cfast
	call fentry2	;Which FAST command?
	mov al,136	;Unknown directive.
	jnc dfcontu
	ret

dfcont3	call find	;Is the first word a valid keyword?
	jc dofin2	;Assume a variable or label if not a keyword.

dfcontu	call pssp	;Store SPOINT (see restsp)

	lea si,ecurst	;Set TERRS, TERRJ pointers.
	mov ecurnt,si

	mov di,1[di]	;Get address of command table.
	jmp loop	;Return to LOOPFIN to check EOL, errors and ':'.

dofin2	call findbin	;Does word exist?
	mov es,labseg
	mov addf1,si

	jc addwol
	cmpb es:[di+1],label
	jne process
	mov al,3	;Double definition of label.
adwrc	stc
adwerr	ret

addwol	jz adwrc	;Valid word?

	call addfb	;Try to add word to the binary sorted table.
	jc adwerr
	call gtypeal

	add di,10	;was 8, now store usage counter
	mov lablast,di
	sub di,10	;Back to correct place.
	mov es,labseg
	mov es:[di+1],al	;Put type.
	mov es:[di+2],bp
	movb es:[di+4],0	;NOT V (+ varbase).

	cmp al,labiv
	jne labni	;Label in Variables space?
	push es,di
	incw spoint	;Skip over the '?'.
	call decima1
	pop di,es
	jc adwerr	;Must have space.
	mov cx,ax
	jmps labin
	
labni	cmp al,variabl
	jne process
	mov cx,2
labin	mov bx,varadd
	mov es:[di+2],bx	;Put variable's address and update VARADD.
	movb es:[di+4],1	;Signed variable.
	test cx,1
	jz aligncd
	inc cx			;Align CX for variables.
aligncd addw varadd,cx
	mov al,13
	jc adwerr	;No more memory if Carry.

process ;SPOINT must be on next valid character after word.
	;ES:DI must point to start of word in table.
	incw es:[di+8]		;usage counter
	mov al,es:[di+1]	;Type of word.
	cmp al,label
	jne notlab
	mov lastpf,0		;what if 'label: return' - would fuck a call!
	ret			;Another statement after label? ':'

notlab	cmp al,variabl
	jne notvar
	jmp letcont

notvar	cmp al,udlab
	jne notund
	movb es:[di+1],label
	mov es:[di+2],bp	;Set new address.
	mov lastpf,0		;what if 'label: return' - would fuck a call!
	ret			;Another statement after label? ':'

notund	cmp al,array
	jne notaray
	sub al,al
	stc
	ret	;Error temporarily.

notaray cmp al,var32
	jne notv32
	mov ax,es:[di+2]
	mov assvar,ax
	mov assdi,di
	call skip2
	mov di,offset(oper32)
	jmp letnas

notv32	cmp al,functio
	jne notproc
	jmp fnuse	;Execute the code (if function ignore return).

notproc cmp al,labiv
	jne npisyn	;LABIV - return same as labels but no : to skip over.
	movb sign,0	;Unsigned.
	ret

npisyn	sub al,al
lper150	stc
sgeret	ret

gtypeal call skip2
	cmp al,':'
	mov al,label
	je sgeret
	cmpb [si],'?'
	mov al,labiv
	je sgeret
	mov al,variabl
	ret

lperrj	mov si,ecurnt
	cmp si,offset(ecurend)
	mov al,150
	jae lper150	;Too many lperrj?
	mov [si],bp	;Put code address in table.
	add si,2
	mov ecurnt,si
	movw [bp],72h	;JC $+0
	add bp,2
	ret

errstat	db 1		;Default #ERRORS ON
lperrs	mov dx,ecurnt
	lea si,ecurst
lperslp	cmp si,dx
	jae lperszz
	mov di,[si]
	add di,2
	add si,2	;Point to next error jump.
	mov ax,bp
	sub ax,di
	cmp ax,127
	ja lpeswn
	mov ss:[di-1],al
	jmps lperslp

lperszz movb [hflags+xhfrun],1	;Say using the ERROR library.
	movb [bp],0e8h
	inc bp
	mov al,onelibo
	mov cl,xehon
	cmp errstat,1
	je lpzcal
	mov cl,xehoff
lpzcal	jmp hfncsu     ;Find address of main procedure.

lpeswn	mov al,151
	stc
	ret

avarv	mov al,variabl
	mov ah,1	;Signed!
avar	push ax
	call skip2
	call findbin	;Does word exist?
	mov es,labseg
	mov addf1,si
	pop ax
	jc addwv
	cmpb es:[di+1],al
	je varet
	mov al,18	;Not a variable.
anfuv	stc
	ret
addwv	jz anfuv	;Valid word?

	push ax
	call addfb	;Try to add word to the binary sorted table.
	pop ax
	jc geret
	add di,10	;was 8, usage counter
	mov lablast,di
	sub di,10	;Back to correct place.

	mov es,labseg
	mov es:[di+1],al	;Put type.
	mov es:[di+4],ah	;Signed(NZ) or Unsigned(Z)?
	mov ax,varadd
	mov es:[di+2],ax	;Put variable's address and update VARADD.
	addw varadd,2

varet	mov ax,es:[di+2]
	mov wvar,ax
	incw es:[di+8]		;usage counter
	clc
geret	ret		;Return with WVAR set = variable address.

aunique call findbin	;Does word exist?
	mov es,labseg
	mov addf1,si
	jc addwl
aqnvu	mov al,18	;Not a variable.
aqfuv	stc
	ret
addwl	jz aqnvu
	call addfb	;Try to add word to the binary sorted table.
	jc geret
	add di,10	;was 8, usage counter
	mov lablast,di
	sub di,10	;Back to correct place.

	mov es,labseg
	movb es:[di+1],label	;Put type.
	mov es:[di+2],bp	;Put variable's address and update VARADD.
	movb es:[di+4],0	;Signed variable.
lpnull	clc		;LPNULL here because it fits criteria.
	ret

lsaven	mov ax,wnum
	mov [snum],ax
	clc
	ret

lloadn	mov ax,[snum]
	mov wnum,ax
	clc
	ret

snum	dw ?

lpnum2	mov ax,wnum2
	jmp lpute2

lpvar	mov ax,wvar
	jmp usv

loopnm8 call decima1
	mov wnum,al
	mov al,0
	jc aqfuv
	cmp ah,255
	jz loopnmo	;Negative but in range.
	cmp ah,al	;If positive but AH<>0 then error out of range.
	mov al,1
	jnz aqfuv	;If AH not 0 then error (must be greater than 255).
loopnmo clc
	ret		;OR resets carry?

lpnd	call decis1	;If undefined then set in ONESEG!
	jmps lpntwo

loopnum call decima1	;If undefined then error!
lpntwo	mov bx,wnum
	mov wnum2,bx	;Transferr old one.
	mov wnum,ax
	mov wnuml,dx	;DX:AX
	ret		;Return with the carry determined by decimal.

o1idle	pushf
	callf cs:[idleo]
	push ax,bx,cx,dx,bp,si,di,es,ds
	push cs
	pop ds
	db tf,tend
o2idle	pop ds,es,di,si,bp,dx,cx,bx,ax
o0idle	db tf,tend

offax2	push ax
	push bp
	lea di,o1idle
	call loopz
	lea di,o2idle
	jmps offax3

offax	push ax 	;Store code to put at end.
	push bp
	lea di,o0idle	;Do nothing at end!
offax3	push di
	call lpstat
	pop di
	pop bx
	mov ofxerr,al
	pop ax		;Restore code.
	pushf
	push ax
	call loopz
	pop ax
	mov [bp],al
	inc bp
	mov ax,bp
	sub ax,bx
	mov di,bx
	mov ss:-2[di],ax
	mov al,ofxerr
	popf
	ret
ofxerr	db 0

loopvar call getvar
	mov wvar,ax
	ret

looperr stc		;Set carry flag.
	ret		;AL = error number.

lutopia mov ax,bp
	sub ax,2
	mov utopia,ax	;Set windows limit address.
	ret

startb	mov si,spoint
	mov startbr,si
	ret
startr	mov si,startbr
	mov spoint,si	;Put spoint back to start because of failure.
	ret
startbr dw 0

loopin1 call startb
	movb dmust,1
	call decmal
	call lpntwo
	jmps lpnnc

sign	db 1		;Signed variables?
lfsign	mov si,ssdi
	mov ax,[si]	;Get next parameter of a LOOP function.
	addw ssdi,2

	cmpb sign,0
	jnz lsnput
	mov al,ah
lsnput	mov ah,1
	mov [bp],ax	;Jcc 1
	movb [bp+2],48h ;DEC AX
	add bp,3
	ret

lisign	mov si,ssdi
	mov ax,[si]	;Get next parameter of a LOOP function.
	addw ssdi,2

	cmpb sign,0
	jnz linput
	mov al,ah
linput	mov ah,3
	mov [bp],ax	;Jcc 3
	add bp,2
	ret

popl	dw ?
lpifnd	call getal
lpifud	mov si,oplace
	mov popl,si
	call startb
	sub ah,ah
	push bp
	add bp,ax	;Add offset for LPIFND.
	call lpnd	;Defined? Who cares?
	call lpnnc
	pop bp
	jc ifndc
	cmp al,99
	je ifndc
	clc
	ret
ifndc	mov si,popl	;If error or continuing then reset OPLACE.
	mov oplace,si
	ret

loopifn call startb
	call loopnum	;Must be defined!
lpnnc	jnc ifok
lifcont mov di,ssdi
lifcon2 inc di
	mov cx,1024
	cld
	mov ax,ds
	mov es,ax
	mov al,tf
	repne scasb
	jne fetab
	cmpb [di],tend
	jne lifcon2

	inc di
	mov ssdi,di
	mov al,99	;Got error, skipped to next command.
	jmp startr	;Reset SPOINT.

fetab	mov al,129
	jmp comperr

ifok	call endxp	;C if still an expression.
	jc lifcont
	sub al,al	;Not 99.
ifnc	clc		;This was a number only.
	ret

loopifv call startb
	call loopvar
	jnc ifok
	jmps lifcont

loopchk jc loopfin
	jmp loop2	;Continue with loop, no error.
loopfin ret

lpavar	mov ax,assvar
	jmp usv

lpequal call skip2
	cmp al,'='
	jne lpxcc
	incw spoint
	ret
lpxcc	mov al,19
	stc
	ret

hash	call skip2
	cmp al,'#'
	je hashok2
thash	mov al,22
	stc
hashok2 ret

lphand	call hash
	jc hashok2
	incw spoint
	call loopnum
	jc hincode
	call endxp
	jc hincode
	mov ax,wnum
	cmp ax,1
	jb thash
	cmp ax,12
	ja thash
thanpa0 dec ax		;0-11
	add ax,ax
	add ax,ax
	push ax
	movw [bp],1e8bh
	add bp,2

	mov cl,xhandle
	mov al,onelib
	call hfncsu	;Find address of main procedure.
	pop [bp-2]
	clc
	ret
hincode call restsp
	call hash
	incw spoint
	mov di,offset(hinc2)
	jmp loopz
hinc2	db tf,texp
	dec ax		;0-11
	add ax,ax
	add ax,ax
	db 0bfh,tf,hfuse,xhandle
	add di,ax
	mov bx,[di]
	db tf,tend

thsh2	jmp thash
lphget	call hash
	jc thsh2
	incw spoint
	call loopnum
	jc gincode
	call endxp
	jc gincode
	mov ax,wnum
	cmp ax,1
	jb thsh2
	cmp ax,12
	ja thsh2
lphpa0	dec ax		;0-11
	add ax,ax
	add ax,ax
	push ax
	movb [bp],0bfh
	inc bp

	mov cl,xhandle
	mov al,onelib
	call hfncsu	;Find address of main procedure.
	pop [bp-2]
	clc
	ret
gincode call restsp
	call hash
	incw spoint
	mov di,offset(ginc2)
	jmp loopz
ginc2	db tf,texp
	dec ax
	add ax,ax
	add ax,ax
	db 0bfh,tf,hfuse,xhandle
	add di,ax
	db tf,tend

ltcode	mov di,ssdi
	mov cl,[di]
	incw ssdi
ltcc0	mov ax,bp
ltcc1	mov di,repnest
	cmp di,offset(rnest)+150
	jb tcstore
	mov al,17
	stc
	ret
tcstore mov [di],ax	;Store repeat address.
	inc di
	inc di
	mov [di],cl
	inc di
	mov repnest,di
	clc		;Ensure that this was successful.
	ret
getclp	mov di,repnest
	cmp di,offset(rnest)
	jne getclp2
getcerr mov al,6
	stc
	ret
getclp2 dec di
	mov ch,[di]
	dec di
	dec di
	mov ax,[di]
	mov repnest,di
	cmp ch,cl
	jne getcerr
	clc
nosege	ret

segds	db ?
loopseg mov segds,0
	call loopexp
	jc nosege
	call nextq	;Another expression?
	jnc dssets
	lea di,lsegdx
	jmp loopz
dssets	movb [bp],1eh	;PUSH DS
	inc bp
	mov segds,1	;Set flag to say pushed DS.
	ret
lsegdx	push ax
	db tf,texp,tf,tend

loopds	cmpb segds,0
	jz lds1
	dec bp		;Drop PUSH DS.
	ret
lds1	movb [bp],1fh	;POP DS
	inc bp
	clc
	ret


;******* EXPRESSION HANDLERS

nextch	call skip2
	jnc nextret	;Any characters on remaining line.

	mov ax,cs
	mov es,ax
	cld
	mov cx,256
	mov al,13
	mov di,spoint
	repne scasb

	cmpb [di],10
	jne nhlnf
	inc di
nhlnf	mov buffp,di
	cmpb [di],26
	mov al,29	;Unexpected EOF.
	je nextstc

	call getln
	jmps nextch
nextstc stc
nextret ret

lpstat	call nextch	;One statement is until EOL or { xxx }.
	jc nextret	;Return with an error - EOL.
ch2	push ssdi
	call doline
	pop ssdi	;Restore previous LOOP pointer.

	jc chper	;Compile line, stop if error/end.
	call skip2
	jc chpok

	cmp al,':'   ;More statements?
	jne chpok
	incw spoint
	jmps ch2

chper	stc
	ret
chpok	clc
	ret

endxp	call skip2	;NC=end of expression, C=more and hence not end.
	jc chpok
	push si
	lea di,funcsj
	call findnc
	mov ah,1
	jnc ffen2	;If found then say NC (not end of expression).

	lea di,funcsa
ffen4	call fentry2
ffen3	mov ah,1

ffen2	mov cx,spoint
	pop bx
	mov spoint,bx
	cmc		;If found then not end of expression.
	ret

endx32	call skip2	;NC=end of expression, C=more and hence not end.
	jc chpok
	push si
	lea di,fsj32
	call findnc
	jnc ffen3	;If found then say NC (not end of expression).

	lea di,fsa32
	jmps ffen4


lput32	movb [bp],0a3h
	inc bp
	mov ax,assvar
	call usv
	movw [bp],1689h
	add bp,2
	inc ax
	inc ax
	jmp usv

lget32	movw [bp],1e8bh
	add bp,2
	mov ax,assvar
	call usv
	movw [bp],0e8bh
	add bp,2
lpasp	mov ax,assvar
	inc ax
	inc ax
	jmp usv


eword	call findbin
	mov addf1,si
	jc expey
	mov es,labseg
	incw es:[di+8]		;usage counter
	mov al,es:[di+1]
	mov bx,es:[di+2]
	mov si,spoint
	cmp al,variabl
	jne ewret
	cmpb es:[di+4],0
	jnz ewret

	movb sign,0	;Unsigned if an unsigned variable.

ewret	clc
	ret

loopexp call skip2
	jc expstc
	call fstexp
	jc expey

nextexp call skip2
	push spoint
	lea di,funcsj
	call findnc
	pop si
	jnc gotffe
	push si
	lea di,funcsa
	call fentry2
	pop si
	jc ngfe
gotffe	mov di,[di+1]
	call loopz
	jc expey
	jmps nextexp

ngfe	mov spoint,si
	clc
	ret	;End of expression ok.

expstc	mov al,30
expey	stc
	ret

optone	db ?		;Optimizing more than one exp?
onudef	db ?
oneexp	mov optone,0
	movb dmust,0
	inc bp
	call decmal
	dec bp
	jnc enput
	mov si,spoint
	mov al,[si]
notnumj	jmp notnum	;Get one expression.

fstexp	mov optone,1
	mov al,[si]
	call ifdigit
	jnc notnumj
	mov si,spoint
expnum	mov spoint,si
	push oplace
	inc bp
	call decis1
	dec bp
	pop bx
	jc expey
enput	mov cl,1
	cmp oplace,bx
	je enput2
	dec cl
enput2	mov onudef,cl	;0=Undefined, 1=Defined.
	or ax,ax
	jnz leavenz
	cmp cl,1	;Use short method for AX=0 if DEFINED as 0.
	jne leavenz
	movw [bp],0c029h	;SUB AX,AX
	add bp,2
	ret

leavenz push ax
	cmp optone,1
	jne leaveoz

	call skip2
	cmp al,'+'	;n + exp
	je lplus
	cmp al,'*'	;n * exp
	je lmult

leaveoz	movb [bp],0b8h	;n [operator exp]
	pop [bp+1]
	add bp,3
lfuck	ret

lplus	call pmdig
	jc leaveoz
	mov spoint,si
	push oplace
	push onudef
	call loopexp
	pop cx
	pop di,dx	;OPLACE & n.
	jc lfuck
	movb [bp],5	;ADD AX,n
	inc bp
	cmp cl,1	;Only update ONESEG if was undefined (CL=ONUDEF).
	je defe1
	mov es,oneseg
	mov es:[di-4],bp	;Rewrite address in UNDEF seg.
	jmps defdp
defe1	cmp dx,1	;1+exp?
	jne defdp
	movb [bp-1],40h	;INC AX
	clc
	ret
defdp	mov [bp],dx	;n+exp
	add bp,2
	ret

lmult	call pmdig
	jc leaveoz
	mov spoint,si
	push oplace
	push onudef
	call oneexp
	pop cx
	pop di,dx	;OPLACE & n.
	jc lfuck
	movb [bp],0bbh	;MOV BX,n
	inc bp
	cmp cl,1	;Only update ONESEG if was undefined.
	je defdm
	mov es,oneseg
	mov es:[di-4],bp	;Rewrite address in UNDEF seg.
defdm	mov [bp],dx
	movw [bp+2],0e3f7h	;MUL BX
	add bp,4
	ret

pmdig	inc si
	call skip
	jmp ifdigit	;C=digit.

expnumj	jmp expnum

exopen	cmp al,'('
	je exopeny

	cmp al,'"'      ;function "text" = address of text, puts text at bp too
	jne expnumj
	mov di,bp
	add di,5
	movb [bp],0b8h		;move ax,start_of_text
	mov [bp+1],di
	movb [bp+3],0ebh	;jmps ???
	add bp,5
	mov ah,120		;maximum string length

	;get text
	mov si,[spoint]
nextcos inc si
	mov al,[si]
	cmp al,'"'
	je got_eos
	cmp al,13
	je miseq		;end of line?
	mov [bp],al
	inc bp
	dec ah
	jnz nextcos
miseq	mov al,11		;missing end quote!
	stc
	ret

got_eos inc si
	mov [spoint],si
	movb [bp],0		;end of string marker = 0
	inc bp
	mov ax,bp
	sub ax,di
	mov ss:[di-1],al	;jump to skip_text
	clc
	ret

exopeny incw spoint
	jmp openb

notnum	call lowq
	jc exopen
	call getfunc
	jc gotffn
	jmp gotffe
gotffn	push spoint
	call eword
	pop si
	jc expnumj
expw2	cmp al,udlab	;Undefined label?
	je expnumj
	cmp al,label
	je expnumj	;Label?
	cmp al,labiv
	je expnumj
	ja expwa

	mov si,spoint
	cmpb [si],'['   ;Absolute addressing?
	je usesba

	cmp optone,1
	jne nvvar
	call skip2
	cmp al,'+'	;var + exp
	je vplus
	cmp al,'*'	;var * exp
	je vmult
nvvar	movb [bp],0a1h	;MOV AX,[var]
	inc bp
	mov ax,bx
	jmp usv

vplus	call pmdig
	jc nvvar
	mov spoint,si
	push bx		;Var address.
	call loopexp
	pop bx
	jc vamerr
	movw [bp],0603h	;ADD AX,[var]
	add bp,2
	mov ax,bx
	jmp usv

vamerr	ret

vmult	call pmdig
	jc nvvar
	mov spoint,si
	push bx		;Var address.
	call oneexp
	pop bx
	jc vamerr
	movw [bp],26f7h	;MULW [var]
	add bp,2
	mov ax,bx
	jmp usv

expwa	cmp al,10
	jne epscj
	jmp fnuse
epscj	jmp expstc

usesba	inc spoint	;Past the square bracket.
	push bx 	;Save ES variable address.
	call loopexp
	pop bx
	jc ussae
	movw [bp],068eh
	add bp,2
	mov ax,bx
	call usv
	call skip2
	cmp al,']'
	mov al,16
	jne ussae
	inc si
	mov spoint,si
	cmpb [si],'b'
	je absbyte
	cmpb [si],'B'
	je absbyte
	lea di,absw
lpabs	jmp loopz
absbyte inc spoint
	lea di,absb
	jmps lpabs
absw	mov di,ax
	mov ax,es:[di]
	db tf,tend
absb	mov di,ax
	mov al,es:[di]
	sub ah,ah
	db tf,tend
ussae	ret

ltchr	call skip2
	push si
	call getal
	pop si
	cmp [si],al
	jne ltchrf
	inc spoint
	ret		;Compared ok.
ltchrf	jmp nextend

lthex	call skip2
	sub dx,dx
	call dechex
	mov spoint,si
	jnc cdhex
	mov ax,dx
	or ah,ah
	jz cdhex
	mov al,1	;Out of range!
	stc
	ret
cdhex	jmp lpntwo	;Save AX as hex number.

fnuse	call skip2	;Must be FULLY RECURSIVE!
	cmp al,'('
	push di		;Labseg address.
	push assvar	;Store ASSVAR (used probably for functions).
	jne endfnu
	incw spoint

	mov di,es:2[di] ;Get variable details offset.

fnulp	call skip2
	jc endfnu
	cmp al,')'
	je endfnu1
	mov es,pfmseg
	cmpb es:[di],ect
	je endfnu	;Is it end of function details?

	push spoint
	push di

	mov cx,bigfind
	push ds
	cld
	mov ax,ds
	mov es,ax
	mov ds,pfmseg
	mov si,di
	lea di,pfsvar	;Copy variable to PFSVAR.
	mov cs:spoint,di
	rep movsb
	pop ds

	mov lprep,0
	call getvar	;Find the stored variable address.
	mov assvar,ax	;Equate for variable.
	mov al,24	;If not found then wrong number of parameters.
	jc fnovar

	call lpcomma	;Don't check if there or not.

	pop di
	mov si,spoint
	sub si,offset(pfsvar)
	add di,si
	pop spoint
	push di

	lea di,fequal
	call loopz	;Set the PASSer variable to expression.
	jc fnovar3

	pop di
	call lpcomma
	jmps fnulp

fnovar	pop bx
fnovar3	pop bx,bx,bx
	ret

endfnu1 incw spoint
endfnu	pop assvar
	pop di
	mov es,labseg
	mov dx,es:4[di]
	jmp cgcall	;The actual code is call function_address.

pfsvar	ds bigfind+1	;Store PFM variable details.

lopword call nextch
	jc lwe
	call rlook
	jc lwe		;Default syntax error.
	mov di,ssdi
	incw ssdi
	cmp ah,[di]	;Does the found word match.
	mov al,21
	jne lwe
	ret
lwe	stc
	ret

name	call skip2
	cmp al,'"'
	je nameq
	call loopexp
	jnc nameq1
	ret
nameq1	movw [bp],0c289h	;MOV DX,AX.
	add bp,2
	ret
nameq	push si
	movb [bp],0bah	;MOV DX,address of name.
	inc bp
	mov ax,bp
	add ax,4
	mov [bp],ax
	movb [bp+2],0ebh	;JMPS nn
	add bp,3
	pop si
	push bp
	inc bp
	inc si
namlp	mov al,[si]
	cmp al,'"'
	je namfin
	cmp al,13
	je namend
	mov [bp],al
	inc bp
	inc si
	jmps namlp
namfin	inc si
	mov spoint,si
	movb [bp],0
	inc bp
	pop di
	mov ax,bp
	sub ax,di
	dec al
	mov ss:[di],al
nomdec	clc
	ret

namend	pop ax
	mov al,22
	stc
	ret

decis1	mov dmust,0
	jmps decmuc1

decima1 mov dmust,1

decmuc1 mov defd,1	;Default to defined.

decimal call decmal
	jc decinot	;Must be at least one number.
decima2 call decjoin	;Get a join of some sort.
	jc nomdec	;If not valid then ignore, leave as is.
	jmps decima2
decjoin push ax
	call skip2
	cmp al,'+'
	je adddeci
	cmp al,'-'
	je subdeci
	cmp al,'*'
	je muldeci
	cmp al,'/'
	je divdeci
	pop ax		;Return with the number.
	stc
decinot ret

adddeci push spoint
	incw spoint
	call decimal
	jc stopdec
	call asfrend
	jc stopdec
	pop bx
	pop bx
	add ax,bx
	clc
	ret

subdeci push spoint
	incw spoint
	mov bl,omust
	push bx
	mov omust,onesub
	call decmal
	pop bx
	mov omust,bl
	jc stopdec
	call asfrend
	jc stopdec
	pop bx
	pop bx
	sub bx,ax
	mov ax,bx
	clc
	ret

muldeci push spoint
	incw spoint
	movb dmust,1	;Must be defined if 'x * undef label'
	call decmal
	jc stopdec
	pop bx	;Drop spoint.
	pop bx	;Get last value.
	mul bx
	clc
	ret

stopdec pop spoint	;Reset to where we came from.
	pop ax
	stc
	ret

divdeci push spoint
	incw spoint
	movb dmust,1	;Must be defined if 'x / undef label'
	call decmal
	jc stopdec
	pop bx	;Drop spoint.
	pop bx	;Get last value.
	xchg ax,bx
	sub dx,dx
	cmp bx,dx
	jz divzero
	div bx
	clc
	ret
divzero mov al,15	;Divide by 0.
	stc
	ret

asfrend push ax
	call skip2
	jc asfok	;EOL means EO(expression)
	cmp al,'+'
	je asfok
	cmp al,'-'
	je asfok
	call endxp	;If end of expression then return NC.
	jnc asfok
	pop ax
deshit	stc
	ret
asfok	pop ax
	clc
	ret


decmal	sub dx,dx
	mov si,spoint
	call skip
	mov al,30
	jc deshit
	mov al,-1[si]
	call sept
	jnc decmin
	cmpb [si],'-'
	jne decmin
	inc si
	mov spoint,si
	call decimal
	jc notmin
	neg ax		;Get direct numerical expression and negate it.
notminn	clc
notmin	ret
decmin	call skip3
	mov stdec,si
	cmp al,'.'
	jne decdot
	incw spoint	;Get variables address.
	push spoint
	call eword
	mov cx,spoint
	pop spoint
	jc notdot	;Stuffed variable.
	cmp al,variabl
	je usedotv
	cmp al,var32
	je usedotv
	cmp al,functio
	jne notdot
	mov spoint,cx
	mov ax,es:[di+4]	;functions address
	ret

usedotv	mov spoint,cx
	mov ax,bx
	cmp dmust,1
	je notdot

	call usv	;Use variables value.
	sub bp,2	;Restore BP.
	ret

notdot	mov al,26	;Expected a variable.
	stc
	ret

decdot	call ifdigit	;Is AL a digit? NC = not.
	jnc dnloop
	jmp decloop
dnloop	cmp al,39	;'''
	jne declab
	jmp decasc
decsyn	sub al,al
expc	stc
dclret	ret
declab	call lowq
	mov al,0
	jc dclret
	movb omust,oneadd
	jmp getlab

resvet	push spoint
	jmps resrl

resve	push spoint
	call getfunc
	jnc resnc
resrl	call rlook
resnc	pop spoint
	ret

getvar	push spoint
	call eword
	pop dx
	jc expcv
	cmp al,variabl
	jne expcv
	mov si,spoint
	cmpb [si],'['   ;Must not be absolute array.
	je gveabs
	mov ax,bx
	clc
	ret
gveabs	mov spoint,dx
	mov al,7
	stc
	ret
expcv	mov al,26
glfc	stc
wdlret	ret

getlab	call resve
	mov al,31
	jnc glfc

	push spoint
	call eword
	pop dx
	jnc glchk
	mov al,0
	jz glfc

	cmp dmust,1	;Must it be defined?
	je glfc

	call addfb
	jc wdlret
	add di,10	;was 8, usage
	mov lablast,di
	sub di,10	;Back to correct place for EQU etc.

	call putone
	
	mov es,labseg
	movb es:[di+1],udlab
	movw es:[di+2],0
	incw es:[di+8]		;usage
	sub ax,ax		;Assume offset of zero.

wordr	clc
	ret

glchk	cmp al,label	;A label?
	je glchl
	cmp al,labiv
	je gclv
	cmp al,udlab	;Undefined label?
	jne glchf
	cmp dmust,1	;Must it be defined?
	je glfc

	call putone
	sub bx,bx

glchl	mov ax,bx	;Return value.
	clc
	ret
glchf	mov al,128
	mov spoint,dx
glrc	stc
	ret

gclv	cmp dmust,1	;Must it be defined?
	je glrc
	call putone
	sub ax,ax
	ret

putone	mov es,oneseg
	mov si,oplace
	mov defd,0	;Undefined!

	cmp si,onesize
	jb pistop
e131	mov al,131	;Abort - no bloody memory left.
	jmp comperr

pistop	mov al,omust
	mov es:[si],al
	mov es:[si+1],bp
	mov es:[si+3],di
	add oplace,5
	ret

tabuc	db label

decasc	inc si
	sub ah,ah
	mov al,[si]
	inc si
	inc si
	mov spoint,si
	cmpb -1[si],39
	je absret
	sub al,al
	stc
absret	ret

hfjump	mov al,0e9h
	jmps hfcjom
hfcall	mov al,0e8h
hfcjom	mov [bp],al
	inc bp
	mov al,onelibo
	jmps hfncsu1

hfuset	mov al,onelib
hfncsu1 mov di,ssdi
	mov cl,[di]
	incw ssdi

hfncsu	mov di,oplace
	cmp di,onesize
	jb chkclc
	jmp e131	;Out of space for ONESEG table.
chkclc	mov es,oneseg
	mov bl,al
	cmp bl,onelibo
	jne hfal
	mov bl,onelib
hfal	movb es:[di],bl
	mov  es:[di+1],bp
	movb es:[di+3],cl
	addw oplace,5

	sub dx,dx
	cmp al,onelibo
	jne hfzero

	mov ax,bp
	add ax,2
	sub dx,ax

hfzero	movw [bp],dx
	add bp,2	;Allow space for use of label.
	push di
	call hfuscn
	pop di
	ret

hfuscn	lea di,hftable
	sub ch,ch
	push cx
	add di,cx
	mov cl,[di]	;Get actual routine start.

	lea di,hflags
	add di,cx	;Find its flag position.
	cmpb [di],0	;Only set if undefined at present, else has been used.
	jne hfgetit
	movb [di],1	;Set to 1 (use).
hfgetit lea di,hfadd
	pop cx
	add cl,cl
	add di,cx	;DI=HFADD+2*CL
	mov ax,[di]	;Address.
	mov dx,ax	;Address.	;**********************
	clc
	ret

hfsett	mov di,ssdi
	mov cl,[di]
	incw ssdi
hfsecn	lea di,hfadd
	sub ch,ch
	add cl,cl
	add di,cx
	mov [di],bp	;Put current CODEADD.
	clc
	ret

;==============================================================================

ltitle	cmpb output,0
	jz ltitle1
	lea dx,titlex
	call onm

ltitle1 mov di,[ssdi]
	mov al,[di]
	incw ssdi
	or al,al
	jz ltite1

	cmpb output,0
	jz ltitle1

	call ona
	jmps ltitle1

ltite1	cmpb output,0
	jz ltite2

	call onnl

ltite2	clc
	ret

;==============================================================================

;******* DATA

hfast	dw hfmain,hfrun,ikeyint,hfint
	dw handles,getrnd,advance,?
	dw hfloc,wlocate,print,sprint
	dw cldir,pmeslp,spmess,bpmess
	dw lpmess,printb,sprintb,lprintb
	dw bpb,printh,sprinth,bprinth
	dw lprinth,testscl,sptest,cpos
	dw sprite,rthit,takey,inpax
	dw hinpax,getiii,wdata,tptm
	dw wclose,wopen,ctrace,hmove
	dw hline,cdebug,hmul32,hdiv32
	dw hplot,hpoint,hpadd,ermsg
	dw exit,seek,ehon,ehoff
	dw herc,hfind,hsinit,hprotek
	dw hmul64,hdiv64,hprn64,hqsort
	dw h386,hvga_line


irange	db 0		;#long status for IF THEN ELSE.

hflags	ds libs 	;Space for all main routine flags.
hfadd	ds librts*2	;Space for addresses of each subroutine.
hfmitab dw 0
hfbreak dw 0

pfmoff	dw 0
pfmseg	dw 0	;Segment for PROCEDURE/FUNCTION/MACRO data.
endpfm	dw 1200 ;Size of pfmseg.

varlen	dw 0
varbase dw 0

wnum	dw 0	;Compiler stores variables here.
wnum2	dw 0	;Store of second parameteer (first).
wnuml	dw 0	;long part of number DX:ax

wvar	dw 0
wabsoff dw 0
wabsseg dw 0
absall	db 0
varadd	dw 0
specf	db 0	;Compiling specials?

t_file	dw 0
t_files db 0
t_start db 0
t_point dw t_base
ftseg	dw ?
doption db 0
t_name	ds 64

entry	dw 100h+speclen

introm	db 13,10,'!FAST COMPILER v2.92 (09/06/97)$'
entern	db 'Source filename [.F] $'
memerr	db 'FAST needs 200K free memory!',13,10,'$'
libline db 'LIBRARY ANALYSIS <<<',13
titlex	db 'Library : $'

;******* FAST COMPILER TABLES

cfast	db 1,'if'
	dw offset(ccpif)
	db 4,'include',13
	dw offset(cincl)
	db 3,'endif',13
	dw offset(cifend)
	db 3,'inpend'
	dw offset(cinpend)
	db 3,'errors'
	dw offset(cerrors)
	db 3,'window'
	dw offset(cwindow)
	db 3,'stack',13
	dw offset(chstack)
	db 3,'debug',13
	dw offset(cfdebug)
	db 3,'trace',13
	dw offset(cftrace)
	db 2,'para'
	dw offset(cpara)
	db 3,'entry',13
	dw offset(centry)
	db 2,'long'
	dw offset(clong)
	db 3,'short',13
	dw offset(cshort)
	db 3,'setdos'
	dw offset(csetdos)
	db 4,'protect',13
	dw offset(cprotek)
	db 2,'else'
	dw offset(cdelse)
	db 3,'local',13
	dw offset(clocal)
	db 2,'open'
	dw offset(copenm)
	db 255

hsh000	db 3,'write',13
	dw offset(cwrite)
	db 255

hsh002	db 2,'reg',13
	dw offset(creg)
	db 255

hsh003	db 2,'ink',13
	dw offset(cink)
	db 2,'null'
	dw offset(cnull)
	db 255

hsh006	db 2,'for',13
	dw offset(cfor)
	db 255

hsh008	db 5,'deallocate'
	dw offset(cdeall)
	db 255

hsh014	db 3,'while',13
	dw offset(cwhile)
	db 3,'modify'
	dw offset(cmodify)
	db 255

hsh015	db 3,'inline'
	dw offset(cdatab)
	db 2,'dos',13
	dw offset(cdos)
	db 255

hsh016	db 2,'plot'
	dw offset(cplot)
	db 255

hsh018	db 3,'remove'
	dw offset(cremove)
	db 255

hsh024	db 3,'endfor'
	dw offset(cendfor)
	db 255

hsh028	db 2,'int',13
	dw offset(cint)
	db 3,'fname',13
	dw offset(cfname)
	db 255

hsh034	db 3,'const',13
	dw offset(cconst)
	db 3,'print',13
	dw offset(cp)
	db 255

hsh036	db 3,'inputs'
	dw offset(cinputs)
	db 3,'clocks'
	dw offset(cclocks)
	db 255

hsh038	db 4,'endcode',13
	dw offset(cendc)
	db 3,'printb'
	dw offset(cpb)
	db 255

hsh044	db 3,'printh'
	dw offset(cph)
	db 255

hsh045	db 4,'disable',13
	dw offset(cdisbrk)
	db 255

hsh047	db 3,'mul64',13
	dw offset(cmul64)
	db 255

hsh051	db 2,'free'
	dw offset(cfree)
	db 255

hsh052	db 2,'page'
	dw offset(cpage)
	db 255

hsh055	db 4,'lprinth',13
	dw offset(lcph)
	db 3,'prints'
	dw offset(cprints)
	db 255

hsh056	db 2,'swap'
	dw offset(cswap)
	db 255

hsh057	db 3,'error',13
	dw offset(cerror)
	db 255

hsh059	db 2,'line'
	dw offset(cline)
	db 2,'make'
	dw offset(cmake)
	db 255

hsh061	db 3,'create'
	dw offset(ccreate)
	db 2,'jump'
	dw offset(cjump)
	db 4,'lprintb',13
	dw offset(lcpb)
	db 255

hsh063	db 2,'find'
	dw offset(cfind)
	db 255

hsh068	db 2,'retf'
	dw offset(cretf)
	db 255

hsh071	db 3,'repeat'
	dw offset(crepeat)
	db 255

hsh072	db 2,'halt'
	dw offset(chalt)
	db 255

hsh074	db 3,'popall'
	dw offset(cpopal)
	db 3,'var32',13
	dw offset(cvar32)
	db 255

hsh075	db 1,'if'
	dw offset(cif)
	db 255

hsh080	db 3,'sppos',13
	dw offset(csppos)
	db 255

hsh082	db 3,'htext',13
	dw offset(chtext)
	db 255

hsh088	db 2,'data'
	dw offset(cdata)
	db 2,'dimb'
	dw offset(cdimb)
	db 255

hsh092	db 2,'pop',13
	dw offset(cpop)
	db 255

hsh100	db 5,'procedure',13
	dw offset(cfunct)
	db 255

hsh102	db 4,'vga_line'
	dw offset(cvga_line)
	db 255

hsh103	db 3,'colour'
	dw offset(ccolour)
	db 2,'out',13
	dw offset(cout)
	db 2,'wait'
	dw offset(cwait)
	db 255

hsh105	db 3,'datab',13
	dw offset(cdatab)
	db 2,'let',13
	dw offset(clet)
	db 255

hsh109	db 4,'curtoloc'
	dw offset(cctl)
	db 255

hsh112	db 4,'execute',13
	dw offset(cexec)
	db 3,'reclen'
	dw offset(creclen)
	db 255

hsh115	db 2,'cls',13
	dw offset(ccls)
	db 255

hsh118	db 3,'moveb',13
	dw offset(cmoveb)
	db 255

hsh119	db 2,'fill'
	dw offset(cfill)
	db 3,'shell',13
	dw offset(cshell)
	db 255

hsh120	db 2,'beep'
	dw offset(cbeep)
	db 255

hsh121	db 2,'if32'
	dw offset(cif32)
	db 255

hsh122	db 4,'print64',13
	dw offset(cprn64)
	db 255

hsh124	db 4,'sprinthb'
	dw offset(scphb)
	db 255

hsh125	db 3,'return'
	dw offset(creturn)
	db 255

hsh127	db 2,'proc'
	dw offset(cfunct)
	db 255

hsh128	db 3,'reset',13
	dw offset(creset)
	db 255

hsh130	db 4,'unsigned'
	dw offset(cunsign)
	db 255

hsh131	db 1,'on'
	dw offset(con)
	db 3,'space',13
	dw offset(cds)
	db 255

hsh132	db 4,'lprinthb'
	dw offset(lcphb)
	db 5,'hgraphics',13
	dw offset(chgraph)
	db 255

hsh135	db 2,'var',13
	dw offset(cvar)
	db 255

hsh136	db 3,'handle'
	dw offset(chandle)
	db 255

hsh140	db 3,'fillb',13
	dw offset(cfillb)
	db 255

hsh141	db 2,'poke'
	dw offset(cpoke)
	db 255

hsh143	db 3,'stack',13
	dw offset(cstack)
	db 255

hsh144	db 4,'palette',13
	dw offset(cpalett)
	db 255

hsh145	db 4,'pushall',13
	dw offset(cpushal)
	db 255

hsh150	db 3,'sprite'
	dw offset(csprite)
	db 3,'setint'
	dw offset(csvect)
	db 255

hsh151	db 2,'save'
	dw offset(csave)
	db 3,'string'
	dw offset(cstring)
	db 255

hsh152	db 4,'function'
	dw offset(cfunct)
	db 3,'timer',13
	dw offset(ctimer)
	db 255

hsh160	db 2,'move'
	dw offset(cmove)
	db 255

hsh165	db 2,'iret'
	dw offset(ciret)
	db 255

hsh168	db 3,'paper',13
	dw offset(cpaper)
	db 255

hsh175	db 3,'color',13
	dw offset(ccolour)
	db 255

hsh179	db 3,'bright'
	dw offset(cbright)
	db 255

hsh180	db 2,'open'
	dw offset(copen)
	db 255

hsh181	db 3,'scroll'
	dw offset(cscroll)
	db 2,'test'
	dw offset(ctest)
	db 255

hsh182	db 3,'asciiz'
	dw offset(casciiz)
	db 3,'close',13
	dw offset(cclose)
	db 255

hsh190	db 3,'screen'
	dw offset(cscreen)
	db 255

hsh192	db 3,'change'
	dw offset(cchange)
	db 3,'gosub',13
	dw offset(cgosub)
	db 255

hsh195	db 2,'load'
	dw offset(cload)
	db 255

hsh196	db 3,'sprint'
	dw offset(scp)
	db 255

hsh200	db 3,'rename'
	dw offset(crename)
	db 255

hsh205	db 3,'break',13
	dw offset(cbrk)
	db 4,'restore',13
	dw offset(crestor)
	db 255

hsh206	db 2,'dir',13
	dw offset(cdir)
	db 255

hsh209	db 2,'dim',13
	dw offset(cdim)
	db 255

hsh211	db 3,'flash',13
	dw offset(cflash)
	db 3,'pokeb',13
	dw offset(cpokeb)
	db 255

hsh213	db 2,'call'
	dw offset(ccall)
	db 4,'forever',13
	dw offset(cforev)
	db 5,'quick_sort'
	dw offset(cqsort)
	db 255

hsh214	db 3,'delete'
	dw offset(cdelete)
	db 5,'terminate',13
	dw offset(cterm)
	db 255

hsh217	db 2,'next'
	dw offset(cnext)
	db 3,'div64',13
	dw offset(cdiv64)
	db 255

hsh220	db 4,'const32',13
	dw offset(ccon32)
	db 255

hsh223	db 4,'loctocur'
	dw offset(cltc)
	db 2,'psp',13
	dw offset(cpsp)
	db 255

hsh226	db 2,'mode'
	dw offset(cmode)
	db 255

hsh228	db 2,'read'
	dw offset(cread)
	db 255

hsh229	db 5,'randomize',13
	dw offset(crandm)
	db 255

hsh231	db 4,'printhb',13
	dw offset(cphb)
	db 255

hsh235	db 2,'push'
	dw offset(cpush)
	db 2,'seek'
	dw offset(cseek)
	db 255

hsh237	db 3,'shape',13
	dw offset(cshape)
	db 255

hsh239	db 2,'goto'
	dw offset(cgoto)
	db 255

hsh240	db 4,'sprinth',13
	dw offset(scph)
	db 255

hsh241	db 3,'locate'
	dw offset(clocate)
	db 255

hsh245	db 2,'stop'
	dw offset(cstop)
	db 255

hsh247	db 3,'curpos'
	dw offset(ccurpos)
	db 255

hsh250	db 3,'cursor'
	dw offset(ccursor)
	db 3,'lprint'
	dw offset(lcp)
	db 4,'sprintb',13
	dw offset(scpb)
	db 255

hsh252	db 3,'sound',13
	dw offset(csound)
	db 255

hsh253	db 3,'enable'
	dw offset(cenable)
	db 255

hsh254	db 3,'readb',13
	dw offset(creadb)
	db 255

hsh255	db 3,'noise',13
	dw offset(cnoise)
	db 255,255

tabtab	dw hsh000,tabnul,hsh002,hsh003,tabnul,tabnul,hsh006,tabnul
	dw hsh008,tabnul,tabnul,tabnul,tabnul,tabnul,hsh014,hsh015
	dw hsh016,tabnul,hsh018,tabnul,tabnul,tabnul,tabnul,tabnul
	dw hsh024,tabnul,tabnul,tabnul,hsh028,tabnul,tabnul,tabnul
	dw tabnul,tabnul,hsh034,tabnul,hsh036,tabnul,hsh038,tabnul
	dw tabnul,tabnul,tabnul,tabnul,hsh044,hsh045,tabnul,hsh047
	dw tabnul,tabnul,tabnul,hsh051,hsh052,tabnul,tabnul,hsh055
	dw hsh056,hsh057,tabnul,hsh059,tabnul,hsh061,tabnul,hsh063
	dw tabnul,tabnul,tabnul,tabnul,hsh068,tabnul,tabnul,hsh071
	dw hsh072,tabnul,hsh074,hsh075,tabnul,tabnul,tabnul,tabnul
	dw hsh080,tabnul,hsh082,tabnul,tabnul,tabnul,tabnul,tabnul
	dw hsh088,tabnul,tabnul,tabnul,hsh092,tabnul,tabnul,tabnul
	dw tabnul,tabnul,tabnul,tabnul,hsh100,tabnul,hsh102,hsh103
	dw tabnul,hsh105,tabnul,tabnul,tabnul,hsh109,tabnul,tabnul
	dw hsh112,tabnul,tabnul,hsh115,tabnul,tabnul,hsh118,hsh119
	dw hsh120,hsh121,hsh122,tabnul,hsh124,hsh125,tabnul,hsh127
	dw hsh128,tabnul,hsh130,hsh131,hsh132,tabnul,tabnul,hsh135
	dw hsh136,tabnul,tabnul,tabnul,hsh140,hsh141,tabnul,hsh143
	dw hsh144,hsh145,tabnul,tabnul,tabnul,tabnul,hsh150,hsh151
	dw hsh152,tabnul,tabnul,tabnul,tabnul,tabnul,tabnul,tabnul
	dw hsh160,tabnul,tabnul,tabnul,tabnul,hsh165,tabnul,tabnul
	dw hsh168,tabnul,tabnul,tabnul,tabnul,tabnul,tabnul,hsh175
	dw tabnul,tabnul,tabnul,hsh179,hsh180,hsh181,hsh182,tabnul
	dw tabnul,tabnul,tabnul,tabnul,tabnul,tabnul,hsh190,tabnul
	dw hsh192,tabnul,tabnul,hsh195,hsh196,tabnul,tabnul,tabnul
	dw hsh200,tabnul,tabnul,tabnul,tabnul,hsh205,hsh206,tabnul
	dw tabnul,hsh209,tabnul,hsh211,tabnul,hsh213,hsh214,tabnul
	dw tabnul,hsh217,tabnul,tabnul,hsh220,tabnul,tabnul,hsh223
	dw tabnul,tabnul,hsh226,tabnul,hsh228,hsh229,tabnul,hsh231
	dw tabnul,tabnul,tabnul,hsh235,tabnul,hsh237,tabnul,hsh239
	dw hsh240,hsh241,tabnul,tabnul,tabnul,hsh245,tabnul,hsh247
	dw tabnul,tabnul,hsh250,tabnul,hsh252,hsh253,hsh254,hsh255


;Functions and operators valid for expressions and variable assignment.

funcsj	db 1,'-'
	dw offset(fsub)
	db 1,'='              ;Put twice and first so it is compiled faster.
	dw offset(fce)
	db 1,'+'
	dw offset(fadd)
	db 1,'*'
	dw offset(fmult)
	db 1,'/'
	dw offset(fdiv)
	db 2,'<>'
	dw offset(fcne)
	db 2,'>='
	dw offset(fcge)
	db 2,'<='
	dw offset(fcle)
	db 1,'>'
	dw offset(fcg)
	db 1,'<'
	dw offset(fcl)
	db 5,'above'
	dw offset(ifabove)
	db 5,'below'
	dw offset(ifbelow)
	db 255

fj1	db '-=+*/<>ab@[ctlf",'

fsj32	db 1,'-'
	dw offset(fsub32)
	db 1,'='              ;Put twice and first so it is compiled faster.
	dw offset(fce32)
	db 1,'+'
	dw offset(fadd32)
	db 1,'*'
	dw offset(fmult32)
	db 1,'/'
	dw offset(fdiv32)
	db 2,'<>'
	dw offset(fcne32)
	db 2,'>='
	dw offset(fcge32)
	db 2,'<='
	dw offset(fcle32)
	db 1,'>'
	dw offset(fcg32)
	db 1,'<'
	dw offset(fcl32)
	db 5,'above'
	dw offset(fcg32)
	db 5,'below'
	dw offset(fcl32)
	db 255

getfunc lea di,ftabtab
	call skip2
	jmp findf
gotfunc ret

gfunc32 lea di,ftab32
	jmp fentry2

fh002	db 2,'reg',13
	dw offset(freg)
	db 255

fh008	db 4,'rrightz',13
	dw offset(frightz)
	db 255

fh011	db 3,'select'
	dw offset(fselect)
	db 255

fh021	db 2,'high'
	dw offset(fhigh)
	db 255

fh027	db 3,'scrchr'
	dw offset(fscreen)
	db 255

fh030	db 2,'eof',13
	dw offset(feof)
	db 255

fh032	db 2,'not',13
	dw offset(fnot)
	db 255

fh041	db 3,'printm'
	dw offset(fprintm)
	db 255

fh043	db 2,'low',13
	dw offset(flow)
	db 255

fh047	db 3,'peekb',13
	dw offset(fpeekb)
	db 255

fh052	db 2,'page'
	dw offset(fpage)
	db 255

fh053	db 3,'inputb'
	dw offset(fib)
	db 255

fh060	db 3,'rleft',13
	dw offset(fleft)
	db 255

fh063	db 3,'inputh'
	dw offset(fih)
	db 255

fh067	db 1,'in'
	dw offset(fin)
	db 255

fh070	db 4,'compare',13
	dw offset(ccomp)
	db 255

fh085	db 4,'findbin',13
	dw offset(ffindb)
	db 255

fh089	db 6,'processor32',13
	dw offset(fproc32)
	db 255

fh095	db 4,'bcd2dec',13
	dw offset(fbcd2)
	db 255

fh096	db 2,'rnd',13
	dw offset(frnd)
	db 255

fh104	db 3,'ucase',13
	dw offset(fupper)
	db 255

fh106	db 2,'peek'
	dw offset(fpeek)
	db 255

fh118	db 2,'scan'
	dw offset(fscan)
	db 255

fh120	db 2,'key',13
	dw offset(fkey)
	db 3,'carry',13
	dw offset(fcarry)
	db 255

fh126	db 2,'dta',13
	dw offset(fdta)
	db 2,'idiv'
	dw offset(fidiv)
	db 255

fh133	db 4,'inputhb',13
	dw offset(fihb)
	db 255

fh136	db 3,'handle'
	dw offset(fhandle)
	db 255

fh143	db 3,'stack',13
	dw offset(fstack)
	db 255

fh147	db 3,'rright'
	dw offset(fright)
	db 255

fh152	db 3,'timer',13
	dw offset(ftimer)
	db 255

fh155	db 2,'menu'
	dw offset(cow)
	db 255

fh164	db 3,'cdisk',13
	dw offset(fcdisk)
	db 255

fh166	db 3,'indoss'
	dw offset(findoss)
	db 255

fh168	db 6,'serial_send',13
	dw offset(fssend)
	db 255

fh169	db 7,'serial_status',13
	dw offset(fsstat)
	db 255

fh170	db 4,'compareb'
	dw offset(ccompb)
	db 255

fh172	db 3,'point',13
	dw offset(fpoint)
	db 255

fh181	db 3,'digits'
	dw offset(fdigits)
	db 255

fh186	db 2,'abs',13
	dw offset(fabs)
	db 3,'input',13
	dw offset(fi)
	db 3,'indoso'
	dw offset(findoso)
	db 255

fh190	db 3,'screen'
	dw offset(fmode)
	db 255

fh195	db 2,'imul'
	dw offset(fimul)
	db 255

fh201	db 2,'usr',13
	dw offset(ccall)
	db 255

fh205	db 3,'search'
	dw offset(fsearch)
	db 255

fh209	db 5,'keypressed'
	dw offset(fkped)
	db 255

fh219	db 4,'searchb',13
	dw offset(fsearcb)
	db 255

fh223	db 2,'psp',13
	dw offset(fpsp)
	db 255

fh225	db 4,'allocate'
	dw offset(falloc)
	db 255

fh228	db 2,'read'
	dw offset(fread)
	db 255

fh241	db 4,'keyscan',13
	dw offset(fks)
	db 255

fh247	db 3,'curpos'
	dw offset(fcurpos)
	db 255

fh248	db 2,'hit',13
	dw offset(fhit)
	db 255

fh249	db 3,'lcase',13
	dw offset(flower)
	db 255

fh253	db 3,'rleftz'
	dw offset(fleftz)
	db 255,255

ftabtab dw tabnul,tabnul,fh002,tabnul,tabnul,tabnul,tabnul,tabnul
	dw fh008,tabnul,tabnul,fh011,tabnul,tabnul,tabnul,tabnul
	dw tabnul,tabnul,tabnul,tabnul,tabnul,fh021,tabnul,tabnul
	dw tabnul,tabnul,tabnul,fh027,tabnul,tabnul,fh030,tabnul
	dw fh032,tabnul,tabnul,tabnul,tabnul,tabnul,tabnul,tabnul
	dw tabnul,fh041,tabnul,fh043,tabnul,tabnul,tabnul,fh047
	dw tabnul,tabnul,tabnul,tabnul,fh052,fh053,tabnul,tabnul
	dw tabnul,tabnul,tabnul,tabnul,fh060,tabnul,tabnul,fh063
	dw tabnul,tabnul,tabnul,fh067,tabnul,tabnul,fh070,tabnul
	dw tabnul,tabnul,tabnul,tabnul,tabnul,tabnul,tabnul,tabnul
	dw tabnul,tabnul,tabnul,tabnul,tabnul,fh085,tabnul,tabnul
	dw tabnul,fh089,tabnul,tabnul,tabnul,tabnul,tabnul,fh095
	dw fh096,tabnul,tabnul,tabnul,tabnul,tabnul,tabnul,tabnul
	dw fh104,tabnul,fh106,tabnul,tabnul,tabnul,tabnul,tabnul
	dw tabnul,tabnul,tabnul,tabnul,tabnul,tabnul,fh118,tabnul
	dw fh120,tabnul,tabnul,tabnul,tabnul,tabnul,fh126,tabnul
	dw tabnul,tabnul,tabnul,tabnul,tabnul,fh133,tabnul,tabnul
	dw fh136,tabnul,tabnul,tabnul,tabnul,tabnul,tabnul,fh143
	dw tabnul,tabnul,tabnul,fh147,tabnul,tabnul,tabnul,tabnul
	dw fh152,tabnul,tabnul,fh155,tabnul,tabnul,tabnul,tabnul
	dw tabnul,tabnul,tabnul,tabnul,fh164,tabnul,fh166,tabnul
	dw fh168,fh169,fh170,tabnul,fh172,tabnul,tabnul,tabnul
	dw tabnul,tabnul,tabnul,tabnul,tabnul,fh181,tabnul,tabnul
	dw tabnul,tabnul,fh186,tabnul,tabnul,tabnul,fh190,tabnul
	dw tabnul,tabnul,tabnul,fh195,tabnul,tabnul,tabnul,tabnul
	dw tabnul,fh201,tabnul,tabnul,tabnul,fh205,tabnul,tabnul
	dw tabnul,fh209,tabnul,tabnul,tabnul,tabnul,tabnul,tabnul
	dw tabnul,tabnul,tabnul,fh219,tabnul,tabnul,tabnul,fh223
	dw tabnul,fh225,tabnul,tabnul,fh228,tabnul,tabnul,tabnul
	dw tabnul,tabnul,tabnul,tabnul,tabnul,tabnul,tabnul,tabnul
	dw tabnul,fh241,tabnul,tabnul,tabnul,tabnul,tabnul,fh247
	dw fh248,fh249,tabnul,tabnul,tabnul,fh253,tabnul,tabnul


funcsa	db 2,'and',13
	dw offset(fand)
	db 1,'or'
	dw offset(for)
	db 2,'xor',13
	dw offset(fxor)
	db 2,'mod',13
	dw offset(fmod)
	db 255

fsa32	db 2,'and',13
	dw offset(and32)
	db 1,'or'
	dw offset(or32)
	db 2,'xor',13
	dw offset(xor32)
	db 2,'mod',13
	dw offset(mod32)
	db 255

ftab32	db 2,'not',13
	dw offset(fnot32)
	db 3,'input',13
	dw offset(fi)
	db 3,'inputh'
	dw offset(fih)
	db 2,'rnd',13
	dw offset(frnd32)
	db 2,'peek'
	dw offset(fpek32)
	db 3,'timer',13
	dw offset(ftim32)
	db 3,'getint'
	dw offset(fgvect)
	db 2,'imul'
	dw offset(fimul)
	db 255


;Operators valid for variable definition.

opers	db 1,'='
	dw offset(fequal)
	db 2,'+='
	dw offset(fadde)
	db 2,'-='
	dw offset(fsube)
	db 2,'*='
	dw offset(fmulte)
	db 2,'/='
	dw offset(fdive)
	db 2,'++'
	dw offset(finc)
	db 2,'--'
	dw offset(fdec)
	db 1,'@'
	dw offset(fbase)
	db 1,'['
	dw offset(endnas)
	db 255


;Operators valid for 32 bit variables.

oper32	db 1,'='
	dw offset(equal32)
	db 2,'+='
	dw offset(adde32)
	db 2,'-='
	dw offset(sube32)
	db 2,'*='
	dw offset(mule32)
	db 2,'/='
	dw offset(dive32)
	db 2,'++'
	dw offset(iic32)
	db 2,'--'
	dw offset(ddc32)
	db 255


;Operators for absolute BYTE arrays.

nasop	db 1,'='
	dw offset(nequal)
	db 2,'+='
	dw offset(nadde)
	db 2,'-='
	dw offset(nsube)
	db 2,'*='
	dw offset(nmulte)
	db 2,'/='
	dw offset(ndive)
	db 2,'++'
	dw offset(ninc)
	db 2,'--'
	dw offset(ndec)

	db 255


;Operators for absolute BYTE arrays.

nasopb	db 1,'='
	dw offset(nbequal)
	db 2,'+='
	dw offset(nbadde)
	db 2,'-='
	dw offset(nbsube)
	db 2,'*='
	dw offset(nbmulte)
	db 2,'/='
	dw offset(nbdive)
	db 2,'++'
	dw offset(nbinc)
	db 2,'--'
	dw offset(nbdec)
	db 255


rwthen	db 2,'then',1
rwelse	db 2,'else',2
rwto	db 1,'to',3
rwstep	db 2,'step',4
rwat	db 1,'at',5
rwwith	db 2,'with',6
rwfrom	db 2,'from',7
rwfor	db 2,'for',13,8
rwand	db 2,'and',13,9
rwxor	db 2,'xor',13,10
rwor	db 1,'or',11
rwgoto	db 2,'goto',15
rwgosub db 3,'gosub',13,16

rhash	db 89,8,225,180,88,248,122,6,44,126,159,239,192
rvect	dw rwgosub,rwgoto,rwor,rwxor,rwand,rwfor,rwfrom,rwwith,rwat,rwstep
	dw rwto,rwelse,rwthen

rlook	call prepfnd
	jc rlookr
	cld
	mov bx,cs
	mov es,bx
	mov cx,13
	lea di,rhash
	repne scasb
	jne rlookr
	lea di,rvect
	add cx,cx
	add di,cx
	mov di,[di]
	mov cl,[di]
	inc di
	mov si,prestr
	repe cmpsw
	jne rlookr
	mov ah,[di]	;Get number of word if found else meaningless.
	mov si,skipsi
	mov spoint,si
	ret
rlookr	sub al,al
	stc
	ret

findnc	call skip2
	jc fnca
	cld
	mov dx,di
	lea di,fj1
	mov ax,ds
	mov es,ax
	mov al,[si]
	mov cx,17
	repne scasb
	jne fnca

	mov di,dx
flnc	mov cl,[di]
	inc di
	mov dx,cx	;Length to add to spoint.
	mov si,spoint
	repe cmpsb
	je matnc
	add di,cx
	add di,2
	cmpb [di],255
	jne flnc
fnca	sub al,al
	stc		;Carry set.
	ret
matnc	dec di		;Set DI = one before offset address.
	add spoint,dx
	clc
	ret

english mov al,[si]
	call sept
	jnc engret
	sub al,al
	stc
engret	ret

;32 bit arithmetic - DEC32

dec32	call mal32
	jc inot32	;Must be at least one number.
a232	call join32	;Get a join of some sort.
	jc nmd32	;If not valid then ignore, leave as is.
	jmps a232
join32	push ax,dx
	call skip2
	cmp al,'+'
	je da32
	cmp al,'-'
	je ds32
	cmp al,'*'
	je dm32
	cmp al,'/'
	je dd32
	pop dx,ax	;Return with the number.
	stc
inot32	ret

da32	push spoint
	incw spoint
	call dec32
	jc stop32
	call asfrend
	jc stop32
	pop bx
	pop cx,bx
	add ax,bx
	jnc da32c
	inc dx
da32c	add dx,cx
	clc
	ret

ds32	push spoint
	incw spoint
	call mal32
	jc stop32
	call asfrend
	jc stop32
	pop bx
	pop cx,bx
	sub cx,dx
	sub bx,ax
	jnc ds32c
	dec cx
ds32c	mov ax,bx
	mov cx,dx
nmd32	clc
	ret

dm32	push spoint
	incw spoint
	call mal32
	jc stop32
	pop bx		;Drop spoint.
	pop cx,bx	;Get last value.
	jmp emul32

stop32	pop spoint	;Reset to where we came from.
	pop dx,ax
	stc
	ret

dd32	push spoint
	incw spoint
	call mal32
	jc stop32
	pop bx		;Drop spoint.
	pop cx,bx	;Get last value.
	xchg ax,bx
	or ax,ax
	jnz dv32ok
	or dx,dx
	jz dzeroj
dv32ok	push dx,ax,cx,bx
	jmp ediv32
dzeroj	jmp divzero

mal32	call skip2
	mov stdec,si
	jc shit32
	mov al,-1[si]
	call sept
	jnc dmin32
	cmpb [si],'-'
	jne dmin32
	incw spoint
	call dec32
	jc nmin32
	sub cx,cx
	mov bx,cx	;Negate:
	sub cx,dx	;= 0 - DX:AX
	sub bx,ax
	jnc sneg32
	dec cx
sneg32	mov ax,bx
	mov dx,cx
	clc
nmin32	ret
dmin32	mov al,[si]
	call ifdigit	;Is AL a digit? NC = not.
	mov ax,0
	mov dx,ax
	jc dc32

	push spoint
	call eword
	pop si
	jc shit32s
	mov cl,al
	mov al,128	;Expected constant.
	cmp cl,label
	je ulab32
	cmp cl,const32
	jne shit32s

ulab32	mov ax,bx
	sub dx,dx
	cmp cl,label
	je swul
	mov dx,es:[di+4]
	clc
swul	ret

shit32s mov spoint,si
shit32	stc		;Syntax error!
	ret

dc32	push ax
	mov al,[si]
	call sept
	pop ax
	jc dend32
	mov bx,ax	;Keep DX:AX in one form or another.
	mov al,[si]
	push bx,dx
	call ifdigit
	pop dx,ax
	jc dlc32
	mov di,si
	mov si,stdec
	sub ax,ax
	mov dx,ax
	jmps dh32

dlc32	mov bx,ax	;Save AX.
	mov al,[si]
	sub al,'0'
	inc si
	push ax 	;Save digit.
	mov ax,bx
	sub cx,cx
	mov bx,10
	push si
	call emul32	;AX:DX * 10
	pop si
	pop bx
	sub bh,bh
	add ax,bx	;Add the digit.
	jnc dc32
	inc dx
	jmps dc32
dend32	mov spoint,si
	clc
	ret		;Return with No Carry from digit.

edh32	inc si
	mov spoint,si	;Skip past the B or H if there is one.
h3nc	clc
	ret

dh32	push ax
	call hex
	pop ax
	jnc xpey32
hlp32	push ax
	call hex
	mov bl,al
	pop ax
	jnc edh32
	push bx
	sub cx,cx
	mov bx,16
	push si
	call emul32
	pop si
	pop bx
	sub bh,bh
	add ax,bx	;Add new digit.
	jnc hlc32
	inc dx
hlc32	inc si
	jmps hlp32

hex	mov al,[si]
	sub bh,bh
	mov bl,al
	or al,[bx+lows]	;If a hex letter then make it lower case.
	cmp al,'0'
	jb h3nc
	cmp al,'9'
	jbe h3yes	;Only return ok if either 0-9 or a-f.
	cmp al,'h'
	je ed3h
	cmp al,'a'
	jb h3nc
	cmp al,'f'
	ja h3nc
	sub al,39	;'A'-10-'0'
h3yes	sub al,'0'      ;Make AL between 0-15
	stc
	ret
ed3h	inc si
	mov spoint,si
	clc
	ret

lexp32	call skip2
	jc expc32
	call fste32	;Get one expression.
	jnc next32	;Abort if error else get next.
	cmp al,100
	jne xpey32
expc32	mov al,30
xpey32	stc
expnc32 ret

next32	call endx32	;If the next word is a function then not end of exp.
	jnc expnc32	;NC = end.
	mov spoint,cx
	call gotf32
	jnc next32
	ret

fste32	call gfunc32
	jnc gotf32	;A function.
	mov si,spoint
	cmpb [si],'('
	jne nu32m

	incw spoint
	jmp fope32

nu32m	call dec32
	jc tra32

	movb [bp],0b8h
	mov [bp+1],ax
	movb [bp+3],0bah
	mov [bp+4],dx
	add bp,6
	ret

gotf32	mov di,1[di]
	jmp loopz

tra32	call lvar32
	jnc trv32
	call loopvar
	jc tre16
trv16	movw [bp],0d229h	;SUB DX,DX
	movb [bp+2],0a1h
	add bp,3
	mov ax,wvar
	jmp usv

tre16	mov al,2
	ret
trv32	movb [bp],0a1h
	inc bp
	mov ax,wvar
	call usv
	movw [bp],168bh
	add bp,2
	inc ax
	inc ax
	jmp usv

lvar32	push spoint
	call findbin
	pop bx
	jc lve32
	mov es,labseg
	cmpb es:[di+1],var32
	jne excc32
	mov ax,es:[di+2]
	mov wvar,ax
	ret
excc32	mov al,18	;Defined as something else.
lve32	mov spoint,bx
	stc
	ret

usv	cmp ah,250	;Special variables?
	jae usvx
	mov di,oplace
	mov es,oneseg
	push ax
	mov ax,bp
	mov es:[di],ah
	mov es:[di+1],al	;HIGH,LOW !!!
	pop ax
	add oplace,2
	mov [bp],ax
	add bp,2
	ret
usvx	xor ax,0ffffh	;Put special variable.
	mov [bp],ax
	add bp,2
	ret

strace	dw 0,0		;Trace source pointer.

t_put	movb [bp],0cch	;INT 03 (breakpoint).
	inc bp

	mov ax,strace
	mov dx,strace+2

	mov es,ftseg
	mov di,t_point
	mov bl,t_file
	mov es:[di],bl	;Set file number.
	mov es:[di+1],bp	;Code address.
	mov es:[di+3],ax
	mov es:[di+5],dl	;Source address.
	add t_point,6
	mov al,154	;Too many lines compiled?
	ret

t_anof	mov al,t_files	;DX points to the filename.
	mov bl,64
	mul bl
	inc ax		;Address for file name.
	mov es,ftseg
	mov di,ax
	mov si,dx
	cld
	mov cx,64
	rep movsb	;Move the name across.
	ret


#include fastproc.asm
#include fastfunc.asm
#include fastlib.asm


fsend	endp
