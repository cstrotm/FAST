;==============================================================================
;= * PCG * GRAPHICAL PRESENTER FOR MAGIX * BY PETER CAMPBELL - 01 Nov. 1988 * =
;==============================================================================

;- Printing codes -------------------------------------------------------------
;  report FD/field "^nnn" nnn is a decimal number eg ^027 sends the ESC char.
;------------------------------------------------------------------------------

;- Alarm codes ----------------------------------------------------------------
;  say 'A:hh:mm,n	   Beeps n times when the time is hh:mm, 24 hr.
;  ! Not implemented because I don't have enough info.
;------------------------------------------------------------------------------

;- Graphics control codes -----------------------------------------------------
;  say 'G:TITLE';	   Print message (cls & set graphics first time only).
;  say 'x1 xstep';	   Starting x and step. If x1=9999 then no x scaling.
;  say 'y1 ystep [ybase]'; Starting y and step, ybase is optional.
;  say 'x y';		   x and y.
;
;  say 'G:@x y';	   Set message print location at coord x,y.
;  say 'G:ax y w';	   Bar graph, same as 'G:b' but automatically fills it.
;  say 'G:bx y w';	   Bar graph, x-xbase coordinate and y value, width=w.
;  say 'G:c';		   Clears graphics screen.
;  say 'G:d';		   Toggles double line mode.
;  say 'G:e';		   End graph/test (must be done to reset SAY statement).
;  say 'G:fx y';	   Primitive fill, y is at top of the area.
;  say 'G:g';		   Draw grid on graph - dotted line grid.
;  say 'G:kn';		   Sets pattern to n. 4 = solid (default).
;  say 'G:lx1 y1 x2 y2';   Draws a line from x1,y1 to x2,y2 (screen coords).
;  say 'G:p';		   Dump graph to printer.
;  say 'G:o0/1';	   Sets pattern change on or off.
;  say 'G:r';		   Reset lines and text position for new graph line.
;  say 'G:s';		   Toggle shading mode (all lines, not text).
;  say 'G:t';		   Clears screen and draws programmed TITLE.
;  say 'G:u';		   Toggle underline mode.
;  say 'G:w';		   Wait for keypress (check PrtSc = print).
;  say 'G:x';		   Place diamond shape at each point graphed.
;------------------------------------------------------------------------------

	#org 0

panas	equ 1
laser	equ 0

xscale	equ 60
yscale	equ 30

;== HEADER INFO ===============================================================

	dw 55aah
	dw last 	;Size

#include psrtr.ain

e_pinit retf
	ds 3
e_init	push ax,dx,di

	mov ax,[u2pxmt]
	mov dx,[u2pxmt+2]
	mov cs:prto,ax
	mov cs:prts,dx	   ;Save old vector.

	lea ax,newprt
	mov [u2pxmt],ax
	mov [u2pxmt+2],cs	;Set new vector.

	lea di,colsay
	mov al,[m2hdwf]
	and al,hdf$egl+hdf$ti
	jnz nonherc
	mov ax,[s2sseg]
	cmp ax,0b000h
	jne nonherc

	lea di,newsay

nonherc mov ax,[s2vpem]
	mov dx,[s2vpem+2]
	mov cs:sayo,ax
	mov cs:says,dx		;Save old vector.
	mov [s2vpem],di
	mov [s2vpem+2],cs	;Set new vector.

	mov ax,[s2vkbin]
	mov dx,[s2vkbin+2]
	mov cs:kbo,ax
	mov cs:kbs,dx		;Save old vector.
	lea ax,newkb
	mov [s2vkbin],ax
	mov [s2vkbin+2],cs	;Set new vector.

	mov ax,[u2vmclk]
	mov dx,[u2vmclk+2]
	mov cs:mbo,ax
	mov cs:mbs,dx		;Save old vector.
	lea ax,minute
	mov [u2vmclk],ax
	mov [u2vmclk+2],cs	;Set minute clock.

	pop di,dx,ax
	retf

;== PXMT ======================================================================

maxcode equ 6

newprt	cmp al,'^'  ;If '^' then needs 3 more characters to form code.
	je setmode
	cmpb cs:mode,0
	je prtold   ;Not expecting any characters.

	push bx
	sub al,'0'
	mov bh,al
	mov bl,10
	mov al,cs:code
	mul bl
	add al,bh
	pop bx
	mov cs:code,al
	decb cs:mode
	jz prtold
	retf

setmode movb cs:mode,3	;Needs three digits.
	movb cs:code,0	;Current answer.
	retf

prtold	jmpf cs:prto

;== NEWSAY ====================================================================

newsay	push ax,bx,cx,dx,di,si,es

	mov di,si
	add di,cx
	movb [di],0	;End character for string.

	cmpb cs:status,0
	jz saylp
sspj	jmp sspec

sayoldj jmp sayold

saylp	cmpw [si],':'*256+'G'
	jne sayoldj
	inc si
	inc si

	push si
	call hset   ;Set graphics mode.
	pop si

	call hreset
	cmpb [si],'a'
	jb tnl
	sub si,2	;Process the code.
	jmps sspj
tnl	call msg		;Print the title.
	movb cs:under,0 	;Underline off.

	movb cs:status,1	;Next will need the graph details.
	call credit

	jmps exits

sayold	pop es,si,di,dx,cx,bx,ax
	jmpf cs:sayo

chcls	call hclear
exits	pop es,si,di,dx,cx,bx,ax
	retf

chend	movb cs:status,0
	call scrnorm
	jmps exits

chmpos	inc si
	call getn	;Get column for new printing.
	jc exits
	call space
	jc exits
	push ax
	call getn	;Get row for new printing.
	pop dx
	jc exits
	call hadd
	mov cs:mpos,bx
	jmp exits

chunder xorb cs:under,1
	jmp exits

chline	inc si
	mov cs:ssavep,sp	;Save stack pointer.
	call getn
	jc chlc
	call space
	jc chlc
	push ax
	call getn	;Get coordinates X1 Y1 X2 Y2
	jc chlc
	call space
	jc chlc
	push ax
	call getn
	jc chlc
	call space
	jc chlc
	push ax
	call getn
	jc chlc
	mov bx,ax
	pop cx,ax,dx
	call line
chlc	mov sp,cs:ssavep	;Restore stack pointer.
	jmp exits

tdouble mov di,cs:vpower
	cmp di,offset(power)
	je don
	lea di,power
dret	mov cs:vpower,di
	jmp exits
don	lea di,powerd
	jmps dret

shade	xorb cs:shadef,1
	jmp exits

chwait	movb cs:waiting,1	;Wait for keypress (check for PrtSc).
wtime	sti			;Just be 100% sure interupts are running.
	cmpb cs:waiting,1
	je wtime		;Wait for NEWKB to reset waiting flag.
exitsj	jmp exits

;== SSPEC =====================================================================

sspec	cmpb [si],' '
	je exitsj	;Ignore null say (wipeout).

	cmpb [si],'G'
	je nsays	;If not G: then must be the XY coordinates i'm going to
sayj	jmp sayxy
nsays	inc si		;graph?
	cmpb [si],':'
	jne sayj
	inc si

	mov al,[si] ;Command?
	cmp al,'t'
	jne tnpg
	jmp tprogl	;Title.
tnpg	cmp al,'r'
	jne nreset
	call hreset ;Reset for new lines.
	jmp exits
nreset	cmp al,'c'
	jne nchcls  ;Clear screen.
	jmp chcls
nchcls	cmp al,'e'
	jne nchend  ;End graph coordinates.
	jmp chend
nchend	cmp al,'@'
	jne nchmpos
	jmp chmpos
nchmpos cmp al,'u'  ;Underline mode?
	jne nchund
	jmp chunder
nchund	cmp al,'l'
	jne nhline
	jmp chline
nhline	cmp al,'w'
	je chwait
	cmp al,'f'
	jne afill
	jmp gfill
afill	cmp al,'a'
	jne bfill
	movb cs:[autofil],1
	jmp bars
bfill	cmp al,'b'
	jne nbars
	movb cs:[autofil],0
	jmp bars
nbars	cmp al,'s'
	jne nshade
	jmp shade
nshade	cmp al,'x'
	jne nxshape
	jmp xshape
nxshape cmp al,'p'
	jne nprint

	call hprint	;Print the screen image to a printer - if available.
	jmp exits

nprint	cmp al,'d'
	jne ndouble
	jmp tdouble
ndouble cmp al,'g'
	jne ngrid
	call grid	;Draw grid.
	jmp exits

ngrid	cmp al,'o'
	jne npatt
	jmp opattn

npatt	cmp al,'k'
	jne kpatt
	jmp kpattn

kpatt	call msg    ;Print the message at current MPOS.
	jmp exits

yrj	jmp yrange
xrj	jmp xrange

sayxy	mov al,cs:status	;XY coordinates - status says which ones.
	cmp al,2
	jb xrj
	je yrj

	call getn
	jc xyexit
	call space
	jc xyexit
	push ax     ;Save X.
	call getn
	pop dx	    ;DX=X
	jc xyexit

	sub ax,cs:ybot		;Perform scaling.
	sub dx,cs:xbot
	push dx
	mov bx,yscale
	imul bx 		;Y*=yscale
	idivw cs:ystep
	pop dx
	push ax
	mov bx,xscale
	mov ax,dx
	mul bx
	divw cs:xstep
	mov dx,ax   ;DX=xscale*(X-XBOT)/XSTEP
	pop ax	    ;AX=yscale*(Y-YBOT)/YSTEP

	add dx,cs:xbase 	;X+=xbase.
	mov bx,332
	sub bx,ax
	mov ax,bx	;Y=ybase-Y.

	cmpw cs:xlast,-1
	je jsl	    ;Just set last if this is the first coordinate.

	cmp ax,347
	jg xyexit
	cmp dx,719
	ja xyexit

	push ax,dx
	mov cx,cs:xlast
	mov bx,cs:ylast
	call line	;Draw line from XLAST,YLAST to X,Y.
	pop dx,ax

jsl	mov cs:xlast,dx
	mov cs:ylast,ax
	call ishape
xyexit	jmp exits

yrange	incb cs:status		;Prepare for Y range.
	call getn
	jc xyexit
	call space
	jc xyexit
	push ax
	call getn
	pop bx	    ;BX=starting value.
	jc xyexit
	call space
	jc igsp
	push ax,bx
	call getn	;If another parameter then its the new ybase.
	jc iggn
	mov cs:ybase,ax
iggn	pop bx,ax
igsp	or ax,ax
	jz xyexit   ;No scale if step=0.
	mov cx,ax   ;CX=step

	mov cs:ystep,cx
	mov cs:ybot,bx

	call axis		;Prints axis and x coordinates.
	mov ax,332
	mov bx,cs:ybot
	mov cx,cs:ystep
	sub ax,4
yrloop	push ax,bx,cx
	sub dx,dx
	push bx
	call hadd
	pop ax
	call gpn
	pop cx,bx,ax
	add bx,cx
	sub ax,yscale
	cmp ax,25
	jge yrloop
	jmps xyexit

xrange	incb cs:status	;Prepare for coordinates.
	call getn
	jc xyexit
	call space
	jc xyexit
	push ax
	call getn
	pop bx	    ;BX=starting value.
	jc ny9999
	or ax,ax
	jz ny9999   ;No scale if step=0.

	mov cx,ax   ;CX=step

	mov cs:xstep,cx
	mov cs:xbot,bx
	movb cs:nflag,1

	cmp bx,9999	;9999 is a special case (no numbers along range).
	jne ny9999
	movb cs:nflag,0
	movw cs:xbot,0	;Set bottom for date range.
ny9999	jmp exits

;== XSHAPE ====================================================================

xshape	proc near
	xorb cs:shapef,1
	jmp exits

ishape	cmpb cs:shapef,1
	je iscont
	ret	;Return - not supposed to call anyway.
iscont	mov dx,cs:xlast
	mov ax,cs:ylast
	mov cx,dx
	mov bx,ax
	sub dx,3
	sub bx,2
	push ax,bx,cx,dx
	call line
	pop dx,cx,bx,ax
	add dx,6

	push ax,bx,cx,dx
	call line
	pop dx,cx,bx,ax
	add bx,4

	push ax,bx,cx,dx
	call line
	pop dx,cx,bx,ax
	sub dx,6

	jmp line
	endp

;== MSG =======================================================================

msg	mov ax,cs:mpos		;Print message.
	mov cs:gpos,ax
msgl	mov al,[si]
	or al,al
	jnz msgp
	mov ax,cs:gpos
	mov cs:mpos,ax
msgr	ret
msgp	inc si
	push si
	call gpc	;Print the character.
	pop si
	jmps msgl

dsmsg	mov al,cs:[si]
	or al,al
	jz msgr
	inc si
	push si
	call gpc	;Print the character.
	pop si
	jmps dsmsg

;== HSET ======================================================================

hset	proc near
	cmpb cs:sstatus,0
	jne hsetret
	movb cs:sstatus,1
	call putimg
	lea di,hcode
	mov ax,0a02h
	jmps hset3

hset2	movb cs:sstatus,0

hset3	push ax

	push ax
	mov dx,3bfh
	mov al,3
	out dx,al
	pop ax

	mov dl,0b8h
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

	call hclear

	pop ax
	mov dx,3b8h
	mov al,ah
	out dx,al	    ;OUT control,enable
hsetret ret
	endp

;== HCLEAR ====================================================================

hclear	proc near
	mov es,s2sseg
	mov cx,16384
	sub ax,ax
	sub di,di
	cld
	rep stosw	    ;Clean screen.
	ret
	endp

;== HRESET ====================================================================

hreset	proc near
	movw cs:mpos,35 	;Initialise title pointer.
	movb cs:under,1 	;Set underline mode.
	movw cs:xlast,-1	;Say no previous coordinates.
	movb cs:shadef,0	;Shading off.
	movb cs:shapef,0	;Shape not used.
	lea di,power
	mov cs:vpower,di
	movw cs:xbase,60
	movw cs:ybase,332	;Initialise XBASE & YBASE.
	lea di,pblack
	mov cs:cpat,di		;Initial fill pattern = black.
	movb cs:cpatc,0 	;Change patterns off.
	ret
	endp

;== HADD ======================================================================

hadd	proc near	;AX=y DX=x, return GPOS, [ES]:BX for address & AL=bit.
	mov bh,al   ;BX=(y and 3)*256
	and bh,3
	sub bl,bl
	mov cl,5
	shl bx,cl   ;BX*=32 -> BX=(y and 3)*8192

	shr ax,1
	shr ax,1
	mov cl,90
	mul cl
	add bx,ax   ;BX+=(y/4)*90

	mov al,dl
	mov cl,3
	shr dx,cl
	add bx,dx   ;BX+=x/8

	mov di,cs:vpower
	and al,7
	sub ah,ah
	add di,ax
	mov al,cs:[di] ;Bit to set.

	mov cs:gpos,bx
	ret
	endp

;== HPLOT =====================================================================

;FAST - dot=peekb (power+(hx and 7))
;	m=(hy mod 3)*8192+(hy/3)*80+hx/8
;	video[m]b=video[m]b or dot

hplot	proc near	;AX=y DX=x
	push ax
	mov ah,cs:shadef2
	or ah,ah
	pop ax
	jz hpput
	decb cs:shadef2
	ret

hpput	call hadd
	mov es,s2sseg
	or es:[bx],al
	mov ah,cs:shadef
	mov cs:shadef2,ah
	ret
	endp

;== SPACE =====================================================================

space	proc near	;Space=NC.
	cmpb [si],' '
	jne spc
	inc si
	ret	    ;Space, so return NC.
spc	stc
	ret	    ;Return C, an error.
	endp

;== GETN ======================================================================

getn	proc near
	sub ax,ax   ;AX=0.
	sub bh,bh
	mov bl,[si] ;First character.
	sub bl,'0'
	jc getc
	cmp bl,9
	ja getc     ;Must be a digit.

getlp	mov bl,[si]
	sub bl,'0'
	jc getnc
	cmp bl,9
	ja getnc    ;Must be a digit.

	mov cx,10
	mul cx	    ;AX*=10
	add ax,bx   ;AL+=digit
	inc si
	jmps getlp

getnc	clc
	ret
getc	stc
	ret
	endp

;== GPC =======================================================================

gpc	proc near
	cmp al,'a'
	jb gupper
	cmp al,'z'
	ja gupper
	and al,255-32
gupper	push bx
	mov bx,cs:gpos
	push bx
	lea di,sset
	sub ah,ah
	cmp al,'/'	;Special characters?
	je gpcds
	inc ah
	cmp al,'*'
	je gpcds
	inc ah
	cmp al,'-'
	je gpcds
	inc ah
	cmp al,','
	je gpcds
	inc ah
	cmp al,'%'
	je gpcds
	inc ah
	cmp al,'.'
	je gpcds
	inc ah
	cmp al,'<'
	je gpcds
	inc ah
	cmp al,'>'
	je gpcds

	sub ah,ah	;Assume 0 character (space).
	cmp al,'0'
	jb gpcdo
	cmp al,'9'
	ja gpcaq
	sub al,'0'-1	;Digits from 1-10.
	mov ah,al
	jmps gpcdo2

gpcaq	cmp al,'A'
	jb gpcdo
	cmp al,'Z'
	ja gpcdo
	sub al,'A'-11	;Letters from 11-36.

gpcdo2	mov ah,al
gpcdo	lea di,cset
gpcds	mov al,ah
	mov ah,8
	mul ah
	add di,ax	;DI=cset+char*8
	mov es,s2sseg

	mov ah,8
gpcl	mov al,cs:[di]
	inc di
	mov si,cs:gpos
	mov es:[si],al	;Merge with any other screen image.
	call gdown	;gpos down one line.
	dec ah
	jnz gpcl

	cmpb cs:under,1
	jne nound
	call gdown
	movb es:[si],255

nound	pop bx
	mov cs:gpos,bx
	pop bx
	incw cs:gpos
	ret
	endp

;== GPN =======================================================================

gpn	proc near	;Print AX at GPOS.
	mov cl,' '
	test ah,128
	jz gpsign
	neg ax		;Change sign.
	mov cl,'-'
gpsign	push ax
	mov al,cl
	call gpc	;Print the sign if negative.
	pop ax

	mov cl,'0'
	mov bx,10000
	call pdigv
	mov bx,1000
	call pdigv
	mov bx,100
	call pdigv
	mov bl,10
	call pdigv
	mov bl,1
	sub cl,cl
pdigv	mov dx,0
	div bx
	add al,48
	push dx
	cmp al,cl
	je pzero
	call gpc    ;Print each digit.
	sub cl,cl
pzero	pop ax
	ret
	endp

;== LINE ======================================================================

line	proc near	;LINE (DX,AX)-(CX,BX)
	cmp dx,cx
	jbe xok
	xchg dx,cx
	xchg ax,bx	;Swap coordinates over?

xok	movb cs:xlow,0
	movw cs:ylow,0

	cmp ax,bx	;Which is higher - Y1 or Y2?
	movw cs:yinc,0

	mov di,cx
	sub di,dx   ;Difference between x's.

	mov si,bx
	sub si,ax
	jns igy
	neg si	    ;Difference between y's.
	movw cs:yinc,-1

igy	movb cs:xinc,255   ;Make up default XINC and YINC.
	push ax,bx,dx
	mov ax,si
	mov bx,255
	mul bx
	or di,di
	jz dy0
	div di
dy0	xor cs:yinc,ax

	cmp di,si
	jae drawit

	xor cs:yinc,ax	   ;Restore YINC to signed only.
	xorw cs:yinc,255
	mov ax,di
	mov bx,255
	mul bx
	or si,si
	jz dx0
	div si
dx0	mov cs:xinc,al

drawit	pop dx,bx,ax

drawl	push cx,bx,dx,ax    ;Save coordinates.
	call hplot	    ;Plot the dot.

	pop ax,dx	    ;Retrieve X1 & Y1.
	mov bl,cs:xlow
	add bl,cs:xinc
	jnc dpx
	inc dx
dpx	mov cs:xlow,bl

	mov bx,cs:ylow
	add bx,cs:yinc
	cmp bx,255
	jg da1
	cmp bx,-255
	jg dpop
	dec ax		;Y1--
	inc bh
	jmps dpop

da1	inc ax		;Y1++
	dec bh

dpop	mov cs:ylow,bx
	pop bx,cx
	cmp dx,cx
	ja dpend	;Exceed x limit?

	cmpw cs:yinc,0
	jl dclow
	cmp ax,bx
	ja dpend	;Exceed y limit?
	jmps drawl
dclow	cmp ax,bx	;Exceeded?
	jae drawl

dpend	ret
	endp

;== GDOWN =====================================================================

gdown	proc near   ;GPOS down one line.
	mov si,cs:gpos
	add si,8192
	cmp si,32768
	jb gdput
	sub si,32768-90
gdput	mov cs:gpos,si
	ret
	endp

;== NEWKB =====================================================================

newkb	cmpb cs:minutef,1
	jne newkm
	movb cs:minutef,0
	push ax
	jmps newg

newkm	cmpb cs:system,1
	je keycl

	cmpb cs:sstatus,0
	jne newap		;If in graphics mode then ignore!

	cmp cl,82
	jne noge
	push ax
	mov al,[s2shft]
	and al,1ah
	cmp al,18h
	je newg
	pop ax

noge	cmp cl,25
	jne newap
	push ax
	mov al,[s2shft]
	and al,18h
	cmp al,18h
	pop ax
	jne newap
	andb [s2shft],239	;Reset ALT.
	movb cs:system,1
	call syspc
	movb cs:system,0
	retf
newap	cmpb cs:waiting,1
	jne kold
	cmp cl,128	;If un-pressing key then ignore.
	jae kold
	cmp cl,55
	jne kreset	;PrtSc?

	call hprint	;Print the screen image to printer - if available.
	jmps kreset2	;Still 'waiting' after this PrtSc.

kreset	movb cs:waiting,0	;Reset waiting flag.
kreset2 retf			;Ignore the key.

newg	pop ax
	andb [s2shft],231	;Reset ALT/SHIFT
	movb cs:system,1
	call boredpc
	movb cs:system,0
	retf

kold	jmpf cs:kbo

keycl	push ax,di
	lea di,ceyspc
	mov al,cl
	and al,127
	cmp al,57
	je keycey
	inc di
	cmp al,75
	je keycey
	inc di
	cmp al,77
	je keycey
	pop di,ax
	movb cs:curk,cl
	retf

keycey	cmp cl,127
	ja ceyz
	cmpb cs:[di],0	;Is key already down?
	jne ceyx
	movb cs:[di],1	;Say key pressed.
	jmps ceyx
ceyz	movb cs:[di],0	;Reset key - not pressed.
ceyx	pop di,ax
	retf

;== HPRINT ====================================================================

hprint	proc near		;Hires Screen Dump.
	cmpb [u2prtr],0 	;Does this workstation have a printer?
	je pnone

	cli
	push ax,bx,cx,dx,di,si,es ;Save registers.

	#if panas
	#include panas.asm
	jmps lpops

prt640	db 27,65,8		;9/72" line spacing.
	db 27,'*',4,208,2	;720 dots per line bit image mode.
	db 255

prtend	db 12,27,65,12		;Normal 1/6" line spacing & ff.
	db 255
	#endif

	#if laser
	#include laser.asm
	jmps lpops

prtinit db 27,'*p420x700Y'	;700=Top, 2200=Bottom.
	db 27,'*t150R'		;150 dots per inch.
	db 27,'*r1A'
	db 255

prttran db 27,'*b90W'
	db 255

prtend	db 27,'*rB'	;Finish page.
	db 12,255
	#endif

lpops	pop es,si,di,dx,cx,bx,ax
	sti
pnone	ret
	endp

;== PCODES ====================================================================

pcodes	proc near
	push ax
pcodesl mov al,cs:[di]
	inc di
	cmp al,255
	jne pcp
	pop ax
	ret
pcp	call pout
	jmps pcodesl
	endp

;== POUT ======================================================================

pout	proc near
	mov dx,3bch
	out dx,al	;OUT character.
	inc dx
poutl	in al,dx
	test al,80h	;Busy?
	jz poutl
	inc dx
	in al,dx
	push ax
	mov al,0dh	;Confirm.
	out dx,al
	mov al,0ch
	out dx,al
	pop ax
	out dx,al
	ret
	endp

;== CREDIT ====================================================================

credit	proc near
	movw cs:gpos,442
	lea si,acspc
	call dsmsg

	mov dx,651	    ;Draw a box around 'ACS/PC'
	mov ax,13
	mov bx,ax
	mov cx,651+56
	push dx,cx
	call line
	pop cx,dx
	mov ax,26
	mov bx,ax
	push dx,ax
	call line
	pop ax,dx
	mov bx,14
	mov cx,dx
	push ax,bx
	call line
	pop bx,ax
	mov dx,651+56
	mov cx,dx
	jmp line

acspc	db 'ACS/PC',0

	endp

;== TPROGL ====================================================================

tprogl	proc near	;Draw pre-programmed title.
	lea di,powerd
	mov cs:vpower,di
	lea si,ctitle
tpl	mov dx,cs:[si]
	mov ax,cs:[si+2]
	mov cx,cs:[si+4]
	mov bx,cs:[si+6]
	cmp dx,-1
	je tpexit
	push si
	call line
	pop si
	add si,8
	jmps tpl
tpexit	movb cs:status,50
	lea di,power
	mov cs:vpower,di
	jmp exits
	endp

;== GFILL =====================================================================

gfill	inc si
	call getn	;Get X coord for fill.
	jc fyexit
	call space
	jc fyexit
	push ax
	call getn	;Get Y coord for fill.
	pop dx
	jc fyexit

fy	push ax,dx
	call hadd
	mov es,s2sseg
	mov di,cs:cpat
	test es:[bx],al
	mov cl,al
	pop dx,ax
	jnz fillx

fleft	test es:[bx],cl
	jnz fright
	rol cl,1
	jnc fleft
	dec bx
	jmps fleft

fright	ror cl,1
	jnc fr2
	inc bx
fr2	test es:[bx],cl
	jnz fdown
	push bx
	mov bx,ax
	and bx,7
	db 2eh		;CS:
	db 84h,09h	;TEST CL,[DI+BX]
	pop bx
	jz fright
	or es:[bx],cl
	jmps fright

fdown	inc ax
	jmps fy

fillx	cmpb cs:cpatc,0    ;Change fill pattern?
	jz fyexit
	add di,8	;Use next pattern.
	cmp di,offset(patend)
	jb fillex
	lea di,pattern
fillex	mov cs:cpat,di

fyexit	jmp exits

;== BARS ======================================================================

bars	inc si
	call getn	;Get X coord for bar, 0-9.
	jc fyexit
	call space
	jc fyexit
	push ax
	call getn	;Get Y coord for top of bar.
	pop dx
	jc fyexit
	call space
	jc fyexit
	push ax,dx
	call getn	;Get Y coord for top of bar.
	mov cs:bx2,ax
	pop dx,ax

	add dx,xscale	;X=coordinate, Y must be converted before lines.
	push dx 	;Save X coord.

	sub ax,cs:ybot	;Perform scaling.
	mov bx,yscale
	imul bx 	;Y*=yscale
	idivw cs:ystep

	mov bx,332
	sub bx,ax
	mov ax,bx	;Y=ybase-Y.
	pop dx		;Retrieve X.

	cmp ax,347
	jg bexit
	cmp dx,719
	ja bexit

	push ax,dx
	add dx,4
	sub ax,11

	cmp ax,cs:ybase
	jbe barsp
	add ax,13		;If below the line then set print pos below bar.

barsp	call hadd
;	inc bx
	mov cs:mpos,bx	   ;Set printing position.
	dec bx
	pop dx,ax

	push dx,ax	;Save x,y for auto fill if required.

	push ax,dx
	mov cx,dx
	mov bx,cs:ybase
	call line
	pop dx,ax

	mov cx,dx
	add cx,cs:bx2
	mov bx,ax
	push cx,bx
	call line
	pop bx,cx

	mov ax,cs:ybase
	mov dx,cx
	call line

	pop ax,dx
	cmpb cs:autofil,1
	jne bexit
	mov bx,cs:ybase
	cmp ax,bx
	jl filly
	je bexit	;Don't fill if on the line.
	mov ax,bx

filly	inc ax
	add dx,6
	jmp fy

bexit	jmp exits

autofil db 0

;== AXIS ======================================================================

axis	proc near
	mov dx,cs:xbase    ;Print the axis.
	mov bx,10*yscale+8
	mov ax,332
	sub ax,bx
	mov cx,dx
	mov bx,332
	call line	    ;Vertical line.

	mov dx,cs:xbase
	mov ax,cs:ybase
	mov cx,dx
	add cx,12+xscale*10
	mov bx,ax
	call line	    ;Horizontal line.

	mov ax,332
ta1	push ax
	mov dx,cs:xbase
	sub dx,4
	mov cx,dx
	add cx,6
	mov bx,ax
	call line
	pop ax
	sub ax,yscale
	cmp ax,25
	jae ta1

	mov dx,cs:xbase
	add dx,xscale
ta2	push dx
	mov ax,cs:ybase
	dec ax
	mov cx,dx
	mov bx,ax
	add bx,3
	call line
	pop dx
	add dx,xscale
	mov ax,cs:xbase
	add ax,10*xscale
	cmp dx,ax
	jbe ta2

	cmpb cs:nflag,1
	jne xleft
	mov bx,cs:xbot
	mov cx,cs:xstep
	mov dx,cs:xbase
	sub dx,10
xrloop	push dx,bx,cx
	mov ax,cs:ybase
	add ax,6
	push bx
	call hadd
	pop ax
	call gpn
	pop cx,bx,dx
	add bx,cx
	add dx,xscale
	mov ax,cs:xbase
	add ax,xscale*10+10
	cmp dx,ax
	jbe xrloop

xleft	ret
	endp

;== COLSAY ====================================================================

colsay	push ax,si

	cmpb [si],'G'
	jne colold
	inc si
	cmpb [si],':'
	jne colold

	movb cs:status,1
	mov al,[si+1]
	cmp al,'e'
	jne colold

	movb cs:status,0

colold	cmpb cs:status,0
	jnz colex
	pop si,ax
	jmpf cs:sayo

colex	pop si,ax
	retf

;== CSET ======================================================================

cset	db 0,0,0,0,0,0,0,0  ;Space.

	db 01111100b	;0
	db 11000110b
	db 11001110b
	db 11011110b
	db 11110110b
	db 11100110b
	db 11000110b
	db 01111100b

	db 00011000b	;1
	db 01111000b
	db 00011000b
	db 00011000b
	db 00011000b
	db 00011000b
	db 00011000b
	db 01111110b

	db 01111100b	;2
	db 10000110b
	db 00001100b
	db 00011000b
	db 00110000b
	db 01100000b
	db 11000010b
	db 11111110b

	db 01111100b	;3
	db 10000110b
	db 00000110b
	db 00011100b
	db 00000110b
	db 00000110b
	db 10000110b
	db 01111100b

	db 00011100b	;4
	db 00111100b
	db 01101100b
	db 11001100b
	db 11111110b
	db 00001100b
	db 00001100b
	db 00001100b

	db 11111110b	;5
	db 11000000b
	db 11000000b
	db 11111100b
	db 00000110b
	db 00000110b
	db 10000110b
	db 01111100b

	db 00111000b	;6
	db 01100000b
	db 11000000b
	db 11111100b
	db 11000110b
	db 11000110b
	db 11000110b
	db 01111100b

	db 11111110b	;7
	db 11000110b
	db 00000110b
	db 00001100b
	db 00011000b
	db 00110000b
	db 01100000b
	db 01100000b

	db 01111100b	;8
	db 11000110b
	db 11000110b
	db 01111100b
	db 11000110b
	db 11000110b
	db 11000110b
	db 01111100b

	db 01111100b	;9
	db 11000110b
	db 11000110b
	db 01111110b
	db 00000110b
	db 00000110b
	db 00001100b
	db 01111000b

	db 00011000b	;A
	db 00111100b
	db 01100110b
	db 11000110b
	db 11111110b
	db 11000110b
	db 11000110b
	db 11000110b

	db 11111100b	;B
	db 11000110b
	db 11000110b
	db 11111100b
	db 11000110b
	db 11000110b
	db 11000110b
	db 11111100b

	db 00111100b	;C
	db 01100110b
	db 11000000b
	db 11000000b
	db 11000000b
	db 11000000b
	db 01100110b
	db 00111100b

	db 11111000b	;D
	db 11001100b
	db 11000110b
	db 11000110b
	db 11000110b
	db 11000110b
	db 11001100b
	db 11111000b

	db 11111110b	;E
	db 11000010b
	db 11000000b
	db 11111000b
	db 11000000b
	db 11000000b
	db 11000010b
	db 11111110b

	db 11111110b	;F
	db 11000010b
	db 11000000b
	db 11111000b
	db 11000000b
	db 11000000b
	db 11000000b
	db 11000000b

	db 00111100b	;G
	db 01100010b
	db 11000000b
	db 11000000b
	db 11011100b
	db 11000110b
	db 11000110b
	db 01111100b

	db 11000110b	;H
	db 11000110b
	db 11000110b
	db 11000110b
	db 11111110b
	db 11000110b
	db 11000110b
	db 11000110b

	db 00111100b	;I
	db 00011000b
	db 00011000b
	db 00011000b
	db 00011000b
	db 00011000b
	db 00011000b
	db 00111100b

	db 00011110b	;J
	db 00001100b
	db 00001100b
	db 00001100b
	db 00001100b
	db 11001100b
	db 11001100b
	db 01111000b

	db 11000110b	;K
	db 11001100b
	db 11011000b
	db 11110000b
	db 11110000b
	db 11011000b
	db 11001100b
	db 11000110b

	db 11000000b	;L
	db 11000000b
	db 11000000b
	db 11000000b
	db 11000000b
	db 11000000b
	db 11000010b
	db 11111110b

	db 10000010b	;M
	db 11101110b
	db 11010110b
	db 11000110b
	db 11000110b
	db 11000110b
	db 11000110b
	db 11000110b

	db 11000110b	;N
	db 11100110b
	db 11100110b
	db 11010110b
	db 11010110b
	db 11001110b
	db 11001110b
	db 11000110b

	db 00111000b	;O
	db 01111100b
	db 11000110b
	db 11000110b
	db 11000110b
	db 11000110b
	db 01111100b
	db 00111000b

	db 11111110b	;P
	db 11000110b
	db 11000110b
	db 11111100b
	db 11000000b
	db 11000000b
	db 11000000b
	db 11000000b

	db 01111100b	;Q
	db 11000110b
	db 11000110b
	db 11000110b
	db 11000110b
	db 11010110b
	db 01111100b
	db 00000110b

	db 11111100b	;R
	db 11000110b
	db 11000110b
	db 11111110b
	db 11011000b
	db 11001100b
	db 11000110b
	db 11000110b

	db 01111100b	;S
	db 11000110b
	db 11000000b
	db 01111000b
	db 00001100b
	db 00000110b
	db 10000110b
	db 01111100b

	db 11111110b	;T
	db 00011000b
	db 00011000b
	db 00011000b
	db 00011000b
	db 00011000b
	db 00011000b
	db 00011000b

	db 11000110b	;U
	db 11000110b
	db 11000110b
	db 11000110b
	db 11000110b
	db 11000110b
	db 11000110b
	db 01111100b

	db 11000110b	;V
	db 11000110b
	db 11000110b
	db 11000110b
	db 11000110b
	db 01101100b
	db 00111000b
	db 00010000b

	db 11000110b	;W
	db 11000110b
	db 11000110b
	db 11000110b
	db 11010110b
	db 11111110b
	db 01101110b
	db 01000100b

	db 11000110b	;X
	db 11000110b
	db 01101100b
	db 00111000b
	db 00111000b
	db 01101100b
	db 11000110b
	db 11000110b

	db 11000110b	;Y
	db 11000110b
	db 11000110b
	db 01101100b
	db 00011000b
	db 00011000b
	db 00011000b
	db 00111100b

	db 11111110b	;Z
	db 10000110b
	db 00001100b
	db 00011000b
	db 00110000b
	db 01100000b
	db 11000010b
	db 11111110b

sset	db 00000001b	;/
	db 00000011b
	db 00000110b
	db 00001100b
	db 00011000b
	db 00110000b
	db 01100000b
	db 11000000b

	db 00000000b	;*
	db 00000000b
	db 01010100b
	db 00111000b
	db 11111110b
	db 00111000b
	db 01010100b
	db 00000000b

	db 00000000b	;-
	db 00000000b
	db 00000000b
	db 00000000b
	db 01111110b
	db 00000000b
	db 00000000b
	db 00000000b

	db 00000000b	;,
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00011000b
	db 00011000b
	db 00100000b

	db 11000001b	;%
	db 11000011b
	db 00000110b
	db 00001100b
	db 00011000b
	db 00110000b
	db 01100011b
	db 11000011b

	db 00000000b	;.
	db 00000000b
	db 00000000b
	db 00000000b
	db 00000000b
	db 00011000b
	db 00011000b
	db 00000000b

	db 00000000b	;<
	db 00011000b
	db 00110000b
	db 01100000b
	db 11000000b
	db 01100000b
	db 00110000b
	db 00011000b

	db 00000000b	;>
	db 00011000b
	db 00001100b
	db 00000110b
	db 00000011b
	db 00000110b
	db 00001100b
	db 00011000b

;== DATA ======================================================================

rest	db '*MAGIX.SBKI*',0
simage	ds 24*80

sstatus db 0
sayo	dw ?
says	dw ?
kbo	dw ?
kbs	dw ?
status	db 0
under	db ?
shadef	db ?
shadef2 db 0
waiting db 0
shapef	db ?
ssavep	dw ?

cpat	dw ?
cpatc	db 0

gpos	dw ?
mpos	dw ?
xlast	dw ?
ylast	dw ?

prto	dw ?
prts	dw ?
mode	db 0
code	db 0

hcode	db 35h,2dh,2eh,7,5bh,2,57h,57h,2,3,0,0,0,0,0,0
htcode	db 61h,50h,52h,0fh,19h,6,19h,19h,2,13,11,12,0,0,0,0

power	db 128,64,32,16,8,4,2,1
powerd	db 192,96,48,24,12,6,3,3
vpower	dw ?

xlow	db ?
xinc	db ?
ylow	dw ?
yinc	dw ?

xbot	dw ?
ybot	dw ?
xstep	dw ?
ystep	dw ?
bx2	dw ?
nflag	db ?

xbase	dw ?
ybase	dw ?

curk	db ?
system	db 0
dmode	db 0	;0=ascii, 1=hex
dline	dw 64
dpage	dw 1536
dstring ds 32

ctitle
#include title.asm
	dw -1

;== GRID ======================================================================

grid	proc near
	mov al,cs:shadef   ;Save shading flag.
	push ax

	movb cs:shadef,5   ;Shading on.

	mov ax,332
gr1	push ax
	mov dx,cs:xbase
	mov cx,672
	mov bx,ax
	call line
	pop ax
	sub ax,yscale
	cmp ax,25
	jae gr1

	movb cs:shadef,3   ;Shading on.

	mov dx,cs:xbase
	add dx,xscale
gr2	push dx
	mov ax,332
	mov bx,28
	mov cx,dx
	call line
	pop dx
	add dx,xscale
	mov ax,cs:xbase
	add ax,10*xscale
	cmp dx,ax
	jbe gr2

	pop ax
	mov cs:shadef,al
	ret
	endp

;== SYSPC =====================================================================

syspc	proc
	push ax,bx,cx,dx
	push es,di,si,ds
	call putimg
	mov es,[s2sseg]
	sti
	and si,0fff0h		;Paragraph boundary.
	mov ax,ds
	and ax,61440		;64k boundary.
	mov ds,ax

sysd	call dump
	movb cs:curk,0
sysk	mov al,cs:curk
	cmp al,73
	je kpgup
	cmp al,81
	je kpgdn
	cmp al,75
	je kleft
	cmp al,77
	je kright
	cmp al,72
	je kup
	cmp al,80
	je kdown
	cmp al,33
	je sysfind
	cmp al,1
	je sysx
	cmp al,30
	jne kas
	movb cs:dmode,0
	movw cs:dline,64
	movw cs:dpage,1536
	jmps sysd
kas	cmp al,35
	jne sysk
	movb cs:dmode,1
	movw cs:dline,32
	movw cs:dpage,768
	jmps sysd

kup	sub si,cs:dline
	jmps sysd
kdown	add si,cs:dline
	jmps sysd
kpgup	sub si,cs:dpage
	jmps sysd
kpgdn	add si,cs:dpage
	jmps sysd
kleft	mov ax,ds
	sub ah,16
	mov ds,ax
	jmp sysd
kright	mov ax,ds
	add ah,16
	mov ds,ax
	jmp sysd

sysfind inc si
	call sinput
	or cl,cl
	jz sfq

sfq	jmp sysd

sysx	pop ds
	call scrnom2
	pop si,di,es
	pop dx,cx,bx,ax
	ret

dump	mov ah,7
	mov di,160

	push si
dpl	mov bx,ds
	mov al,bh
	call hex
	mov al,bl
	call hex
	mov al,':'
	call prtf
	mov bx,si
	mov al,bh
	call hex
	mov al,bl
	call hex
	mov al,' '
	call prtf

	cmpb cs:dmode,1
	mov cx,16
	je dnr

	mov cx,64
dcr	mov al,[si]
	and al,127
	cmp al,' '
	jae dcrp
	mov al,'.'	;Don't print the illegal characters.
dcrp	inc si
	call prtf
	loop dcr
	jmps dmpend

dnr	mov al,[si]
	inc si
	mov ah,15
	call hex
	mov al,[si]
	inc si
	mov ah,7
	call hex	;Print hex digits.
	loop dnr

dmpend	mov al,' '
	mov cl,6
dmpe2	call prtf
	loop dmpe2

	cmp di,4000
	jb dpl
	pop si
	ret

hex	push ax
	shr al,1
	shr al,1
	shr al,1
	shr al,1
	call hex1
	pop ax
hex1	and al,15
	add al,'0'
	cmp al,'9'
	jbe prtf
	add al,7
prtf	mov es:[di],ax
	add di,2
	ret

sinput	mov cl,0	;Input string - hex or ascii?
	ret

	endp

;== KPATTN ====================================================================

kpattn	inc si
	call getn
	jc opc
	lea di,pattern
	add ax,ax
	add ax,ax
	add ax,ax
	add di,ax
	mov cs:cpat,di
	jmp exits

;== OPATTN ====================================================================

opattn	inc si
	call getn
	jc opc
	mov cs:cpatc,al
opc	jmp exits

;== PATTERN ===================================================================

pattern

	db 10101010b
	db 01010101b
	db 10101010b
	db 01010101b
	db 10101010b
	db 01010101b
	db 10101010b
	db 01010101b

	db 00001111b
	db 11100000b
	db 00000111b
	db 11110000b
	db 00001111b
	db 11100000b
	db 00000111b
	db 11110000b

	db 01001010b
	db 10100100b
	db 01001010b
	db 10100100b
	db 01001010b
	db 10100100b
	db 01001010b
	db 10100100b

	db 11000000b
	db 01100000b
	db 00110000b
	db 00011000b
	db 00001100b
	db 00000110b
	db 00000011b
	db 10000001b

patend

pblack	db 255,255,255,255,255,255,255,255

;== BOREDPC ===================================================================

gbulls	equ 3
genems	equ 15
gdots	equ 25

boredpc proc
	push ax,bx,cx,dx
	push es,di,si
	mov cs:datads,ds
	push ds
	call hreset
	mov cs:under,0
	call hset
	mov es,[s2sseg]
	sti
	push cs
	pop ds		;Set DS=CS!
	mov curk,0	;Reset keypress.
	call prtsh

bgloop	lea di,ceyspc
	mov cl,3
bceylp	cmpb [di],0
	jz bceyn
	decb [di]
	jnz bceyn
	movb [di],4
bceyn	inc di
	dec cl
	jnz bceylp

	call byou
	call bbullet
	call benemy
	call bdot

	cmpb curk,1
	jne bgloop

boredx	pop ds
	call scrnorm
	pop si,di,es
	pop dx,cx,bx,ax
	ret

datads	dw ?
ceyspc	db 0
ceylft	db 0
ceyrig	db 0

dead	db 0
gx	dw 720
gy	dw 340
gscore	dw 0
ghighs	dw 0
gdir	dw 0

gb	ds 6*gbulls

ge	ds 8*genems

gd	ds 8*gdots

prtsh	mov gpos,50
	push di,bx,cx
	lea si,mscore
	mov ds,datads
	call dsmsg
	mov ax,cs:gscore
	call gpn
	lea si,mhighs
	call dsmsg
	mov ax,cs:ghighs
	call gpn
	mov al,' '
	call gpc
	mov al,' '
	call gpc
	mov al,' '
	call gpc
	pop cx,bx,di
	push cs
	pop ds
	ret

byou	cmp dead,0
	jz balive
	dec dead
	jnz byoyr
	mov gscore,0
	call prtsh
byoyr	ret

balive	mov ax,gy
	shr ax,1
	mov dx,gx
	shr dx,1
	lea di,bsyou
	call bsprite

	mov bx,gdir
	cmp ceylft,1
	jne byl2
	inc bx
	jmps byku
byl2	cmp ceyrig,1
	jne bnd
	dec bx
byku	and bx,15
	mov gdir,bx
bnd	mov ax,gdir

	lea di,byinc
	add ax,ax
	add ax,ax
	add di,ax
	mov cx,[di]
	mov dx,[di+2]

	mov ax,gx
	add ax,cx
	cmp ax,10
	jb byxn
	cmp ax,1370
	ja byxn
	mov gx,ax

byxn	mov ax,gy
	add ax,dx
	cmp ax,20
	jb byyn
	cmp ax,660
	ja byyn
	mov gy,ax

byyn	ret

bflag	db 0
bbullet mov cx,gbulls
	lea di,gb
	sub al,al
	cmp dead,0
	jne bnonef
	mov al,ceyspc
bnonef	mov bflag,al
bbloop	push cx
	mov dx,[di]
	mov ax,[di+2]
	or dx,dx
	jnz bbgo

	cmp bflag,1
	jne bbnext
	mov dx,gx
	mov ax,gy
	mov bx,gdir
	mov [di+4],bx	;Set this bullet in motion.
	mov bflag,0	;Fire once only.

bbgo	push di
	mov bx,[di+4]
	lea di,bbinc
	add bx,bx
	add bx,bx
	add di,bx
	add dx,[di]
	add ax,[di+2]
	pop di
	cmp ax,20
	jb bbclr
	cmp dx,10
	jb bbclr
	cmp ax,660
	ja bbclr
	cmp dx,1370
	ja bbclr

	call bhit
	jc bbclr

	mov [di],dx
	shr dx,1
	mov [di+2],ax
	shr ax,1
	push di
	lea di,spbull
	call bsprite
	pop di
	jmps bbnext

bbclr	mov dx,[di]
	shr dx,1
	mov ax,[di+2]
	shr ax,1
	add ax,6
	push di
	lea di,spbclr
	call bsprite
	pop di
	movw [di],0	;Remove bullet.

bbnext	pop cx
	add di,6
	dec cx
	jz bbex
	jmp bbloop
bbex	ret

benemy	mov cx,genems
	lea di,ge
beloop	push cx

	mov dx,[di]
	mov ax,[di+2]
	or dx,dx
	jnz bego

benew	call rnd
	mov [di+4],ax	;Set enemy direction.
	mov dx,ax
	and dx,1023
	add dx,120
	mov [di],dx
	cmp ax,32768
	mov ax,20
	jae begax
	mov ax,640
begax	mov [di+2],ax

bego	push di
	push ax
	call rnd
	mov bx,ax
	pop ax
	cmp bx,20000
	jg begi
	cmp bx,-20000
	jnl begd
	decb [di+4]
	jmps begd
begi	incb [di+4]
begd	mov bx,[di+4]
	and bx,15
	lea di,beinc
	add bx,bx
	add bx,bx
	add di,bx
	add dx,[di]
	add ax,[di+2]
	pop di
	cmp ax,20
	jb beold
	cmp dx,10
	jb beold
	cmp ax,650
	ja beold
	cmp dx,1370
	jbe beuse

beold	mov dx,[di]
	mov ax,[di+2]	;Restore old coordinates.
	jmps becnt
beuse	mov [di],dx
	mov [di+2],ax
becnt	shr dx,1
	shr ax,1
	push di
	push dx,ax

	mov cx,gx
	mov bx,gy
	shr cx,1
	shr bx,1
	sub dx,25
	cmp dx,cx
	jg beali
	add dx,48
	cmp dx,cx
	jl beali
	sub ax,8
	cmp ax,bx
	jg beali
	add ax,15
	cmp ax,bx
	jl beali
	mov dead,60
	mov ax,gscore
	cmp ax,ghighs
	jl ghsn
	mov ghighs,ax
ghsn	pop ax,dx
	mov bx,gdots
	call mdots
	jmps beal2

beali	pop ax,dx
beal2	lea di,spenem
	call bsprite

	pop di

benext	pop cx
	add di,8
	dec cx
	jz beex
	jmp beloop
beex	ret

bdot	mov cx,gdots
	lea di,gd
bdloop	push cx

	cmpw [di],0
	jz bdnext
	mov dx,[di]
	mov ax,[di+2]

	sub cx,cx
	call dotput
	add dx,[di+4]
	add ax,[di+6]
	cmp dx,40
	jb bdres
	cmp ax,40
	jb bdres
	cmp dx,2860
	jae bdres
	cmp ax,1380
	jae bdres

	mov [di],dx
	mov [di+2],ax
	mov cx,-1
	call dotput
	jmps bdnext

bdres	movw [di],0

bdnext	pop cx
	add di,8
	loop bdloop
	ret

dotput	push ax,dx,di
	push cx
	shr dx,1
	shr dx,1
	shr ax,1
	shr ax,1
	call hadd
	lea di,dottab
	and dx,7
	add di,dx
	add di,dx
	mov ax,[di]
	pop cx

	and ax,cx
	mov es:[bx],ax
	pop di,dx,ax
	ret
dottab	db 11111100b,00000000b
	db 01111110b,00000000b
	db 00111111b,00000000b
	db 00011111b,10000000b
	db 00001111b,11000000b
	db 00000111b,11100000b
	db 00000011b,11110000b
	db 00000001b,11111000b

bsprite push di 	;DX,AX @ DI.
	push dx
	call hadd
	mov si,bx
	pop cx
	and cl,7	;ROTATE
	mov bh,-1
	shr bh,cl	;AND1
	mov ch,bh	;AND2
	xor ch,-1

	pop di
	mov dh,[di]	;Get size of sprite.
	inc di

ahsprow push dx
	mov ax,[di]
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

	mov ax,[di]
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

	add si,8188
	cmp si,32768
	jb bcput
	sub si,32768-90
bcput	pop dx
	dec dh
	jnz ahsprow
	ret

and1	dw ?
and2	dw ?
rotate	db ?

mdots	push ax,dx
	add ax,ax
	add ax,ax
	add dx,dx
	add dx,dx
	mov cx,gdots
	lea di,gd
mdloop	cmpw [di],0
	jnz mdnext

	push bx,ax
	mov [di],dx
	mov [di+2],ax
	call rnd
	mov bx,ax
	and bx,31
	add bx,6
	cmp ax,32768
	jb mdxi
	neg bx
mdxi	mov [di+4],bx

	call rnd
	mov bx,ax
	and bx,31
	add bx,6
	cmp ax,32768
	jb mdyi
	neg bx
mdyi	mov [di+6],bx
	pop ax,bx

	dec bx
	jz mdend

mdnext	add di,8
	loop mdloop
mdend	pop dx,ax
	ret

bsyou	db 16
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00011111b,11111000b,00000000b
	db 00000000b,01100000b,00000110b,00000000b
	db 00000001b,10000000b,00000001b,10000000b
	db 00000110b,00000111b,00000000b,01100000b
	db 00001000b,00000111b,00000000b,00010000b
	db 00001000b,00000110b,00000001b,00010000b
	db 00001000b,00000101b,11111010b,00010000b
	db 00001000b,00000100b,00000101b,00010000b
	db 00000110b,00000100b,00000000b,11100000b
	db 00000001b,10000100b,00000001b,10000000b
	db 00000000b,01100111b,00000110b,00000000b
	db 00000000b,00011111b,11111000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b

spbull	db 15
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000011b,11000000b,00000000b
	db 00000000b,00001101b,10110000b,00000000b
	db 00000000b,00000011b,11000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b

spbclr	db 3
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b

spenem	db 12
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00011100b,00001111b,11110000b,00111000b
	db 00000010b,11111111b,11111111b,01000000b
	db 00000011b,11111111b,11111111b,11000000b
	db 00000111b,10000011b,11000001b,11100000b
	db 00000111b,11100001b,10000111b,11100000b
	db 00000011b,11111111b,11111111b,11000000b
	db 00000010b,10101011b,11010101b,01000000b
	db 00000100b,00001111b,11110000b,00100000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b

speclr	db 8
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b
	db 00000000b,00000000b,00000000b,00000000b

byinc	dw 0,4
	dw 2,3
	dw 3,2
	dw 4,1
	dw 6,0
	dw 4,-1
	dw 3,-2
	dw 2,-3
	dw 0,-4
	dw -2,-3
	dw -3,-2
	dw -4,-1
	dw -6,0
	dw -4,1
	dw -3,2
	dw -2,3

bbinc	dw 0,12
	dw 6,9
	dw 9,6
	dw 12,3
	dw 18,0
	dw 12,-3
	dw 9,-6
	dw 6,-9
	dw 0,-12
	dw -6,-9
	dw -9,-6
	dw -12,-3
	dw -18,0
	dw -12,3
	dw -9,6
	dw -6,9

beinc	dw 0,3
	dw 2,3
	dw 3,2
	dw 3,1
	dw 5,0
	dw 3,-1
	dw 3,-2
	dw 2,-3
	dw 0,-3
	dw -2,-3
	dw -3,-2
	dw -3,-1
	dw -5,0
	dw -3,1
	dw -3,2
	dw -2,3

rlast	dw ?
rcons	dw 13
rnd	mov ax,rlast
	adc ax,cx
	sub ax,di
	sub ax,dx
	ror ax,1
	push bx
	mov bx,rcons
	add ax,bx
	inc bx
	cmp bx,60
	jb rndr
	mov bx,13
rndr	mov rcons,bx
	pop bx
	mov rlast,ax
	ret

bhit	push di 	;Return C if bullet has hit an enemy.
	push dx,ax
	shr dx,1
	shr ax,1
	mov cx,genems
	lea di,ge
bhloop	push cx
	mov cx,[di]
	mov bx,[di+2]
	or cx,cx
	jz bhnext

	shr cx,1
	shr bx,1

	sub dx,20
	cmp dx,cx
	jg bhnext
	add dx,45
	cmp dx,cx
	jl bhnext

	sub ax,8
	cmp ax,bx
	jg bhnext
	add ax,19
	cmp ax,bx
	jl bhnext

	add gscore,5	;5 points.
	call prtsh
	movw [di],0
	mov ax,bx
	mov dx,cx
	add ax,2

	mov bx,6	;Explosion.
	call mdots

	lea di,speclr
	call bsprite
	pop cx		;Drop CX.
	jmps bhgone

bhnext	pop cx
	add di,8
	loop bhloop
bhgone	pop ax,dx
	pop di
	ret

mscore	db 'SCORE ',0
mhighs	db '  HIGH ',0

	endp

;== SCRNORM ===================================================================

scrnorm proc
	mov ax,02901h
	lea di,htcode
	call hset2

scrnom2 mov es,s2sseg
	cld
	mov cx,79	;Restore attributes to top line.
	mov ax,7820h
	sub di,di
	rep stosw

	sub si,si
	lea di,rest    ;Put MAGIX.SBK on top line.
chrest	mov al,cs:[di]
	or al,al
	jz scrnr
	mov es:[si],al	    ;Put individual characters.
	add si,2
	inc di
	jmps chrest
scrnr	jmp getimg
	endp

;== PUTIMG ====================================================================

putimg	proc near
	mov di,160
	lea si,simage
	mov cx,24*80
	mov es,[s2sseg]
putil	mov al,es:[di]
	mov cs:[si],al
	add di,2
	inc si
	loop putil
	ret
	endp

;== GETIMG ====================================================================

getimg	proc near
	mov di,160
	lea si,simage
	mov cx,24*80
	mov es,[s2sseg]
	mov ah,7
getil	mov al,cs:[si]
	mov es:[di],ax
	add di,2
	inc si
	loop getil
	ret
	endp

;== MINUTE ====================================================================

minute	push es,di,ax
	mov es,[s2sseg]
	mov di,3*160+136
	cmpb es:[di],'0'
	jne nowaste
	cmpb es:[di+2],'1'
	jne nowaste
	cmpb es:[di+6],'1'
	jne nowaste
	cmpb es:[di+8],'1'
	jne nowaste

	mov di,160+70*2
	cmpb es:[di],'0'
	jne nowaste
	cmpb es:[di+2],'9'
	jne nowaste
	cmpb es:[di+6],'5'
	jne nowaste
	cmpb es:[di+8],'0'
	jb nowaste

	movb cs:minutef,1

nowaste pop ax,di,es
	jmpf cs:mbo

mbo	dw ?
mbs	dw ?
minutef db 0

;== PCG SERVER PROTECTION ==================================================

servchk

servers dw 0448

;== LAST ======================================================================

last
