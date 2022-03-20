;FSI.ASM include file for FAST.ASM and SOFA.ASM


;  (C) Peter Campbell 18/12/1987.
;Entry point of FAST and SOFA is at "begin"


ect	equ 27		;Word end marker. MUST BE < 32
tend	equ 0
onlen	equ 512
;TF	equ xxx 	;First control byte to signify control code is next.
			;** TF must be unique within the assembler code tables.

ercont	equ 250
erwarn	equ 251
erexit	equ 252
ernull	equ 253
lasterr equ 180

cfglen	equ 20		;Configuration file length.

;	Main entry point.
begin	proc near	;All contained within this one near procedure.

	lea dx,introm
	call msg
	lea dx,intros
	call msg
	call onnl
	call onnl

;== Load .CFG file =========================================================

	lea dx,cfg
	mov ax,3d00h
	int 21h
	jc cfgdef

	mov bx,ax
	mov cx,cfglen
	lea dx,bufsize
	mov ah,3fh
	int 21h
	jc cfgdef

	mov ah,3eh
	int 21h
	jnc cfglog

cfgdef	mov cx,cfglen
	cld
	lea si,default
	lea di,bufsize
	rep movsb

cfglog	#if fast
	mov al,pipe
	cmp al,'!'
	je cfgexcl
	mov al,'|'
cfgexcl mov pipem1,al
	mov pipem2,al
	#endif

	mov al,onscrl
	mov di,4000
	mov bl,160
	mul bl
	sub di,ax
	mov sc160,di	;set screen position for fast scrolling

;== Create/Seek end of .LOG file ===========================================

	cmpb logq,'Y'
	jne cfgmem

	lea dx,log
	mov ax,3d02h
	int 21h
	jc logcrt

	mov loghand,ax
	mov bx,ax
	mov ax,4202h
	sub cx,cx
	sub dx,dx
	int 21h 	;Seek end of file.
	jmps cfgmem

logcrt	lea dx,log
	mov ah,3ch
	sub cx,cx
	int 21h 	;Create file.
	jc cfgmem
	mov loghand,ax

	mov bx,ax
	lea dx,logtop
	mov cx,209	;length of log file header
	mov ah,40h
	int 21h

cfgmem
;== Memory Allocation ======================================================

	mov bx,offset(fsend)+15+16+256
	add bx,bufsize
	;Program segment modification.
	mov cl,4
	shr bx,cl	;Size/16 paragraphs.

	mov ah,4ah
	int 21h 	;Modify current segment for program.
	jnc alc1ok
alcfail lea dx,memerr
	jmp lastm

alc1ok	mov ah,48h	;Allocate memory block for code.
	mov bx,4096	;(64k)
	int 21h
	jc alcfail
	mov codeseg,ax
;******************************
	cli
	mov ss,ax	;CODESEG
	mov sp,65500
	sti
;******************************

	#if fast
	mov ah,48h	;Allocate memory block for "name.FT" file.
	mov bx,4096	;(64k)
	int 21h
	jc alcfail
	mov ftseg,ax	;FTSEG.

	mov ax,ss
	mov es,ax	;ES=codeseg.
	mov di,103h
	mov cx,speclen-3
	cld
	sub al,al
	rep stosb
	#endif

	mov ah,48h
	mov bx,maxlab
	mov cl,3	;MAX LABELS * 2 / 16
	shr bx,cl	;Size/8 paragraphs.
	int 21h
	jc alcfail
	mov mxlseg,ax

	mov ah,48h
	mov bx,maxsize
	mov cl,4
	shr bx,cl	;Size/16 paragraphs.
	int 21h
	jc alcfail
	mov labseg,ax

	mov ah,48h
	mov bx,onesize
	mov cl,4
	shr bx,cl	;Size/16 paragraphs.
	int 21h
	jc alcfail
	mov oneseg,ax
	movw oplace,0	;Current position in undefs table.

	lea di,filen+2	;Point to filename to be loaded.
	mov si,81h
loopn1	cmpb [si],13	;End, no valid name so input names.
	je getn1
	cmpb [si],' '
	jne found1	;A valid name character?
	inc si
	jmps loopn1

mchelp	lea dx,helpm
	call msg
	int 20h

maintc	mov al,1[si]
	call lower
	cmp al,'h'
	je mchelp
	cmp al,'?'
	je mchelp
	cmp al,'m'
	mov al,[si]
	jne found1m
	jmp maint

prompts db 1		;Yes, ask prompts.

found1	mov al,[si]
	cmp al,'/'
	je maintc
found1m cmp al,' '
	je getn2
	cmp al,';'
	je getn2p
	cmp al,13
	je getn2
	mov [di],al	;Transferr command line name to filen.
	inc si
	inc di
	jmps found1

getn1	lea dx,entern
	call msg
	lea dx,filen
	mov ah,10
	int 21h
	call newline
	cmpb filen+2,13 ;Was a filename entered or was the enter key pressed?
	je getn1	;Redo if enter pushed only.
	lea di,filen
	call asciiz
	jmps getn2

getn2p	movb prompts,0	;No prompt questions.

getn2	movb [di],0
	lea si,filen+2
	push cs
	pop es
	lea di,runn
	cld
	mov cx,64
	#if fast
	push cx,si
	#endif
	rep movsb	;Copy filename to runtime file.

	#if fast
	pop si,cx
	lea di,t_name
	rep movsb	;Copy filename to fast trace file.
	#endif

	lea di,runn
getlp	cmpb [di],13
	je get2z
	cmpb [di],0
	je get2z
	cmpb [di],'.'           ;Find end of filename of start of extension.
	je get2z
	inc di
	jmps getlp
get2z	movb  [di],'.'
	movb 1[di],'C'
	movb 2[di],'O'
	movb 3[di],'M'
	movb 4[di],0		;COM extension.

	#if fast
	add di,offset(t_name)
	sub di,offset(runn)
	movb  [di],'.'
	movb 1[di],'F'
	movb 2[di],'T'
	movb 3[di],0		;FT extension.
	#endif

	cmpb prompts,0
	jnz entout
	jmp startn

entout	lea dx,outlist
	cmpb symonly,0
	jz entall
	lea dx,symlist
entall	call msg
	lea dx,listt
	mov ah,10
	int 21h
	call newline
	mov si,offset(listt)+2
	cmpb [si],13
	movb output,0
	jne starto
	cmpb symonly,1
	jne symoq
	movb output,2	;Screen is the default for symbols.
	jmps jstart

symoq	cmpb msymbol,'Y'        ;If output=0 then what about symbol table.
	jne jstart
	movb symonly,1	;Symbols only.
	jmps entout

jstart	jmp start

starto	mov al,[si]
	cmp al,13
	je entout
	cmp al,32
	jne starti
	inc si
	jmps starto
starti	movb output,0	;Set default to NULL output.
	call lower
	cmp al,'p'
	jne stoss
	movb output,1
	jmp start
stoss	cmp al,'s'
	jne stodd
	movb output,2
	jmp start
stodd	cmp al,'d'
	jne entout

	movb output,3
	mov ax,cs
	mov es,ax
	lea si,filen+2
	lea di,ldiskn
	cld
	mov cx,32
	rep movsw	;Copy filename to runtime filename.

	lea di,ldiskn
dgetlp	cmpb [di],'.'           ;Find end of filename.
	je dget2z
	cmpb [di],0
	je dget2z
	cmpb [di],13
	je dget2z
	inc di
	jmps dgetlp
dget2z	movb [di],'.'
	movb 1[di],'L'
	movb 2[di],'S'
	movb 3[di],'T'
	movb 4[di],'$'
	movb 5[di],0		;Add the extension ".COM" for the default.
	lea dx,ldiskn
	mov ah,3ch
	sub cx,cx
	int 21h
	jc pdderrd
	mov ohandle,ax
	jmps start

pdderrd lea dx,dderrd
	call msg
	jmp nocode


;Start compiling....

dmust	db ?
omust	db ?	;Type of UNDEF table.
defd	db ?
ount	db ?	;Type for symbol table.

start	call newline
startn	lea di,filen+2
stdot	cmpb [di],0
	je putfext	;If no extension to filename then put ".ASM"
	cmpb [di],'.'
	je ndotext
	inc di
	jmps stdot
putfext movb [di],'.'
	movb 1[di],defext1
	movb 2[di],defext2
	movb 3[di],defext3
	movb 4[di],0	;Must still be ASCIIZ string.

ndotext lea dx,startm
	call msg	;Say that compiling has started.

;== Make LOG details =======================================================

	mov ah,2ah
	int 21h 		;Get date.
	lea di,logtext+12
	sub ah,ah
	push cx,dx
	mov al,dl
	call logn2
	inc di
	pop dx
	mov al,dh
	call logn2
	pop ax
	inc di
	call logn4

	mov ah,2ch
	int 21h 		;Get time.
	lea di,logtext+24
	mov al,ch
	sub ah,ah
	call logn2
	inc di
	mov al,cl
	call logn2

	add di,2
	lea si,filen+2		;Write program name.
logprg	mov al,[si]
	cmp al,'.'
	je logsk
	mov [di],al
	inc si
	inc di
	jmps logprg

logsk	call fwset

	mov ah,2ch
	int 21h 	;What is the starting time?
	mov al,dl
	sub ah,ah
	mov dl,10
	div dl		;AL=0-9
	sub ah,ah
	push ax
	mov al,60
	mul cl
	mov dl,dh
	sub dh,dh
	add ax,dx
	sub dx,dx
	mov bx,10
	mul bx
	pop dx
	add ax,dx
	mov ctime,ax	;minutes*60+seconds.

	lea si,ifstart
	mov ifpoint,si
	movb [si],2

	#if fast ;*******
	mov bx,endpfm
	mov cl,4
	shr bx,cl	;Segment size = pfm/16 paragraphs.
	mov ah,48h
	int 21h 	;Allocate room for PFM's.
	jnc alcpfo
	jmp alcfail
alcpfo	mov pfmseg,ax

	movb fspec,1
	lea di,specvar
	mov buffp,di
	call compspe
	movb fspec,0
	#endif


	;COMPILE MAIN SOURCE FILE (ONCE).

	movb inclf,0	;Not including a file at the moment.
	movw errors,0
	movw clineo,0
	#if fast
	movb doption,0
	#endif

;	*******
	mov bp,org	;Initialise CODEADD !!!!!
;	*******

	call compile

	lea di,logtext+41	;Write number of lines.
	mov ax,clineo
	call logn5

	add di,2		;Write code length.
	mov ax,bp
	sub ax,org
	call logn5

	add di,2
	mov ax,errors
	or ax,ax
	jz lognone
	call logn5

lognone mov ah,40h
	mov cx,62
	lea dx,logtext		;Write log line.
	mov bx,loghand
	or bx,bx
	jz fucklog
	int 21h

fucklog mov ax,errors
	or ax,ax
	jz noep2
	lea dx,passm
	call onm
	mov ax,errors
	lea dx,passme
	call onnm
	jmp nocode

noep2	mov al,13
	call ona
	mov al,10
	call ona

	mov ah,2ch
	int 21h
	mov al,dl
	sub ah,ah
	mov dl,10
	div dl		;AL=0-9
	sub ah,ah
	push ax
	mov al,60
	mul cl
	mov dl,dh
	sub dh,dh
	add ax,dx
	sub dx,dx
	mov bx,10
	mul bx
	pop dx
	add ax,dx

	sub ax,ctime
	jnc secprt
	add ax,36000
secprt	mov bx,10
	sub dx,dx
	div bx
	push dx
	call onn
	mov al,'.'
	call ona
	pop ax
	lea dx,timesec
	call onnm

	mov ax,clineo
	lea dx,asmln
	call onnm

	mov ah,3ch	;CREATE file for saving runtime code.
	lea dx,runn
	sub cx,cx
	int 21h
	jc werr
	push ax

	sub bp,org	;Count of bytes to save.
	mov ax,bp
	push ax
	lea dx,suc2	;Display how many bytes are being written.
	call onnm

	pop cx		;Count of bytes to write.
	pop bx		;File handle.
	mov dx,org
	mov ds,codeseg	;Save code segment.
	mov ah,40h
	int 21h
	mov ax,cs	;Restore DS.
	mov ds,ax
	jnc nwernt	;Report if error writing to file.
werr	lea dx,errm2
	call onm
	jmp end

nwernt	mov ah,3eh
	int 21h 	;Attempt to close the file.
	jc werr

	#if fast
	cmpb hflags+xikeyit,0	;Keyboard interupt used?
	jz noi9
	lea dx,warn9
	call onm
noi9	cmpb hflags+xhfint,0	;Timer tick interupt used?
	jz noi8
	lea dx,warn8
	call onm
	#endif

noi8	#if fast
	cmpb t_start,1
	jne noit
	lea dx,ftsave
	call onm
	mov ah,3ch	;CREATE file for saving FT file.
	lea dx,t_name
	sub cx,cx
	int 21h
	jc werr
	push ax

	mov es,ftseg
	mov al,t_files
	inc al
	mov es:[0],al	;Number of files.
	mov bl,64
	mul bl
	inc ax		;AX=64*files+1
	mov cx,ax
	pop bx		;File handle.

	sub dx,dx
	mov ds,ftseg	;Save FTSEG.
	mov ah,40h
	push bx
	int 21h
	pop bx		;Restore handle.
	mov ax,cs	;Restore DS.
	mov ds,ax
jwerr	jc werr 	;Report if error writing to file.

	mov cx,t_point
	mov dx,t_base
	sub cx,dx
	mov ds,ftseg	;Save FTSEG - address part.
	mov ah,40h
	int 21h
	mov ax,cs	;Restore DS.
	mov ds,ax
	jc jwerr	;Report if error writing to file.

	mov ah,3eh
	int 21h 	;Attempt to close the file.
	jc jwerr
	#endif

noit	cmpb msymbol,'Y'
	jne endsum
	call symbols

endsum	cmpb msumm,'Y'
	jne end
	call summary	
	jmps end

nocode	call newline
	lea dx,nocm
lastm	call msg

end	cmpb output,3
	jne endend
	call onwrite	;Save last bytes.
	mov bx,ohandle
	mov ah,3eh	;If disk file was used then close it.
	int 21h
	jnc endend
	jmp pdderrd

endend	call fwres

	mov bx,loghand
	or bx,bx
	jz nologf
	mov ah,3eh	;Close .LOG file.
	int 21h

nologf	int 20h 	;Terminate program.

newline mov ah,6
	mov dl,13
	int 21h
	mov ah,6
	mov dl,10
	int 21h
	ret

asciiz	mov al,1[di]
	sub ah,ah
	add di,ax
	add di,2
	movb [di],0
	ret

nmsg	push dx
	call nump
	pop dx
msg	mov ah,9
	int 21h
	ret

nump	sub cl,cl
	mov bx,10000
	call pdig
	mov bx,1000
	call pdig
	mov bx,100
	call pdig
	mov bl,10
	call pdig
	mov bl,1
	inc cl
pdig	sub dx,dx
	div bx
	push dx

	mov dl,al
	or al,cl
	jz nopump

	add dl,'0'
	mov ah,6
	int 21h
	mov cl,1
nopump	pop ax
	ret

serror	lea dx,serr
	call msg
	incw errors
	jmp nocode

l	equ 32
lows
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	db 0,l,l,l,l,l,l,l,l,l,l,l,l,l,l,l
	db l,l,l,l,l,l,l,l,l,l,l,0,0,0,0,0
	db 0,l,l,l,l,l,l,l,l,l,l,l,l,l,l,l
	db l,l,l,l,l,l,l,l,l,l,l,0,0,0,0,0


lower	sub bh,bh
	mov bl,al
	or al,[bx+lows]
	ret

lowq	sub bh,bh
	mov bl,al
	or al,[bx+lows]
	cmpb [bx+lows],l    ;0-32 = carry.
	ret

fucked	call serror
	jmp nocode

stab	dw ?	;Start of line.
getln	mov si,buffp
	movw lprep,0	;Say no previuos find preperation done.
	#if fast
	cmpb fspec,1
	je getnm10	;No loading or anything for FSPEC.
	#endif

	mov ax,bufsize
	add ax,offset(fsend)
	cmp si,ax
	jb getnmr

	;1 - Move 256 bytes to low buffer area.
	;2 - Load BUFSIZE bytes.
	;3 - BUFFP -= BUFSIZE

	mov cx,128
	cld
	mov ax,ds
	mov es,ax
	mov si,bufsize
	add si,offset(fsend)
	mov di,offset(fsend)

	rep movsw

	mov cx,bufsize			;Read bufsize bytes (one sector).
	mov bx,handle
	mov ah,3fh
	lea dx,offset(fsend)+256	;Offset zero in BUFSEG.
	push dx
	int 21h 			;Load bytes.
	pop di
	jc fucked

	add di,ax
	movw [di],1a0dh 	;Set end marker.

	mov ax,bufsize
	sub buffp,ax
	#if fast		;For trace offsets!
	  sub stab,ax
	#endif
	mov si,buffp

getnmr	cmpb [si],10	;Skip over a line feed character.
	jne getnm10
	inc si
getnm10 mov spoint,si
	#if fast
	mov ax,si
	sub ax,stab
	add strace,ax
	adcw strace+2,0
	#endif
	mov stab,si

	#if fast
	cmpb fspec,1	;If special compiling stuff then no listing etc.
	jne gespec
	jmp popg2
	#endif

gespec	cmpb output,0
	je popgend	;No listing if say null.
	cmpb symonly,1
	je popgend	;No source output if symbols only.

	mov ax,line
	mov cl,1
	#if fast
	  call onn_4
	#endif
	#if sofa
	  call onn_5
	#endif
	mov al,' '
	call ona

	mov ax,bp
	call numph	;Print lines address in hex for DEBUG.
	mov al,' '
	call ona

	#if 0
	mov al,'['
	call ona
	mov ax,oplace	;Trace OPLACE.
	call onn
	mov al,']'
	call ona
	mov al,' '
	call ona
	#endif

	call ssend
	call onnl	;Signal start of new line.

popgend incw line	;Update line number.
	incw clineo	;Update total lines compiled so far.
popg2	clc
	ret		;Return to analyse the code.

skip2	mov si,cs:spoint
skip3	call skip	;Skip AND update spoint.
	mov cs:spoint,si
	ret
skip	mov al,cs:[si]
	inc si
	cmp al,';'
	ja skir
	je fdc1
	cmp al,' '
	je skip
	ja skir
	cmp al,9
	je skip
	cmp al,10
	je skip
	cmp al,13
	je fdc1
skir	dec si
	clc
	ret
fdc1	dec si
	stc
	mov al,255	;Return with Carry and AL=255.
sftret	ret

tabnul	db 255

	#if fast
find	lea di,tabtab
findf	push di
	mov si,spoint
	call preskip	;C=error, AL=hash code.
	pop di
	jc sftret
	#endif

	#if sofa
find	mov si,spoint
	call preskip
	jc sftret
	lea di,tabtab
	#endif

	sub ah,ah	;Hash code table.
	add ax,ax
	add di,ax
	mov di,[di]	;Address of somewhat smaller table.
	cmpb [di],255	;TABNUL?
	je fd4e
	mov ax,ds
	mov es,ax
	cld
	mov bx,2
	sub ch,ch
	jmps floop
fentry2 mov bx,2
fentry3 cld
	push di,bx
	call prepfnd
	pop bx,di
	mov ax,cs
	mov es,ax
	mov ch,0	;AH always = 0
	jc sefc
floop	mov cl,[di]
	inc di
	lea si,prestr
	repe cmpsw	;Compare words at a time.
	je matched
nomend	add di,cx
	add di,cx
	add di,bx
	cmpb [di],255
	jne floop
fd4e	sub al,al
sectc	stc		;Carry set.
sefc	ret
matched cmpb [si],13
	jne nomend	;Must be matched fully.

	dec di		;Set DI = one before offset address.
	mov si,skipsi
	mov spoint,si	;Update line buffer pointer.
	clc
	ret

sept	sub bh,bh		;C=separator.
	mov bl,al
	cmpb [bx+bept],1
	ret
septnc	clc
	ret

numph	push ax
	mov al,ah
	call opdigh
	pop ax
opdigh	push ax
	shr al,1
	shr al,1
	shr al,1
	shr al,1
	call odighp
	pop ax
odighp	and al,15
	add al,'0'
	cmp al,'9'
	jbe ona
	add al,7
ona	cmpb symonly,1	;If symbols only but not doing symbols then use norm.
	je onnorm

	cmpb output,2
	ja ondisk
	je onscr
	cmpb output,0
	jne onlpt

onnorm	push dx
	mov dl,al
	mov ah,6
	int 21h
	pop dx
	ret		;Normal.

onlpt	push ax,dx
	sub ah,ah
	sub dx,dx
	int 17h
	pop dx,ax
	ret

ondisk	push di,ax
	mov di,onplace
	cmp di,offset(onend)
	jb onnw
	call onwrite
	lea di,onbuf
onnw	pop ax
	mov [di],al
	inc di
	mov onplace,di
	pop di
onsr	ret

onx	db 0
onscr	cmp al,10
	je onsr
	cmp al,13
	je onsd
	cmp al,9
	jne onsprt
	mov al,onx
	mov cl,al
	and al,248
	add al,8
	sub al,cl
ostab	push ax
	mov al,' '
	call onsprt	;TAB with spaces.
	pop ax
	dec al
	jnz ostab
	ret

sc160	dw 3520
onsd	push di
	movb onx,0
	mov ax,flocpos
	add ax,160
	mov bl,160
	div bl
	mul bl
	cmp ax,4000
	jb ftd13

	mov ah,6
	mov al,onscrl
	push cx,dx
	sub cx,cx
	mov dx,184fh	;Scroll page up one line.
	mov bh,7
	int 10h
	pop dx,cx
	mov ax,sc160	;Start of bottom line.

ftd13	mov flocpos,ax

	mov ah,1
	int 16h
	jz ftdnk
	mov ah,8	;Test break while waiting for key.
	int 21h 	;'Use' key.
	sub ah,ah
	int 16h 	;Wait for nother key to continue.

ftdnk	pop di
	ret

onsprt	push di,es
	mov es,fvideo
	mov di,flocpos
	cmp di,4000
	jb fspok
	push ax,cx,dx
	mov ah,6
	mov al,onscrl
	sub cx,cx
	mov dx,184fh
	mov bh,7
	int 10h
	pop dx,cx,ax
	mov di,sc160
fspok	mov ah,7
	mov es:[di],ax
	inc di
	inc di
	inc onx
	mov flocpos,di
	pop es,di
	ret

onwrite push cx,dx
	mov ah,40h
	lea dx,onbuf
	mov cx,onplace
	sub cx,offset(onbuf)
	mov bx,ohandle
	int 21h 	;Write buffer.
	pop dx,cx
	ret


fvideo	dw ?
flocpos dw ?
onbuf	ds onlen
onend
onplace dw offset(onbuf)

sfrnc	clc
	ret

msgs	dw 0
etype	db ?
comperr lea di,baserr
	mov bx,ds
	mov es,bx
	cld
	lea cx,ersend-baserr
	mov ah,al
	repne scasb
	mov al,[di]
	mov etype,al

	#if sofa
	cmp ah,132	;Warning - but print anyway.
	je cewqy
	cmp al,erwarn
	jne cewqy
	cmpb sfwarn,'Y'
	jne sfrnc	;If no warnings then don't print.
	#endif

cewqy	push di
	call onnl
	cmpb symonly,1
	je askmore	;Always ask if output only symbols.
	mov al,output
	cmp al,1	;Don't wait if disk listing
	jz compnm
	cmp al,3	;no waiting for printer listing either.
	jz compnm
	cmp al,2	;If screen then always wait.
	je askalw
askmore mov ax,msgs
	mov bl,6
	div bl
	cmp ah,5
	jne compnm	;If errors is a multiple of 6 then pause.
askalw	lea dx,morem
	call onm
	mov ah,8	;Wait for key to start next scroll.
	int 21h
	call onnl

compnm	pop di
	inc di
	mov dx,di
	call onm
	incw msgs

	#if fast
	movb [inproc],0 ;turn nested procs off
	#endif

	call onnl	;Signal newline.
	cmpb etype,ernull
	je ceincw

	mov ax,line	;Print line details
	dec ax		;Print line#-1 because LINE reflects next line.
	call onn
	mov al,' '
	call ona	;Seperate line# and data with space.

	mov si,spoint
	mov al,[si]
	push ax
	movb [si],'$'
	mov dx,stab
	call onm

	lea dx,errline
	call onm

	pop ax
	mov si,spoint
	mov [si],al
	call ssend
	call onnl

	mov al,etype
	cmp al,erwarn	;What is to be done?
	je cedone
	cmp al,erexit
	je cenod

ceincw	incw errors

cedone	clc
	ret
cenod	jmp nocode

ssend	mov di,spoint
	mov cx,256
	mov al,13
	cld
	mov bx,ds
	mov es,bx
	repne scasb
	mov buffp,di	;Set new start for next line.

	dec di
	movb [di],'$'   ;Put print end marker.
	push di
	mov dx,spoint
	call onm
	pop di
	movb [di],13
	ret

cpara	db tf,trun
	and bp,0fff0h
	add bp,10h	;Align code to a segment boundary.
	ret

cincl	db tf,trun
	call skip2
	mov mstore,si
	cld
ciilp	mov al,[si]
	cmp al,' '
	je cilok
	cmp al,';'
	je cilok
	cmp al,13
	je cilok
	inc si
	jmps ciilp
cilok	push ax
	movb [si],0	;End char.
	push si 	;Save end of line pointer.

	mov ah,48h
	mov bx,bufsize
	add bx,15+256
	mov cl,4
	shr bx,cl	;Size/16 paragraphs.
	int 21h
	jnc istop
	mov al,lasterr	;Abort.
	jmp comperr
istop	mov es,ax
	mov cx,bufsize
	shr cx,1
	add cx,128
	sub di,di
	mov si,offset(fsend)	;Buffer is at CS:FSEND.
	cld
	rep	movsw

	push ax		;The segment.
	push handle
	push stab
	push buffp
	push line
	#if fast
	push strace
	push strace+2
	mov al,irange
	push ax 	;Save #short/long status.
	#endif

	mov dx,mstore	;File start.
	incb inclf	;Including one more.

	#if fast
	incb t_files
	cmpb t_files,50
	jb tfc
	mov al,153	;Too many files used.
	jmp comperr
tfc	push t_file
	call t_anof	;Add file.
	mov al,t_files
	mov t_file,al	;Put new file number.
	#endif

	call compil2

	#if fast
	pop t_file	;Retrieve file number.
	#endif

	decb inclf	;Finished including one.
	#if fast
	pop ax
	mov irange,al
	pop strace+2
	pop strace
	#endif
	pop line
	pop buffp
	pop stab
	pop handle

	mov cx,bufsize
	shr cx,1
	add cx,128
	sub si,si
	mov di,offset(fsend)	;Buffer is at CS:FSEND.
	pop ds
	mov ax,cs
	mov es,ax		;ES:FSEND - move original buffer back.
	cld
	rep	movsw

	push ds
	pop es		;ES is the old segment.
	mov ds,ax

	mov ah,49h	;Deallocate the buffer.
	int 21h
	pop di
	pop ax
	movb [di],13	;Set end of line.
	mov spoint,di	;Set SPOINT to resume at original program.
	ret


maint	lea dx,mstart
	call msg
	mov ax,bufsize
	call mnump
	and ax,65535-1024
	cmp ax,20480
	jbe bufrnge
	mov ax,20480
bufrnge mov bufsize,ax

	lea dx,mclb
	call msg
	mov ax,maxlab
	call mnump
	mov maxlab,ax

	lea dx,mcls
	call msg
	mov ax,maxsize
	call mnump
	mov maxsize,ax

	lea dx,mcos
	call msg
	mov ax,onesize
	call mnump
	mov onesize,ax

	#if fast
	lea dx,mcpipe
	call msg
	sub ah,ah
	int 16h 	;Waiting for key.
	cmp al,'!'
	je pipexcl
	cmp al,'|'
	jne pipexn
pipexcl mov pipe,al

pipexn	lea dx,mcss
	call msg
	mov ax,stamem
	call mnump
	mov stamem,ax

	lea dx,mcws
	call msg
	mov ax,windmem
	call mnump
	mov windmem,ax
	#endif

	lea dx,mcsss
	call msg
	mov al,onscrl
	sub ah,ah
	call mnump
	cmp al,1
	mov ah,1
	jb pscrl2
	mov ah,24
	cmp al,24
	ja pscrl2

	mov ah,al

pscrl2	mov onscrl,ah

	lea dx,mnes
	call msg
	mov dl,msymbol
	call myes
	mov msymbol,dl

	lea dx,mneu
	call msg
	mov dl,msumm
	call myes
	mov msumm,dl

	lea dx,mnlog
	call msg
	mov dl,logq
	call myes
	mov logq,dl

	#if sofa
	lea dx,mncw
	call msg
	mov dl,sfwarn
	call myes
	mov sfwarn,dl
	#endif

	lea dx,msaving
	call msg
	lea dx,cfg
	mov ah,3ch
	sub cx,cx	;Open the file - FAST.COM
	int 21h
	jc mserr

	mov bx,ax
	mov ah,40h
	mov cx,cfglen
	lea dx,bufsize	;Start of file.
	int 21h
	jc mserr

	mov ah,3eh
	int 21h
	jc mserr
	int 20h 	;Save successful.

mserr	lea dx,mserrm
	call msg
	int 20h

myes	push dx
	mov ah,6
	int 21h 	;Print current status/default.
	lea dx,myesd
	call msg
wyn	mov ah,1
	int 21h
	and al,11011111b	;Upper case.
	mov dl,al
	cmp dl,'Y'
	je wynret
	cmp dl,'N'
	je wynret
	cmp dl,13
	jne wyn
	pop dx		;Restore default.
	ret
wynret	pop ax		;Drop the default.
	ret


intros	db ' by Peter Campbell. /h for help.$'
mstart	db 'Maintenance.',13,10,13,10
	db	 'Text buffer size  [$'
myesd	db '] $'
minpd	db '] = $'
mclb	db 13,10,'Maximum labels     [$'
mcls	db 13,10,'Label space       [$'
mcsss	db 13,10,'Scroll step rate      [$'
mcos	db 13,10,'Undef table space [$'
	#if fast
utopia	dw 0
mcpipe	db 13,10,'Segment:Offset |/!  $'
mcws	db 13,10,'Window space       [$'
mcss	db 13,10,'Stack size         [$'
	#endif

	#if sofa
mncw	db	 13,10,'Print Warnings?     [$'
	#endif
timesec db ' seconds for $'
ctime	dw ?
mipf	db 6,0,0,0,0,0,0,0,0
mnes	db 13,10,13,10,'Print Symbol Table? [$'
mneu	db	 13,10,'Print Memory Usage? [$'
mnlog	db	 13,10,'Create Log File?    [$'
symmsg	db 13,10,'Symbol table (sorted by name):',13,10,'$'
symprt	db '..............',0
mserrm	db 'error, save failed!$'
msaving db 13,10,13,10,'Saving CFG file...$'

mstore	dw 0
stdec	dw 0

mnump	mov mstore,ax
	lea dx,minpd
	call nmsg	;Print stuff before input.
	lea dx,mipf
	mov ah,0ah	;Input number.
	int 21h
	lea di,mipf+2
	sub dx,dx
minlp	mov al,[di]
	cmp al,'0'
	jb minchk
	cmp al,'9'
	ja minchk
	sub al,'0'
	add dx,dx
	mov bx,dx
	add dx,dx
	add dx,dx
	add dx,bx
	sub ah,ah
	add dx,ax
	inc di
	jmps minlp
minchk	cmp dx,0
	jz retndef
	mov ax,dx
	ret
retndef mov ax,mstore
	ret


;Compile a line - get source from disk and set parameters.

compln	mov si,buffp
	cmpb [si],10
	jne complnf
	inc si
complnf mov buffp,si
	cmpb [si],26
	je cppifst
compstc call getln
	jmp cpln2

cppifst	#if fast
	cmpb fspec,1
	jne fstspec	;Special stuff - no libraries yet.
cpejf	jmp compend

fstspec cmpb inclf,0
	jnz cpejf

	movb errstat,0	;ERRORS OFF.
	mov cl,xexit
	call ccwlib	;Call the terminate library.

	mov al,[hflags+xhfrun]
	push ax
	mov cl,xhfrun
	mov al,onelibo
	call hfncsu
	sub bp,2
	movw es:[di+1],runerra	;What the fuck does this do???
	pop ax
	mov [hflags+xhfrun],al
	

;Test all the bits and call the appropriate handler routines.
;(library function system)

	lea si,libline
	mov stab,si	;Pointer to library line.
	mov spoint,si

libmain sub cx,cx	;CH=flag (1 if were some).
	lea di,hflags
libloop push di
	push cx
	cmpb [di],1	;Needed? 1=yes, 2=used, 0=no.
	jne libnext

	incb [di]	;Signal as used.
	pop cx
	mov ch,1
	push cx

;	 mov al,cl
;	 sub ah,ah
;	 call nump	 ;Print the number of the library routine.
;	 mov ah,6
;	 mov dl,'-'
;	 int 21h
;	 pop cx
;	 push cx

	lea di,hfast	;Addresses of LOOP data.
	sub ch,ch
	add cl,cl
	add di,cx	;DI=HFADD+CL*2
	mov di,[di]
	call loop	;Compile the routine into the program.
	jnc libnext
	call comperr	;Display the error.

libnext pop cx
	pop di
	inc di
	inc cl
	cmp cl,libs
	jnz libloop
	cmp ch,0
	jnz libmain


;	*** HANDLE UNDEF TABLE OPERATIONS ***

	mov ax,bp
	test ax,1	;Align variables?
	jz aligned
	inc ax
aligned mov varbase,ax	;Set address of variables.
	mov bx,varadd
	mov varlen,bx
	add ax,bx
	jc mfucked

	mov ss:[wpos],ax	;Set window base.
	mov mfwpos,ax

	cmpb hflags+xwopen,0
	jnz yeswind
	movw windmem,0		;Not used.

yeswind add ax,windmem
	jc mfucked

	mov si,utopia
	mov ss:[si],ax		;Set window top.

	add ax,stamem
	mov totale,ax
	jnc nofwy	;Does the code + data exceed segment bounds?
mfucked mov al,13
	jmp comperr	;Self terminating.

	#endif		;Finished with libraries.


;UNDEF for FAST & SOFA.

nofwy	#if fast	;If FAST then process symbol table.
	mov es,labseg	;Firstly handle LABIV types - ADD VARBASE.
	sub di,di
vsetlp	mov al,ect
	mov cx,128
	cld
	repne scasb	;Find start of 'type'.
	mov al,es:[di]	;Type.
	cmp al,labiv
	jne vsnext
	mov ax,varbase
	add es:[di+1],ax	;Update address for LABIV pointer.
vsnext	add di,9		;was 7, usage
	cmp di,lablast
	jb vsetlp
	#endif

	movw line,1	; -1 = 0
	cmpb inclf,0
	je nofwy2
	jmp compend
nofwy2	sub di,di	;Address first command in undef table.
onepl	cmp di,oplace
	jb overn
	jmp onedne
overn	#if 0
	push di
	mov ax,di
	call onn
	mov al,' '
	call ona
	pop di
	#endif

	mov es,oneseg
	mov al,es:[di]
	#if fast
	    cmp al,oneadd	;Quick bytes for variable offsets.
	    jb dovar
	#endif
	#if sofa
	    cmp al,onesub	;If sofa then check table errors.
	    ja oplerr
	#endif
	mov bx,es:[di+1]

	#if 0
	    push ax,dx,di,bx
	    mov ax,bx
	    call numph
	    call onnl
	    pop bx,di,dx,ax
	#endif

	mov si,es:[di+3]
	add di,5
	mov es,labseg
	#if sofa
	mov addf2,si	;Store pointer to label.
dummy	push ax
	xchg di,si
	mov al,ect
	cld
	mov cx,50
	repne scasb
	dec di		;Point to ECT byte.
	xchg di,si
	pop ax
	#endif
	mov cx,es:[si+2]

	lea si,oneadr
	sub ah,ah
	#if fast
	sub al,oneadd	;0-5 (not 250-255)
	#endif
	add ax,ax
	add si,ax
	call [si]
	jmps onepl

	#if fast
dolib	mov es,oneseg
	mov cl,es:[di-2]
	push di,bx
	call hfuscn
	pop si,di
	add ss:[si],ax
	ret

dotot	mov ax,totale
	mov cl,ss:[bx]	;Special case?
	cmp cl,'f'
	jne nortot
	add ax,15
	shr ax,1
	shr ax,1
	shr ax,1
	shr ax,1
	mov ss:[bx],ax	;don't ADD
	ret
nortot	add ss:[bx],ax
	ret

dovar	mov ah,al	;HIGH,LOW so first is always high.
	mov al,es:[di+1]

	;push ax,di	 ;#if 0
	;call numph
	;call onnl
	;pop di,ax

	mov si,ax
	inc di
	inc di
	mov ax,varbase
	add ss:[si],ax
	jmp onepl
	#endif

doadd	add ss:[bx],cx
	clc
	ret

dosub	sub ss:[bx],cx
	clc
	ret

	#if sofa
oplerr	mov al,130
	jmp comperr	;Exit, fatal error.

dojps	mov ax,cx
	inc bx
	sub ax,bx
	cmp ax,127	;More than 128 bytes either side?
	jg dterg
	cmp ax,-128
	jl dterg
	add ss:[bx-1],al
	ret
dterg	push di
	lea di,prestr	;Set STAB & SPOINT to point at the label
	mov stab,di	;(need to move from LABSEG to data area).
	mov spoint,di
	mov ax,cs
	mov es,ax
	push ds
	mov si,addf2
	mov ds,labseg
	mov cx,bigfind	;Maximum length.
	cld
	rep movsb
	pop ds

	mov al,ect
	mov cx,bigfind
	lea di,prestr
	repne scasb
	movb es:[di],13 ;End marker.

	mov al,17	;Error, label out of range.
	call comperr
	pop di
	ret
	#endif

onedne	mov es,labseg	;Any undefined labels in FAST or SOFA?
	cld
	sub di,di
undso	cmp di,lablast
	jae undsfin
	mov si,di
	mov al,ect
	mov cx,bigfind+2
	repne scasb
	jne undsqq
	cmpb es:[di],udlab
	jb nextls

	mov bx,di
	sub bx,si
	push es,di,ds	;Get name ready for printing.
	mov cx,bigfind/2
	lea di,preso
	mov spoint,di
	mov stab,di
	mov ax,cs
	mov es,ax
	mov ds,labseg
	rep movsw
	lea di,preso
	add di,bx
	movb es:[di-1],13

	mov al,132
	pop ds
	call comperr
	pop di,es

nextls	add di,9    ;was 7, usage, sofa was 5.
	jmps undso

undsfin	clc
	jmp compend

undsqq	mov al,135	;Severe LABEL error.
	jmp comperr

	#if fast
oneadr	dw doadd,dosub,dolib,dotot
	#endif
	#if sofa
oneadr	dw dojps,doadd,dosub
	#endif

cpln2	call doline
	jnc compok	;Compile line and check if successful.
	cmp al,255
	je compok	;Line was blank.

	call comperr
	jmps cmpkj

compok	call skip2
	jc eolok

	#if fast 	;Fast seperates statements with ':'
	cmpb ifuse,1
	je cpln2
	cmpb [si],':'
	jne coxs
	incw spoint
	jmps cpln2	;Another statement.
ifuse	db ?
	#endif

coxs	mov al,7
	call comperr	;Excess ignored.

eolok	mov ax,cs
	mov es,ax
	cld
	mov cx,256
	mov al,13
	mov di,spoint
	repne scasb
	mov buffp,di

cmpkj	jmp compln


;Some common flags and pointers for FAST and SOFA.

filen	db 64,0
	ds 65
runn	ds 65
lablast dw 0
labels	dw 0
errors	dw 0
mxlseg	dw 0
labseg	dw 0	;Segment for labels.
codeseg dw 0	;Segment for code.
handle	dw 0	;File handles (source and include).
buffp	dw 0	;Buffer offset.

symonly db 0	;False, ie everything.

spoint	dw 0
var	db 0

oneseg	dw ?
oplace	dw ?

;== Config Defaults ========================================================

loghand dw 0		;Default .LOG file handle.

default dw 10240
	db 3
	db 'N'
	db 'N'

	#if sofa
	dw 3000
	dw 26000
	dw 14000
	db 'N'
	ds 3
	db 'Y'
cfg	db 'sofa.cfg',0
log	db 'sofa.log',0
logtext db 'SOFA v1.77  dd/mm/yyyy  hh:mm            12345  54321  none ',13,10
	#endif

	#if fast
	dw 1000
	dw 12000
	dw 12000
	dw 1500
	dw 1024
	db 'Y'
	db '|'
cfg	db 'fast.cfg',0
log	db 'fast.log',0
logtext db 'FAST v2.92  dd/mm/yyyy  hh:mm            12345  54321  none ',13,10
	#endif

logtop	db 'Peter Campbell Software: PO box 54-180 Mana, New Zealand.',13,10,13,10
	db 'LOG FILE',13,10
	db '========',13,10,13,10
	db 'Version	Date	    Time   Program   Lines  Code   Errors',13,10
	db '-------------------------------------------------------------',13,10

;== Config Data ============================================================

bufsize dw 10240
onscrl	db 3
msymbol db 'N'
msumm	db 'N'

	#if sofa
maxlab	dw 3000
maxsize dw 26000
onesize dw 14000
sfwarn	db 'N'
	ds 3
	#endif

	#if fast
maxlab	dw 1000
maxsize dw 12000
onesize dw 12000
windmem dw 1500
stamem	dw 512
	#endif

logq	db 'Y'
pipe	db '|'

	ds 10

;===========================================================================

	#if fast
ftsave	db 'Saving FT file (TRACE).',13,10,'$'
	#endif

org	dw 100h 	;Code origin for programs.

errline db ' >>> $'
output	db 0
outlist db 'Source listing - Disk, Screen or Printer? [none] $'
symlist db 'Symbol table - Disk, Screen or Printer? [screen] $'
listt	db 2,0,0,0,0
ldiskn	ds 64
dderrd	db 'Error creating/writing the list file.',13,10,'$'
passm	db ' * $'
passme	db ' ERRORS *$'
serr	db 'Source file not found or error reading it!$'
cwarn	db 'Warning! Did not close source file properly.',13,10,'$'
morem	db 'More ... $'
errm2	db '* Runtime file error *$'
nocm	db 'No code has been output!',13,10,'$'
suc2	db ' bytes.',13,10,'$'
usedm	db ', used = $'

helpm	db 'Help.',13,10,13,10
	#if fast
	db 'FAST [/h] [/?] [/m] filename[;]',13,10,13,10
	db '/h or /? This help message.',13,10
	db '/m	     Maintenance, default settings.',13,10
	db 'filename to compile (.F extension added).',13,10
	db ';	     Suppresses all prompts before compiling.',13,10,13,10,'$'
	#endif
	#if sofa
	db 'SOFA [/h] [/?] [/m] filename[;]',13,10,13,10
	db '/h or /? This help message.',13,10
	db '/m	     Maintenance, default settings.',13,10
	db 'filename to compile (.ASM extension added).',13,10
	db ';	     Suppresses all prompts before compiling.',13,10,13,10,'$'
	#endif

line	dw 0
clineo	dw 0

inclf	db 0
ifpoint dw ?
ifstart ds 30	;IF TRUE (2=none, 1=true, 0=false)
ohandle dw 0

cdelse	db tf,trun
	mov si,ifpoint
	xorb [si],1	;toggle #if flag
	clc
	ret

cifend	db tf,trun
	mov si,ifpoint
	cmpb [si],1
	jne ceferr
	jmp efcont
ceferr	mov al,14	;ENDIF, missing IF.
	stc		;ENDIF is checked by DOLINE.
cifcter ret

ccpif	db tf,trun
	mov si,ifpoint
	cmp si,offset(ifstart)+20
	jae ccp20
	call decimal	;Expression must be defined.
	jc cifcter	;Error in expression.
	or ax,ax
	mov al,0
	jz cefput
	inc al
cefput	incw ifpoint
	mov si,ifpoint
	movb [si],al	;Either 0 or 1.
	clc
	ret	;Return after setting flag.
	;All testing of the flag is done in DOLINE.

ccp20	mov al,134	;Too many nested IF statments.
	stc
	ret

compend #if fast
	cmpb fspec,1
	je retok
	#endif

	mov bx,handle
	mov ah,3eh	;Close file.
	int 21h
	jnc retok
	lea dx,cwarn	;Error closing file, warning.
	call msg
retok	pop ax
	cmp ax,-1
	je retok2
sempty	pop ax
	cmp ax,-1
	jnz sempty	;Remove unwanted items from stack.
	mov al,6
	call comperr
retok2	cmpb inclf,0
	jne retproc	;If including then don't check PROC/REPEAT stack.
	cmpb fspec,1
	je retproc	;If compiling special codes then ignore next things.

	#if fast
	mov di,repnest
	cmp di,offset(rnest)	;Report message for un-ended statement.
	je retok3

	mov cl,[di-1]
	mov al,160
	cmp cl,1
	je retno3
	inc al
	cmp cl,3
	je retno3
	inc al
	cmp cl,4
	je retno3
	inc al
	cmp cl,5
	je retno3
	inc al
	cmp cl,6
	je retno3
	inc al
	cmp cl,10
	je retno3
	inc al
	cmp cl,11
	je retno3
	inc al
	cmp cl,2
	je retno3
	mov al,29	;Unknown reason!
	#endif

	#if sofa
	mov di,pset
	cmp di,offset(pnest)
	mov al,28
	#endif

	je retok3

retno3	call comperr

retok3	mov si,ifpoint
	cmp si,offset(ifstart)	;IF ended?
	jne retpr29
       	cmpb [si],2	;No IFs and last if was ENDIFed.
	je retproc

retpr29 mov al,138
	call comperr

retproc mov al,0	;Not 99 (file not found).
	ret

lpnum8	mov al,wnum
lput	mov [bp],al
	inc bp
	clc
	ret

lpnum	mov ax,wnum
lpute2	mov [bp],ax
	inc bp
	inc bp
	clc
	ret

#if fast
lpnuml	mov ax,wnuml
	jmps lpute2
#endif

fshit	sub al,al	;Stuffed word!!!
	stc
	ret

;Binary search routine.
fblow	dw 0
fbhigh	dw 0	; DX now = FBMID
;JZ = STUFFED WORD.
findbin ;Find word at SPOINT and return details.
	;C=Not found, SI=New pointer for word to be put.
	;NC=found, DI=pointer to ECT at end of label.
	call prepfnd
	mov si,0
	jc fshit	;Not a valid word!!! (never do this).

	mov fblow,0
	sub ch,ch
	mov ax,labels
	mov fbhigh,ax
	mov bl,lplen

fbloop	mov ax,fblow
fbloopl mov dx,fbhigh
	sub dx,ax
	jl fbnotf	;While LOW <= HIGH
	shr dx,1
	add dx,ax	;MID
	cmp dx,labels
	jae fbend

	mov es,mxlseg
	mov di,dx
	add di,di
	mov di,es:[di]	;Get address of word to compare.
	#if sofa
	mov addf2,di	;Set pointer to label name for 'Short jump out of range'
	#endif
	mov es,labseg	;ES:[DI] = word in table.
	lea si,prestr	;DS:[SI] = word to find.	HASH also!!
	mov cl,bl

	repe cmpsb	;Are words the same? (COMPARE WORDS)
	je fbchk

	ja fbsetl

fbseth	mov ax,dx
	dec ax
	mov fbhigh,ax
	jmps fbloop

fbnotf	mov si,fblow
	cmp si,labels
	jbe fbnfr
	dec si
fbnfr	add si,si	;Return C and SI=table pointer.
	mov bl,1
	inc bl		;NZ flag, valid word.
	stc
	ret

fbend	mov si,labels
	jmps fbnfr

fbchk	cmpb es:[di],ect
	jne fbseth
	mov si,skipsi
	mov spoint,si
	clc
	ret
fbsetl	mov ax,dx
	inc ax
	mov fblow,ax
	jmp fbloopl


addf1	dw 0
addf2	dw 0

addfb	mov di,lablast
	mov addf2,di
	mov ax,maxsize	;Maximum table size for labels.
	mov es,labseg
	sub ax,20	;Don't override segment (allow 20 bytes for last label)
	cmp di,ax	;Any table space left?
	jb twloop
	mov al,9	;Say no more space.
	jmp comperr

twloop	sub ch,ch
	cld
	mov cl,lplen
	lea si,prestr
	rep movsb

	mov si,skipsi
	call skip3
	movb es:[di],ect	;End of word.
	mov al,var
	mov es:1[di],al 	;Transferr type.
	mov es:2[di],bp 	;All but constants (EQU) are equal to CODEADD

	;After putting label in table then update sort list.

	mov ax,maxlab
	cmp labels,ax
	jae toomany	;Spare labels?

	push di 	;Return at end to caller.

	mov es,mxlseg
	mov si,addf1
	mov di,si
	add di,2
	mov cx,labels
	add cx,cx
	sub cx,si	;Amount of bytes to move.
	jbe putdet
	add di,cx
	add si,cx
	shr cx,1	;Moving words.
	inc cx

	push ds
	mov ds,mxlseg
	std		;Move down.
	rep movsw

	pop ds
	mov si,addf1
putdet	mov ax,addf2	;Start of LABLAST.
	mov es:[si],ax
	incw labels

	pop di
	mov es,labseg
	movw es:[di+8],0	;set usage counter = 0
	clc
	ret

toomany mov al,lasterr+1
	jmp comperr

symbols	mov al,symonly
	push ax
	mov symonly,0
	call symbout	;Actual output program.
	pop ax
	mov symonly,al
	ret

symbout	lea dx,symmsg
	call onm
	sub ax,ax

symloop cmp ax,labels
	jne symne2
	jmp symend
symne2	push ax
	mov es,mxlseg
	add ax,ax
	mov di,ax
	mov di,es:[di]	;Address within LABSEG.

	mov es,labseg
	mov cl,30

symchar mov al,es:[di]
	cmp al,ect
	je symsufx
	inc di
	jmps symsufa

symsufx mov al,' '
symsufa call ona

	dec cl
	jnz symchar

symgot	inc di
	mov al,es:[di]		;Type.
	mov dx,es:[di+1]	;Get value.
	#if fast
	cmp al,variabl
	je symvad
	cmp al,var32
	je symvad
	cmp al,array
	jne symnva
symvad	cmp dh,250
	jb symvd2		;Don't add varbase, invert it.
	xor dx,0ffffh
	jmps symnva
symvd2	add dx,varbase
symnva	cmp al,10
	jb symnfp
	mov dx,es:[di+3]	;Procedure or function? Different address.
	#endif
symnfp	push dx
	push di
	lea di,symtype
symtlp	cmp al,[di]
	mov dl,[di+1]
	je symtf
	cmpb [di],255
	je symtq
	inc di
	inc di
	jmps symtlp
symtq	mov al,'?'
symtf	mov al,dl
	call ona
	pop di
	pop ax
	call numh	;Print address.

	lea dx,usedm	;", used : nnnn"
	call onm

;	mov es,labseg
	mov ax,es:[di+7]
	call onn

	call onnl
	pop ax
	inc ax
	jmp symloop

symend	call onnl

	mov ax,labels
	lea dx,labnump
	call onnm

	#if fast
	mov ax,varbase
	dec ah
	call numh

	lea dx,mapf1
	call onm
	mov ax,varbase
	call numh
	mov al,' '
	call ona
	mov ax,varlen
	call numh

	lea dx,mapf2
	call onm
	mov ax,mfwpos
	call numh
	mov al,' '
	call ona
	mov ax,windmem
	call numh

	lea dx,mapf3
	call onm
	mov ax,totale
	sub ax,stamem
	call numh
	mov al,' '
	call ona
	mov ax,stamem
	call numh

	call onnl
	#endif

	ret	;Finished with symbols.


labnump db ' labels.',13,10
	#if fast
	db 13,10
	db 'Memory map:  Start Length (hex)',13,10
	db ' Code         0100 '
	#endif
	db '$'

	#if fast
mapf1	db 13,10,' Variables    $'
mapf2	db 13,10,' Windows      $'
mapf3	db 13,10,' Stack        $'
mfwpos	dw ?
	#endif

lsumm	db 13,10
	db 'Memory usage:     Size  Used  Free (hex)',13,10
	db '  Label vectors   $'
lsms	db '  Label table     $'
lsop	db '  Undefind data   $'
	#if fast
lspf	db '  Proc parameters $'
	#endif

numh	push ax
	mov al,ah
	call numhb
	pop ax
numhb	push ax
	mov cl,4
	shr al,cl
	call numhb4
	pop ax
numhb4	and al,15
	add al,'0'
	cmp al,'9'
	jbe jona
	add al,7
jona	jmp ona

onnl	mov al,13
	call ona
	mov al,10
	jmp ona

onnm	push dx
	call onn
	pop dx

onm	mov si,dx
onmlp	mov al,[si]
	cmp al,'$'
	je bpert
	call ona
	inc si
	jmps onmlp

onn	sub cl,cl
onn_5	mov bx,10000
	call pbp
onn_4	mov bx,1000
	call pbp
	mov bx,100
	call pbp
	mov bl,10
	call pbp
	mov bl,1
	inc cl
pbp	sub dx,dx
	div bx
	push dx
	mov dl,al
	add dl,'0'
	or al,cl
	jz pbplead
	mov al,dl
	call ona
	inc cl
pbplead pop ax
bpert	ret


	#if sofa
symtype db 0,'L'        ;Label
	db 1,'N'        ;Near proc
	db 2,'F'        ;Far proc
	db 3,'C'        ;Const
	db 4,'B'        ;Byte pointer
	db 5,'W'        ;Word pointer
	db 255
	#endif

	#if fast
symtype db variabl,'V'
	db label,'L'
	db array,'A'
	db labiv,'I'
	db const32,'3'
	db var32,'Q'
	db functio,'F'
	db 255
	#endif

upper	cmp al,'a'
	jb upno
	cmp al,'z'
	ja upno
	and al,11011111b
upno	ret

fspec	db 0

cdsin	call skip2
	cmpb [si],'?'
	jne cdnqn
	incw spoint	;Skip over ?, valid numeric.
	jmp lput
cdnqn	cmpb [si],39
	mov al,11
	jne cdfsic
	incw spoint
cdsi2	mov si,spoint
	mov ah,[si]
	cmp ah,13
	mov al,11
	je cdfsic
	incw spoint
	cmpb [si+1],'''
	jne cdsks
	cmp ah,'''
	jne cdsks
	movb [bp],'''
	inc bp
	incw spoint
	jmps cdsi2
cdsks	cmp ah,'''
	je cdfsinc
	cmp ah,9
	jne cdstp	;Tab character - how many spaces to LPUT.
	call spctab
	jc cdfsp
	jmps cdsi2
cdstp	mov [bp],ah
	inc bp
	jmps cdsi2
cdfsic	stc
	ret
cdfsinc clc
	mov al,0
cdfsp	ret


lprep	dw 0
prepfnd call skip2
preskip cmp si,lprep	;Same preperation as done before?
	jne prediff
	mov al,lhash
	ret
prediff push cs
	pop es
	cld
	mov al,[si]
	call lowq
	jc cdfsp
	mov dx,bigfind*256
	lea di,prestr
	mov lprep,si
	sub bh,bh
	jmps prentry
preloop mov al,[si]
	mov bl,al
	or al,[bx+lows]
	cmpb [bx+bept],1
	jc prend
prentry stosb
	ror dl,1
	ror dl,1
	ror dl,1
	xor dl,al
	inc si
	dec dh
	jnz preloop
prend	movw [di],0d0dh ;TWO EOL's for CMPSW checks.
	mov skipsi,si	;If skipping over an undefined word.
	mov ah,bigfind
	sub ah,dh
	mov lplen,ah	;Set length of characters put + length byte itself!
	mov al,dl
	mov lhash,al
	clc
	ret
lplen	db ?
lhash	db ?		;MUST BE BEFORE PRESTR
prestr	ds bigfind+2	;Allow for end byte and one more.
skipsi	dw ?
preso	ds bigfind+2

loopz	push ssdi
	call loop	;Save SSDI around LOOP.
	pop ssdi
	ret

loop	mov ssdi,di
loop2	mov di,ssdi
loop3	mov al,[di]
	inc di		;Point to next byte in table.
	cmp al,tf	;Is it the control byte prefix?
	je lpcodes
loopmc	mov [bp],al
	inc bp
	jmps loop3

lpcodes mov al,[di]
	cmp al,maxlps
	ja loopmc

	inc di
	mov ssdi,di	;Skip over control code.

	cmp al,tend
	jne loop0	;Return with (NEW?) stack and carry reset (no errors).
	clc
	ret

loop0	sub ah,ah
	dec al
	lea di,dwlps
	add ax,ax
	add di,ax

	mov bx,offset(loopchk)
	push bx 	;Return address for checking errors.

	jmp [di]

looprun pop bx		;Drop LOOPCHK
	jmp [ssdi]

lpcomma call skip2
	cmp al,','
	jne lpmiss
lpecc	incw spoint
	clc
	ret
lpmiss	mov al,10
	stc
	ret

spps	dw ?
lpps	dw ?
opps	dw ?
bpps	dw ?
ssdi	dw ?
restsp	mov si,spps
	mov spoint,si
;	mov si,lpps
;	mov lablast,si
	mov si,opps
	mov oplace,si
	mov bp,bpps
	clc
	ret

pssp	mov si,spoint
	mov spps,si
;	mov si,lablast
;	mov lpps,si
	mov si,oplace
	mov opps,si
	mov bpps,bp
	clc
	ret


;All error codes.
baserr
	db 0,ercont,'Syntax error.$'
	db 1,ercont,'Number out of range.$'
	db 2,ercont,'Word undefined.$'
	db 3,ercont,'Double definition of word.$'
	db 4,ercont,'Expected a string expression using "string" or ''string''.$'
	db 5,ercont,'Invalid parameter(s).$'
	db 6,ernull,'Internal stack error - recovered ok.$'
	db 7,ercont,'Excess characters in line - ignored.$'
	db 9,erexit,'Out of table space - increase with /M option.$'
	db 10,ercont,'Missing comma.$'
	db 11,ercont,'Missing end of string - " or ''$'
	db 12,erwarn,'Warning... deallocation of memory failed after include.$'
	db 13,erexit,'Out of memory, exceeded the 64K limit.$'
	db 14,ercont,'#ENDIF without #IF.$'
	db 15,ercont,'Divide by zero not allowed.$'


	#if fast
	db 16,ercont,'Missing ),] or }$'
	db 17,ercont,'Limit of 50 nested commands exceeded.$'
	db 18,ercont,'Data types mismatch, conflict between two words or double declaration.$'
	db 19,ercont,'Missing equals ''=''.$'
	db 20,ercont,'Cannot set CS register directly, use CALL SEGMENT|OFFSET$'
	db 21,ercont,'Incorrect syntax, wrong seperator.$'
	db 22,ercont,'Expected a valid file indicator - #n, n=1 to 12$'
	db 24,ercont,'Wrong number of parameters for function or procedure.$'
	db 26,ercont,'Wrong type, expected a variable.$'
	db 27,ercont,'Absolute address expected - Eg SEG|OFF or OFF$'
	db 29,ernull,'End of file: Unknown Reason, check {}.$'
	db 30,ercont,'Missing numeric expression.$'
	db 31,ercont,'Cannot use reserved word as variable or label.$'
	db 128,ercont,'Wrong type, expected a label.$'
	db 129,erexit,'FAST code error, optimisation data tables corrupt.$'
	db 150,ercont,'Too many ERROR jumps (compiler error)!$'
	db 151,ercont,'Statement too long, reduce expressions to variables.$'
	db 152,ercont,'Expected either ON or OFF.$'
	db 153,erexit,'Too many files compiled, max=50.$'
	db 154,erexit,'Too many lines compiled, max=10389.$'
	db 155,ercont,'Code exceeds short jump range.$'
	db 156,ercont,'Invalid #SETDOS format: n.xx$'
	db 160,ernull,'End of file: REPEAT statement(s) not closed.$'
	db 161,ernull,'End of file: WHILE statement(s) not closed.$'
	db 162,ernull,'End of file: IF-THEN-ELSE statement(s) not closed.$'
	db 163,ernull,'End of file: WAIT FOR statement(s) not closed.$'
	db 164,ernull,'End of file: ON GOTO/GOSUB statement(s) not closed.$'
	db 165,ernull,'End of file: PROCEDURE or FUNCTION declaration(s) not finished.$'
	db 166,ernull,'End of file: FOREVER statement(s) not closed or ENDFOR mismatch.$'
	db 167,ernull,'End of file: NEXT is missing from FOR loop.$'
	db 168,ercont,'Nested procedure or function declaration not allowed.$'
	#endif

	#if sofa
	db 16,ercont,'Missing ) or ]$'
	db 17,ercont,'Short jump out of range.$'
	db 18,ercont,'Absolute address expected - Eg SEG:OFF or OFF$'
	db 19,ercont,'Is this a word or byte operation? Specify B or W.$'
	db 20,ercont,'Is this a word or byte operation? Data types conflict.$'
	db 21,ercont,'Illegal immediate value - can''t use memory pointers.$'
	db 22,ercont,'Maximum PROCedure nesting exceeded - limit=32$'
	db 23,ercont,'ENDP without PROC.$'
	db 24,ercont,'Illegal number, can''t use register here.$'
	db 25,erwarn,'Warning! Reg and memory pointer types conflict, using reg.$'
	db 26,erwarn,'Warning! Label is a memory pointer.$'
	db 28,ernull,'Missing ENDP(s).$'
	db 30,ercont,'Too many undefined labels on this line.$'
	#endif

	db 130,erexit,'Severe error in UNDEF TABLE.$'
	db 131,erexit,'Out of memory for UNDEFINED data table - increase with /M option.$'
	db 132,erwarn,'Warning! Undefined label (0 used).$'
	db 133,ercont,'Word defined in way which contradicts its use.$'
	db 134,ercont,'Too many compiler #IF statements, maximum is 20.$'
	db 135,erexit,'Severe error in LABEL TABLE.$'
	db 136,ercont,'Unknown compiler directive.$'
	db 137,ercont,'Bad digit (unrecognisable number).$'
	db 138,ernull,'Missing #ENDIF.$'
	db lasterr,erexit,'No room to include file.$'
	db lasterr+1,erexit,'Too many labels - increase with /M option.$'
ersend
	db erexit,'* Invalid error message *$'

lpcomp	mov di,ssdi
	call skip2
lpcp	mov al,[di]
	inc di
	inc si
	cmp al,[si-1]
	je lpcp
	or al,al
	je cpmatch
	cmp al,'A'      ;Could it possibly be upper case.
	jb nextend	;Typically a seperator - can't be upper case.
	sub bh,bh
	mov bl,[si-1]
	or bl,[bx+lows]
	cmp bl,[di-1]	;Are the two bytes equal?
	je lpcp
	jmps nextend
cpmatch dec si
	mov al,[si]
	call sept
	jnc nextend
	mov ssdi,di
	mov spoint,si
	clc
	ret		;CLC from OR

nextend mov di,ssdi
nxed2	inc di
	cmpb -1[di],tf
	jne nxed2
	cmpb [di],tend	;Error becasue not finished at end of code.
	je neinvl
	cmpb [di],stop	;Skip.
	je nextuse
	cmpb [di],tsept ;Skip.
	jne nxed2
nextuse inc di
	mov ssdi,di
	clc
	ret		;Return to loop where the next instruction will occur.
neinvl	mov al,5	;Invalid parameter.
	stc
	ret

lpstop	pop bx
	clc
	ret

	#if fast
warn8	db '(note: INT 08h will be used while program running for interupts.)'
	db 13,10,'$'
warn9	db '(note: INT 10h will be used while program running for keyboard.)'
	db 13,10,'$'
lastpf	dw 0		;last address of proc/function call (for 'return')
	#endif

spctab	sub cl,cl
	mov di,stab
spctx	cmp di,si
	je spccalc
	mov al,[di]
	inc di
	cmp al,10	;LF is not a character for INC XX
	je spctx
	inc cl
	jz tabse	;EOL before tab?
	cmp al,9
	jne spctx
	dec cl
	and cl,248
	add cl,8
	jmps spctx

tabse	sub al,al
	stc
	ret

spccalc mov al,cl
	and cl,248
	add cl,8
	sub cl,al
	mov ch,cl
spctoop movb [bp],' '
	inc bp
	dec cl
	jnz spctoop
	clc
	ret

prtdxn	push dx
	mov di,dx
prtdxl	mov al,[di]
	call upper
	or al,al
	je prtdend
	call ona
	inc di
	jmps prtdxl
prtdend mov al,' '
	call ona

	cmpb symonly,0
	jne prtdxr
	cmpb output,0
	jz prtdxr
	call onnl

prtdxr	pop dx
	ret

startm	db 'Compiling $'
asmln	db ' lines, writing $'

qubw	mov cx,8
qureg	call skip2
	sub bh,bh		;Seperator?

	mov bl,[si+2]		;is character after 'register' a letter?
	cmpb [bx+bept],1
	jc reg16

	#if sofa
	cmp di,offset(quall)	;sofa? if checking register then maybe 32 BIT?
	jne quc

	mov bl,[si]
	or bl,[bx+lows]
	cmp bl,'e'              ;eax, ebx ...
	jne quc

	lea di,reg32
	call fentry2
	jc quc

	cmpb is32bit,0		;32 bit override?
	jnz done32
	movb is32bit,1		;see test386 (sofa.asm)

done32	mov cl,[di+1]		;register number
	ret

	#endif
	jmps quc

reg16	mov ax,[si]
	cmp al,'a'
	jae qulower

	sub bh,bh
	mov bl,al
	or al,[bx+lows]
	mov bl,ah
	or ah,[bx+lows]

qulower cld
	mov bl,cl
	push cs
	pop es
	repne scasw
	jne quc
	add si,2
	mov spoint,si
	dec bl
	sub bl,cl
	mov cl,bl
	ret
quc	stc	;Not a register, error returns AX as two characters.
	ret

getal	mov si,ssdi
	mov al,[si]	;Get next parameter of a LOOP function.
	incw ssdi
	ret

summary mov al,symonly
	push ax
	mov symonly,0

	lea dx,lsumm
	mov ax,maxlab
	add ax,ax
	mov bx,labels
	add bx,bx
	call sumdump

	lea dx,lsms
	mov ax,maxsize
	mov bx,lablast
	call sumdump

	lea dx,lsop
	mov ax,onesize
	mov bx,oplace
	call sumdump

	#if fast
	lea dx,lspf
	mov ax,endpfm
	mov bx,pfmoff
	call sumdump
	#endif

	call onnl

	pop ax
	mov symonly,al
	ret

sumdump	push ax,bx,bx,ax
	call onm
	pop ax
	call numh
	call twosp
	pop ax
	call numh
	call twosp
	pop bx
	pop ax
	sub ax,bx
	call numh
	jmp onnl

twosp	mov al,' '
	call ona
	mov al,' '
	jmp ona

cds	db tf,trun
	call decimal
	jc cspret
csplps	or ax,ax
	jz cspret

	mov cx,ax
	cld
	mov di,bp
	mov ax,ss
	mov es,ax
	sub al,al
	rep stosb	;Store how many bytes.
	mov bp,di
cspret	ret

d1dx	dw ?
decloop mov d1dx,dx
	sub bh,bh
	sub ah,ah
declp	mov al,[si]
	mov bl,al
	cmpb [bx+bept],1
	jc edec10
	call ifdigit
	jc declc
	mov dx,d1dx	;Reset DX.
	mov di,si
	mov si,stdec
	cmp al,'b'
	je decbin
	cmp al,'B'
	jne dechex	;Hex number.
	jmps decbin	;Binary number.

declc	sub al,'0'
	inc si
	shl dx,1
	mov cx,dx
	shl dx,1
	shl dx,1
	add dx,cx	;DX*=10
	add dx,ax	;Add the digit.
	jmps declp

decbin	mov al,[di+1]
	call sept
	jnc dechex	;If not a seperator then more numbers... (hex)
	sub ah,ah
binlp	mov al,[si]
	cmp al,'0'      ;If a binary digit then return AL=0/1 and carry set.
	jb edecc
	cmp al,'1'
	ja dlb
	sub al,'0'
	shl dx,1
	add dx,ax
	inc si
	jmps binlp

dlb	cmp al,'b'	;Must have B (binary) on end.
	je edechb
	cmp al,'B'
	jne edecc
edechb	inc si
edec10	mov spoint,si	;Skip past the B or H if there is one.
	mov ax,dx
	clc
	ret

edecc	mov al,137	;Invalid digit/syntax error.
	sub ah,ah
yesddd	stc
	ret

dechex	sub bh,bh
	sub ah,ah
dhlp	mov al,[si]
	mov bl,al
	or al,[bx+lows]	;If a hex letter then make it lower case.
	cmp al,'0'
	jb edecc
	cmp al,'9'
	jbe hexyes	;Only return ok if either 0-9 or a-f.
	cmp al,'a'
	jb edecc
	cmp al,'f'
	ja dlh
	sub al,39	;'A'-10-'0'
hexyes	sub al,'0'      ;Make AL between 0-15
	shl dx,1
	shl dx,1
	shl dx,1
	shl dx,1	;Multiply current answer by 16.
	add dx,ax	;Add new digit.
	inc si
	jmps dhlp
dlh	cmp al,'h'
	je edechb
	mov al,137
	mov ah,-1	;Error (specfically no 'h' on end).
	jmps yesddd

ifdigit cmp al,'0'      ;C = digit else NC.
	jb notddd
	cmp al,'9'
	jbe yesddd	;A digit = Carry.
notddd	clc
	ret

fwset	mov ah,15
	int 10h
	mov ah,3
	int 10h 	;Get cursor position.
	add bh,0b8h
	sub bl,bl
	sub ax,ax
	mov es,ax
	cmpb es:[463h],0bah-6
	jnz fsnotm
	sub bh,8
fsnotm	mov fvideo,bx	;Screen video segment.

	mov al,160
	mul dh		;AX=row*160
	sub dh,dh
	add ax,dx
	add ax,dx	;AX=row*160+col*2
	mov flocpos,ax
	ret

fwres	cmpb output,2
	jne fwret

	mov ah,15
	int 10h 	;BH=page
	mov ax,flocpos
	mov bl,160
	div bl
	xchg al,ah
	mov dx,ax
	mov ah,2
	shr dl,1
	int 10h
fwret	ret

logn5	mov bx,10000	;Print AX to memory DI.
	call logd
logn4	mov bx,1000
	call logd
	mov bx,100
	call logd
logn2	mov bx,10
	call logd
	mov bl,1
logd	sub dx,dx
	div bx
	push dx
	add al,'0'
	mov [di],al
	inc di
	pop ax
	ret
