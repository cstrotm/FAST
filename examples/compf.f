
;== Compress .F or .ASM files into .FX/.AX =================================

#inpend=0
#short
unsigned ld,ln,flen,nlen,lm,nn,words1,recs1,olm,users,useg,len
var32 seeka

fastrecs=fast1:fastrecs=(fast2-fastrecs)/10
asmrecs=asm1:asmrecs=(asm2-asmrecs)/10

ds=dta segment:do=dta offset
files=0

proc pn(pm)
    {
    l=15
    while peekb pm print bios chr peekb pm;:pm++:l--
    repeat l print bios " ";
    }

on error
    {
    print bios
    error msg "\dos.err"
    print bios "!":stop
    }
on break error 999

;== Compression routines ===================================================

work ? 30
word ? 30

const userx=2730
const t_user=250,t_over=251,t_equal0=252,t_space4=253,t_extend=254,t_newline=255

proc user_word
    {
    pop drop	;Drop caller.
    if len<4 then return len-1

    u=findbin(word,useg,0,users,24,22)
    if u then
	{
	u--
	pokeb work,t_user
	poke work+1,useg[u*24+22]
	return 3
	}
    else
	{
	if users<userx then
	    {
	    sfind=(find_bin_put-1)*24
	    moveb users*24-sfind from useg|sfind to useg|sfind+24
	    moveb 22 from word to useg|sfind
	    useg[sfind+22]=olm
	    users++
	    }
	return len-1
	}
    }

function make_token ;ld[lm]
    {
    olm=lm
    c=ld[lm]:lm++
    if c=0a0dh then lm++:pokeb work,t_newline:return 1
    if c=303dh then lm++:pokeb work,t_equal0:return 1
    if c=2020h then
	{
	if ld[lm+1]=2020h then lm+=3:pokeb work,t_space4:return 1
	}
    c1=low c
    if c1>127 then pokeb work,t_over:pokeb work+1,c1:return 2

    #long
    if (c1>='a') and (c1<'z') then
	{
    #short
	moveb 23 from ld|lm-1 to work
	fillb 23 from word with ' '
	x=word:len=0:k=work
	repeat 23
	    {
	    xc=peekb k:k++
	    len++
	    if (xc>127) or (peekb (septs+xc)=0) then goto make_check
	    lm++
	    pokeb x,xc:x++
	    }
	lm--
	return 23   ;Unknown word, too long.

	make_check:
	lm--
	if len<=11 then
	    {
	    n=findbin(word,reg cs,words1,recs1,10,10)
	    if n=0 then user_word
	    }
	else user_word
	n--
	if n<122 then
	    {
	    pokeb work,n+128:return 1
	    }
	else
	    {
	    pokeb work,t_extend:pokeb work+1,n:return 2
	    }
	}

    pokeb work,c
    return 1
    }

function undo_token ;ld[lm]
    {
    c=ln[nn]b:nn++
    if c=t_newline then poke work,0a0dh:return 2
    if c=t_equal0 then poke work,303dh:return 2
    if c=t_space4 then fill 2 from work with 2020h:return 4
    if c=t_over then pokeb work,ln[nn]b:nn++:return 1
    #long
    if c=t_user then
	{
    #short
	c=ln[nn]:nn+=2
	moveb 23 from ld|c to work
	len=0
	repeat 23
	    {
	    c=peekb (work+len)
	    if (c>127) or (peekb (septs+c)=0) then return len
	    len++
	    }
	return 23
	}
    if c=t_extend then
	{
	n=ln[nn]b:nn++
	goto ext_common
	}
    if c>127 then
	{
	n=c-128
	ext_common:
	m=words1+10*n
	moveb 10 from m to work
	pokeb work+10,' '
	n=searchb 11 from work for ' '

	return n-work
	}

    pokeb work,c
    return 1
    }

function file_compress
    {
    users=0
    nn=0:lm=0
    while lm<flen
	{
	l=make_token
	moveb l from work to ln|nn
	nn+=l
	}
    return nn
    }

function file_uncompress
    {
    nn=0:lm=0
    while nn<nlen
	{
	l=undo_token
	moveb l from work to ld|lm
	lm+=l
	}
    return lm
    }

;== File management etc. ===================================================

print bios cr lf "COMPRESS FAST & ASSEMBLER FILES (v1.1) by Peter Campbell." cr lf
ld=allocate 4096    ;Load sector.
ln=allocate 4096    ;New sector.
useg=allocate 4096  ;User token segment.

print bios "File? (.F or .ASM for compress, .FX or .AX for uncompress) ";
inputs name
print bios
if peekb (name+2)=0 then error 999
ext=searchb 30 from name+2 for '.'
if ext=0 then error 11

fe=peek (ext+1):f1=lcase low fe:f2=lcase high fe
if f1='f' then fast=1:words1=fast1:recs1=fastrecs
    else if f1='a' then fast=0:words1=asm1:recs1=asmrecs else error 87
if (f2=0) or (f2='s') then compress=1
    else if f2='x' then compress=0 else error 87

moveb 30 from name to dest
m=dest+2
while peekb m
    {
    if (peekb m='*') or (peekb m='?') then pokeb m,'_'
    m++
    }

if compress=0 then goto un_compress

shit1=dest+ext
shit2=name
poke shit1-shit2+2,'x'
create #2,dest+2

find first name+2
goto entry

forever
    {
    #errors off
    find next
    if error then goto dtotal
    #errors on
    entry:
    moveb 17 from ds|do+26 to spec
    open #1,spec+4
    pn(spec+4):print bios

    flen=peek spec
    rn=read #1,flen to ld|0:if rn<>flen then error 13
    nlen=file_compress
    poke spec+2,nlen:write #2,15 from spec+2
    write #2,nlen from ln|0

    close #1:files++
    }

dtotal:
close #2
print bios cr lf "Compressed ";files;" file(s). New file: ";
pn(dest+2):print bios
stop

name:
string 30
dest:
string 30
spec:
space 17

un_compress:
open #2,dest+2
seeka=0

unloop:
seek #2,seeka
rn=read #2,15 to spec:if rn=0 then
    {
    print bios cr lf "UnCompressed ";files;" file(s)."
    close #2
    stop
    }
if rn<>15 then error 13
nlen=peek spec:seeka+=nlen+15
pn(spec+2)
#errors off
open #1,spec+2
#errors on
if error=0 then
    {
    print bios "File exists! Overwrite? (y/n) ";
    wait for keypressed:y=lcase key
    if y=27 then error 999
    if y<>'y' then print bios "no";:goto unnext
    print bios "yes";
    close #1
    }
else if error<>2 then error

create #1,spec+2
files++

rn=read #2,nlen to ln|0:if rn<>nlen then error 13
flen=file_uncompress
write #1,flen from ld|0

unnext:
print bios
close #1
goto unloop

;== Word tables ============================================================

fast1:
datab 'above	 '
datab 'abs	 '
datab 'allocate  '
datab 'and	 '
datab 'asciiz	 '
datab 'at	 '
datab 'beep	 '
datab 'below	 '
datab 'break	 '
datab 'bright	 '
datab 'call	 '
datab 'carry	 '
datab 'cdisk	 '
datab 'change	 '
datab 'clocks	 '
datab 'close	 '
datab 'cls	 '
datab 'color	 '
datab 'colour	 '
datab 'com	 '
datab 'compare	 '
datab 'compareb  '
datab 'const	 '
datab 'const32	 '
datab 'create	 '
datab 'curpos	 '
datab 'cursor	 '
datab 'curtoloc  '
datab 'data	 '
datab 'datab	 '
datab 'deallocate'
datab 'debug	 '
datab 'delete	 '
datab 'digits	 '
datab 'dim	 '
datab 'dimb	 '
datab 'dir	 '
datab 'disable	 '
datab 'dos	 '
datab 'dta	 '
datab 'duration  '
datab 'else	 '
datab 'enable	 '
datab 'endcode	 '
datab 'endfor	 '
datab 'endif	 '
datab 'entry	 '
datab 'error	 '
datab 'errors	 '
datab 'errorv	 '
datab 'execute	 '
datab 'fill	 '
datab 'fillb	 '
datab 'find	 '
datab 'flash	 '
datab 'fname	 '
datab 'for	 '
datab 'forever	 '
datab 'free	 '
datab 'from	 '
datab 'function  '
datab 'getint	 '
datab 'gosub	 '
datab 'goto	 '
datab 'halt	 '
datab 'handle	 '
datab 'handle	 '
datab 'hgraphics '
datab 'high	 '
datab 'hit	 '
datab 'htext	 '
datab 'if	 '
datab 'ihere	 '
datab 'in	 '
datab 'include	 '
datab 'indoso	 '
datab 'indoss	 '
datab 'ink	 '
datab 'inline	 '
datab 'inpend	 '
datab 'input	 '
datab 'inputb	 '
datab 'inputh	 '
datab 'inputhb	 '
datab 'inputs	 '
datab 'int	 '
datab 'iret	 '
datab 'jump	 '
datab 'key	 '
datab 'keypressed'
datab 'keyscan	 '
datab 'lcase	 '
datab 'let	 '
datab 'line	 '
datab 'load	 '
datab 'locate	 '
datab 'locpos	 '
datab 'loctocur  '
datab 'long	 '
datab 'low	 '
datab 'lprint	 '
datab 'lprintb	 '
datab 'lprinth	 '
datab 'lprinthb  '
datab 'make	 '
datab 'menu	 '
datab 'mod	 '
datab 'mode	 '
datab 'modify	 '
datab 'mono	 '
datab 'move	 '
datab 'moveb	 '
datab 'next	 '
datab 'noise	 '
datab 'not	 '
datab 'null	 '
datab 'on	 '
datab 'open	 '
datab 'or	 '
datab 'out	 '
datab 'page	 '
datab 'palette	 '
datab 'paper	 '
datab 'para	 '
datab 'peek	 '
datab 'peekb	 '
datab 'plot	 '
datab 'point	 '
datab 'poke	 '
datab 'pokeb	 '
datab 'pop	 '
datab 'popall	 '
datab 'print	 '
datab 'printb	 '
datab 'printh	 '
datab 'printhb	 '
datab 'printm	 '
datab 'prints	 '
datab 'proc	 '
datab 'procedure '
datab 'psp	 '
datab 'push	 '
datab 'pushall	 '
datab 'randomize '
datab 'read	 '
datab 'readb	 '
datab 'receive	 '
datab 'reclen	 '
datab 'reg	 '
datab 'remove	 '
datab 'rename	 '
datab 'repeat	 '
datab 'reset	 '
datab 'restore	 '
datab 'retf	 '
datab 'return	 '
datab 'rleft	 '
datab 'rleftz	 '
datab 'rnd	 '
datab 'rright	 '
datab 'rrightz	 '
datab 'save	 '
datab 'scan	 '
datab 'scrchr	 '
datab 'screen	 '
datab 'scroll	 '
datab 'search	 '
datab 'searchb	 '
datab 'seek	 '
datab 'select	 '
datab 'send	 '
datab 'setdos	 '
datab 'setint	 '
datab 'shape	 '
datab 'shell	 '
datab 'short	 '
datab 'sound	 '
datab 'space	 '
datab 'sppos	 '
datab 'sprint	 '
datab 'sprintb	 '
datab 'sprinth	 '
datab 'sprinthb  '
datab 'sprite	 '
datab 'stack	 '
datab 'step	 '
datab 'stop	 '
datab 'string	 '
datab 'swap	 '
datab 'terminate '
datab 'test	 '
datab 'then	 '
datab 'timer	 '
datab 'to	 '
datab 'trace	 '
datab 'ucase	 '
datab 'unsigned  '
datab 'usr	 '
datab 'var	 '
datab 'var32	 '
datab 'video	 '
datab 'wait	 '
datab 'while	 '
datab 'window	 '
datab 'with	 '
datab 'write	 '
datab 'xor	 '
fast2:

asm1:
datab 'aaa	 '
datab 'aad	 '
datab 'aam	 '
datab 'aas	 '
datab 'adc	 '
datab 'adcb	 '
datab 'adcw	 '
datab 'add	 '
datab 'addb	 '
datab 'addw	 '
datab 'ah	 '
datab 'al	 '
datab 'and	 '
datab 'andb	 '
datab 'andw	 '
datab 'ax	 '
datab 'bh	 '
datab 'bl	 '
datab 'bp	 '
datab 'bx	 '
datab 'call	 '
datab 'callf	 '
datab 'cbw	 '
datab 'ch	 '
datab 'cl	 '
datab 'clc	 '
datab 'cld	 '
datab 'cli	 '
datab 'cmc	 '
datab 'cmp	 '
datab 'cmpb	 '
datab 'cmpsb	 '
datab 'cmpsw	 '
datab 'cmpw	 '
datab 'cs	 '
datab 'cwd	 '
datab 'cx	 '
datab 'daa	 '
datab 'das	 '
datab 'db	 '
datab 'dec	 '
datab 'decb	 '
datab 'decw	 '
datab 'dh	 '
datab 'di	 '
datab 'div	 '
datab 'divb	 '
datab 'divw	 '
datab 'dl	 '
datab 'ds	 '
datab 'dw	 '
datab 'dx	 '
datab 'endif	 '
datab 'endp	 '
datab 'equ	 '
datab 'es	 '
datab 'esc	 '
datab 'hlt	 '
datab 'idiv	 '
datab 'idivb	 '
datab 'idivw	 '
datab 'if	 '
datab 'imul	 '
datab 'imulb	 '
datab 'imulw	 '
datab 'in	 '
datab 'inc	 '
datab 'incb	 '
datab 'include	 '
datab 'incw	 '
datab 'int	 '
datab 'into	 '
datab 'iret	 '
datab 'ja	 '
datab 'jae	 '
datab 'jb	 '
datab 'jbe	 '
datab 'jc	 '
datab 'jcxz	 '
datab 'je	 '
datab 'jg	 '
datab 'jge	 '
datab 'jl	 '
datab 'jle	 '
datab 'jmp	 '
datab 'jmpf	 '
datab 'jmps	 '
datab 'jna	 '
datab 'jnae	 '
datab 'jnb	 '
datab 'jnbe	 '
datab 'jnc	 '
datab 'jne	 '
datab 'jng	 '
datab 'jnge	 '
datab 'jnl	 '
datab 'jnle	 '
datab 'jno	 '
datab 'jnp	 '
datab 'jns	 '
datab 'jnz	 '
datab 'jo	 '
datab 'jp	 '
datab 'jpe	 '
datab 'js	 '
datab 'jz	 '
datab 'lahf	 '
datab 'lds	 '
datab 'lea	 '
datab 'les	 '
datab 'lock	 '
datab 'lodsb	 '
datab 'lodsw	 '
datab 'loop	 '
datab 'loope	 '
datab 'loopne	 '
datab 'loopnz	 '
datab 'loopz	 '
datab 'mov	 '
datab 'movb	 '
datab 'movsb	 '
datab 'movsw	 '
datab 'movw	 '
datab 'mul	 '
datab 'mulb	 '
datab 'mulw	 '
datab 'neg	 '
datab 'negb	 '
datab 'negw	 '
datab 'nop	 '
datab 'not	 '
datab 'notb	 '
datab 'notw	 '
datab 'offset	 '
datab 'or	 '
datab 'orb	 '
datab 'org	 '
datab 'orw	 '
datab 'out	 '
datab 'para	 '
datab 'pop	 '
datab 'popf	 '
datab 'proc	 '
datab 'push	 '
datab 'pushf	 '
datab 'rcl	 '
datab 'rclb	 '
datab 'rclw	 '
datab 'rcr	 '
datab 'rcrb	 '
datab 'rcrw	 '
datab 'rep	 '
datab 'repe	 '
datab 'repne	 '
datab 'repnz	 '
datab 'repz	 '
datab 'ret	 '
datab 'retf	 '
datab 'rol	 '
datab 'rolb	 '
datab 'rolw	 '
datab 'ror	 '
datab 'rorb	 '
datab 'rorw	 '
datab 'sahf	 '
datab 'sal	 '
datab 'salb	 '
datab 'salw	 '
datab 'sar	 '
datab 'sarb	 '
datab 'sarw	 '
datab 'sbb	 '
datab 'sbbb	 '
datab 'sbbw	 '
datab 'scasb	 '
datab 'scasw	 '
datab 'shl	 '
datab 'shlb	 '
datab 'shlw	 '
datab 'shr	 '
datab 'shrb	 '
datab 'shrw	 '
datab 'si	 '
datab 'sp	 '
datab 'ss	 '
datab 'stc	 '
datab 'std	 '
datab 'sti	 '
datab 'stosb	 '
datab 'stosw	 '
datab 'sub	 '
datab 'subb	 '
datab 'subw	 '
datab 'test	 '
datab 'testb	 '
datab 'testw	 '
datab 'wait	 '
datab 'xchg	 '
datab 'xchgb	 '
datab 'xchgw	 '
datab 'xlat	 '
datab 'xor	 '
datab 'xorb	 '
datab 'xorw	 '
asm2:

septs:
datab 1,1,1,1,1,1,1,1 ;0
datab 1,0,0,1,1,0,1,1 ;8
datab 1,1,1,1,1,1,1,1 ;16
datab 1,1,0,0,1,1,1,1 ;24
datab 0,1,0,0,1,1,1,0 ;32
datab 0,0,0,0,0,0,0,0 ;40
datab 1,1,1,1,1,1,1,1 ;48
datab 1,1,0,0,0,0,0,0 ;56
datab 0,1,1,1,1,1,1,1 ;64
datab 1,1,1,1,1,1,1,1 ;72
datab 1,1,1,1,1,1,1,1 ;80
datab 1,1,1,0,1,0,1,1 ;88
datab 1,1,1,1,1,1,1,1 ;96
datab 1,1,1,1,1,1,1,1 ;104
datab 1,1,1,1,1,1,1,1 ;112
datab 1,1,1,0,0,0,1,1 ;120
