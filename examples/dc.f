;Debugger written in FAST.
;Written by Peter Campbell Jan------ 1988.

;Much faster and more versatile than DEBUG.
;Able to debug DOS interupts. Screen shows more of use, including all registers
;and the next 6 instructions in window format.

const MAX_SYMBOLS=5120,UNASM_SCREEN=24,DISK_LEN=1024
var TX,POSITION,FINISH,WAITING,WAITFKEY,CUR_IP,CUR_CS,CUR_DX
var CS,IP,BOXING,DISK_OUTPUT,PRINTER_OUTPUT,IO_OUT,SCROLLF
var GET_SYMBOL,SYMBOLS,ORG_IP,BYTE,DPLACE
COMMAND    ? 64
PREVIOUS   ? 64
DISK_START ? DISK_LEN
DISK_END   = DISK_START+DISK_LEN
BUFFER	   ? 82
SBUFFER    ? 82

#inpend=0
#errors off

HEX=1
DELAY=0
FILTER=0

proc ememory print bios "Insufficient memory!!!":terminate

LAST_TYPED=0
page 0
screen 3
SYS=allocate MAX_SYMBOLS/8+16:if error then ememory
PRG=allocate 4096:if error then ememory ;64k program. temp

dos 35(3)
BRK_SEG=reg es:BRK_OFF=reg bx
dos 35(1)
INT_SEG=reg es:INT_OFF=reg bx
dos 35(10)
SCR_SEG=reg es:SCR_OFF=reg bx
poke SOFF,SCR_OFF:poke SSEG,SCR_SEG

CUR_REG=0
BOXING=1  ;Initially use a drawn box on the screen.
reg dx=SCRCHK:dos 25(10)


proc WRITE5(WL,WS)
    {
    if (WL+DPLACE)>=DISK_END then
	{
	write #5,DPLACE-(DISK_START) from DISK_START
	DPLACE=DISK_START
	}
    moveb WL from WS to DPLACE
    DPLACE+=WL
    }

proc CLOSE5
    {
    if DPLACE<>DISK_START then
	write #5,DPLACE-(DISK_START) from DISK_START:if error then return
    close #5
    }

proc PRINT_OO(OO) if OO then print "on"; else print "off";

function YESNO(DEF)
    {
    loctocur
    forever
	{
	wait for keypressed
	K=ucase key
	if K='Y' then print chr K:return 1
	if K='N' then print chr K:return 0
	if K=13 then print:return DEF
	}
    }

function END_OF_LINE
    {
    M=BUFFER+79
    while M>=BUFFER
	{
	if peekb M<>' ' then goto END_FOUND
	M--
	}
    END_FOUND:
    return 1+M
    }

function COMP_BUFFER
    {
    EST=END_OF_LINE
    ST=BUFFER:F=SBUFFER
    X=0
    while ST below EST
	{
	B=peekb ST
	if B=' ' then
	    {
	    BL=(X and 248)+8
	    FLAG=1
	    for A=1 to BL-X
	      if peekb (ST+A-1)<>' ' then goto SPPUT
	    next A
	    B=9:ST+=A-2:X=BL-1
	    }
	SPPUT:
	pokeb F,B:F++
	X++:ST++
	}
    poke F,0a0dh
    return F+2-(SBUFFER)
    }

proc OUTPUT_LINE
    {
    if IO_OUT then
    {
    OW=80:if BOXING then OW=68
    if PRINTER_OUTPUT then
	{
	reg dx=0,ax=2:int 17h:PS=high reg ax
	if (PS and 00111001b)<>00010000b then
	    {
	    PRINTER_OUTPUT=0
	    print "Printer not ready!":beep:return
	    }

	OM=4000-160 ; Bottom line.
	repeat OW lprint chr video[OM]b;:OM+=2
	lprint
	}
    if DISK_OUTPUT then
	{
	OM=4000-160 ; Bottom line.
	fill 40 from BUFFER with 2020h
	DO=BUFFER
	repeat OW pokeb DO,video[OM]b:OM+=2:DO++
	WRITE5(COMP_BUFFER,SBUFFER)
	if error then DISK_OUTPUT=0:CLOSE5:print "Disk file error!":beep
	}
    }
    }

proc PRINT_8088(P8_ADD)
    {
    LEN8=0
    while peekb P8_ADD print chr peek P8_ADD;:P8_ADD++:LEN8++
    repeat 7-LEN8 print " ";
    }

proc PRINT_8088W(P8_ADD)
    {
    LEN8=0
    while peekb P8_ADD print chr peek P8_ADD;:P8_ADD++:LEN8++
    if BYTE and 1 then print "W"; else print "B";
    if 6-LEN8 then repeat 6-LEN8 print " ";
    }

proc SKIP while peekb POSITION=' ' POSITION++

function END_LINE
    {
    SKIP
    if peekb POSITION=13 then return 1
    return 0
    }

function DIGIT
    {
    L=peekb POSITION
    if (L=' ') or (L=':') or (L=13) then return 100
    POSITION++
    LNUM=L-'0'
    if LNUM>9 then LNUM-=7
    if (LNUM<0) or (LNUM>15) then return 255
    return LNUM
    }

function HEX_NUMBER(DEFAULT)
    {
    DIGCNT=0:VALUE=0
    while DIGCNT<4
	{
	X=DIGIT
	if X=100 then goto CHECK_NUM
	if X=255 then print "Must be hex notation.":ANY_ERRORS=1:return
	VALUE=VALUE*16+X:DIGCNT++
	}
    CHECK_NUM:
    if not DIGCNT then return DEFAULT
    return VALUE
    }

proc UNASM_CLINE
    {
    X=68-((locpos mod 160)/2)
    if X>0 then repeat X print " ";
    }

function WAIT_KEY
    {
    if WAITFKEY then
    {
    if WAITING then
	{
	wait for keypressed
	KS=keyscan:R=low KS
	if R=13 then WAITING=0
	if R=27 then return 1
	}
    if keypressed then
	{
	KS=keyscan:R=low KS
	if R=27 then return 1
	if R<>13 then WAITING=1
	t2:
	}
    else
	{
	if DELAY then repeat DELAY*6000 {}
	}
    }
    return 0
    }

proc NEW_SEG(NEWS,NEWO)
    {
    SKIP
    if peekb POSITION=':' then print "Segment?":ANY_ERRORS=1:return
    if peekb POSITION=13 then return
    N=HEX_NUMBER(N):if ANY_ERRORS then return
    SKIP
    if (peekb POSITION=13) or (peekb POSITION<>':') then NEWO=N:return
    POSITION++:NEWS=N
    NEWO=HEX_NUMBER(NEWO)
    }

proc DUMP_HEX(DS,DX)
    {
    ENDED=0  ;Say not at last range byte.
    printh DS;":";DX;"  ";
    repeat 8 printhb DS[DX];DS[1+DX];" ";:DX+=2
    DX-=16
    print " ";
    repeat 16
	{
	if DX=FINISH then ENDED=1
	p=DS[DX]
	if FILTER then
	    {
	    p=p and 127
	    if p<32 then p='.'
	    }
	print chr p;:DX++
	}
    }

proc INSERT_CHAR(ASCII)
    {
    moveb 63-TX from COMMAND+TX to COMMAND+TX+1
    pokeb COMMAND+TX,ASCII:TX++
    }

proc REG_BAR(COL)
    {
    locate CUR_REG+1,70
    SCR=CUR_REG*160+160+70*2+1
    repeat 9 video[SCR]b=COL:SCR+=2
    }

proc MAKE_SYMBOL(MA)
    {
    if (MA below ORG_IP) or (MA above FINISH) then return
    if GET_SYMBOL then
	{
	if SYMBOLS then if search SYMBOLS from SYS|1 for MA then return
	SYS[SYMBOLS*2+1]=MA:SYMBOLS++
	}
    }

proc PRINT_ADDRESS(PA_NUM)
    {
    MAKE_SYMBOL(PA_NUM)
    if (MA below ORG_IP) or (MA above FINISH) then
	{
	if PA_NUM above 9fffh then print "0";
	printh PA_NUM;"h";
	}
    else printh "L";PA_NUM;
    }

proc PRINT_W(OO)
    {
    if OO and 1 then print "AX"; else print "AL";
    }

proc PRINT_BYTE(PB_NUM)
    {
    if HEX then
	{
	if PB_NUM above 9fh then print "0";
	printhb PB_NUM;"h";
	}
    else printb PB_NUM;
    }

proc PRINT_WORD(PW_NUM)
    {
    if HEX then
	{
	PRINT_ADDRESS(PW_NUM)
	}
    else
	{
	MAKE_SYMBOL(PW_NUM)
	if (MA below ORG_IP) or (MA above FINISH) then print PW_NUM;
	    else printh "L";PW_NUM;
	}
    }

proc PRINT_REG(A,R)
    {
    A+=(R and 7)*2
    print chr peek A;chr peek (A+1);
    }

proc PRINT_RW(RB)
    {
    if RB and 1
	then PRINT_REG(REGW,RB)
	else PRINT_REG(REGB,RB)
    }

proc PRINT_MRM(MB,BW)
    {
    printh "[";
    M_MOD=MB/64
    M_RM=MB and 7
    if (M_MOD=0) and (M_RM=6) then
	{
	PRINT_WORD(CS[IP]):IP+=2
	goto EXIT_PRM
	}
    if M_MOD=3 then
	{
	REGT=REGB:if BW and 1 then REGT=REGW
	locpos-=2
	PRINT_REG(REGT,M_RM)
	return
	}
    MSG=(searchb 100 from TABLE_RM for M_RM)+1
    while peekb MSG<>255 print chr peek MSG;:MSG++
    if M_MOD=2 then print "+";:PRINT_WORD(CS[IP]):IP+=2:goto EXIT_PRM
    if M_MOD=1 then
	{
	DISP=CS[IP]b:IP++
	if DISP<128
	    then print "+";:PRINT_BYTE(DISP)
	    else print "-";:PRINT_BYTE(256-DISP)
	}
    EXIT_PRM:
    print "]";
    }

function UNASSEMBLE_LINE(CS,IP)
    {
    BYTE=CS[IP]b
    IADD=peek(LOOKUP_CODES+BYTE*2)
    if (IADD>=FIRST_MESSAGE) and (IADD<=LAST_MESSAGE) then
	{
	PRINT_8088(IADD)
	UNASM_CLINE
	IP++
	goto FINISH_UNASM
	}
    if not IADD then
	{
	goto NULL_NOT

	NULL_CONTINUE:
	pop DROP_ADDRESS

	NULL_NOT:
	PRINT_8088(I_DB)
	PRINT_BYTE(BYTE):IP++
	UNASM_CLINE
	goto FINISH_UNASM
	}

    call IADD

    FINISH_UNASM:
    return IP
    }

function UNASSEMBLE(CS,IP)
    {
    if not GET_SYMBOL then
	{
	START_IP=IP
	printh CS":"IP;" ";
	START_LOC=locpos
	locpos+=15*2

	if (SYMBOLS>0) and ((search SYMBOLS from SYS|1 for IP)<>0)
	    then printh "L";IP;"  ";
	    else print "       ";
	}

    IP=UNASSEMBLE_LINE(CS,IP)

    if not GET_SYMBOL then
	{
	L1=locpos
	locpos=START_LOC
	LL=IP-START_IP
	repeat LL printhb CS[START_IP];:START_IP++
	if 6-LL then repeat 6-LL print "  ";
	if SCROLLF then print
	    else locpos=L1:UNASM_CLINE:locpos=4000-160:OUTPUT_LINE
	}
    else locpos=4000-160

    return IP
    }

proc BOXES
    {
    if BOXING and (video[0]b<>201) then
	{
	;Boxing required and not on screen yet.
	colour 7
	scroll 0,0,68,8,0
	scroll 69,0,79,24,0
	locate 0,0
	print chr 201;
	repeat 67 print chr 205;
	print chr 187;
	locate 8,0
	print chr 200;
	repeat 67 print chr 205;
	print chr 188;
	locate 1,0
	repeat 7 print chr 186
	for A=1 to 7:locate A,68:print chr 186;:next A

	locate 0,69
	print chr 201;
	repeat 9 print chr 205;
	print chr 187;
	locate 14,69
	print chr 200;
	repeat 9 print chr 205;
	print chr 188;
	for A=1 to 13
	locate A,69:print chr 186;:locate A,79:print chr 186;
	next A

	colour 14
	locate 15,69
	print chr 201;
	repeat 9 print chr 205;
	print chr 187;
	locate 24,69
	print chr 200;
	repeat 9 print chr 205;
	print chr 188;
	for A=16 to 23
	locate A,69:print chr 186;:locate A,79:print chr 186;
	next A

	colour 15
	restore REG_DISPLAY
	for Y=1 to 13
	    readb A,B
	    locate Y,71:printh chr A;chr B;"=";
	next Y

	locate 16,70:print "F1 Status";
	locate 17,70:print "F2 Unasm";
	locate 18,70:print "F4 Dump";
	locate 23,70:print "F10 Trace";
	}
    if BOXING then
	{
	colour 15
	REGP=REGISTERS
	BOX_START=peek REG_IP

	push SCROLLF
	SYMBOLS=0:GET_SYMBOL=0:SCROLLF=1
	for Y=1 to 13
	locate Y,74:printh peek REGP:REGP+=2
	if Y<8 then
	    {
	    locate Y,2
	    UNASM_CLINE
	    locate Y,2
	    BOX_START=UNASSEMBLE(peek (REGSEGS+2),BOX_START)
	    }
	next Y
	pop SCROLLF
	}
    }

proc RESET_REGS
    {
    poke REGSEGS,PRG
    poke REGSEGS+2,PRG
    poke REGSEGS+4,PRG
    poke REGSEGS+6,PRG
    poke REG_IP,100H
    fill 8 from REGISTERS with 0
    poke REGISTERS+8,-1 ;Stack.
    CUR_IP=100h
    CUR_DX=100h
    DISK_OUTPUT=0
    PRINTER_OUTPUT=0
    SCROLLF=1
    GET_SYMBOL=0
    }

proc LOAD_FILE(FIRST)  ;Calls RESET_REGS from here.
    {
    if FIRST then
	{
	N=82h
	pokeb searchb 127 from 81h for 13,0  ;Make ASCIIZ
	if peekb 81h=0 then FIRST=0
	}
    if not FIRST then
	{
	print "Program name (.COM) ";
	loctocur
	fill 32 from FILENAME+2 with 0
	inputs FILENAME
	if peekb (FILENAME+2)=0 then goto NO_LOAD
	F=searchb peekb (FILENAME+1) from FILENAME+2 for '.'
	if not F then
	    {
	    F=searchb 64 from FILENAME+2 for 0
	    pokeb F,'.'
	    pokeb F+1,'C'
	    pokeb F+2,'O'
	    pokeb F+3,'M'
	    pokeb F+4,0
	    }
	N=FILENAME+2
	print
	}
    load N,PRG|100h
    if error then
	{
	print "Program not found."
	goto START
	}
    LEN=(searchb 64 from N for 0)-N
    locate 0,34-(LEN/2)
    print " ";
    while peekb N print chr ucase peek N;:N++
    print " ";
    NO_LOAD:
    RESET_REGS
    }

proc END_IT_ALL
    {
    reg dx=BRK_OFF,ds=BRK_SEG:dos 25(3)
    reg ds=reg cs
    reg dx=INT_OFF,ds=INT_SEG:dos 25(1)
    reg ds=reg cs
    reg dx=SCR_OFF,ds=SCR_SEG:dos 25(10)
    reg ds=reg cs
    cursor 24,0
    if DISK_OUTPUT then CLOSE5:if error then print "Error closing disk file!":beep
    terminate
    }


START:
BOXES
locate 13,0
LOAD_FILE(1)  ;Load from command line or get from input.


MAIN:
TYPED=0:TX=0:INSERT=0
fill 32 from COMMAND with 2020h


MAIN2:
CUR_CS=peek (REGSEGS+2)
CUR_DS=peek (REGSEGS+6)
BOXES
locate 24,0
print ">";

forever
{
if BOXING then REG_BAR(112)
locate 24,1
colour 7
PRT=64
if TYPED then
    {
    for A=1 to TYPED:print chr(peek (COMMAND-1+A));:PRT--
    next A
    }
cursor 24,tx+1
if PRT then repeat PRT print chr' ';

if MONO then
    {
    if INSERT then cursor size 7,12 else cursor size 11,12
    }
else
    {
    if INSERT then cursor size 4,7 else cursor size 6,7
    }

wait for keypressed
KS=keyscan
SS=high KS
KK=low KS

if BOXING then
    {
    REG_BAR(15)
    if KS=20480 then CUR_REG++
    if KS=18432 then CUR_REG--
    if CUR_REG<0 then CUR_REG=12
    if CUR_REG>12 then CUR_REG=0
    if KS=20011 then
	{
	CH=0:PV=0
	locate CUR_REG+1,74:print "    ";:locpos-=8
	GET_PLUS:
	loctocur
	cursor high curpos,low curpos+CH
	wait for keypressed
	KS=keyscan:K=ucase low KS
	if K=27 then goto MAIN
	if K=13 then
	    {
	    if CH then poke REGISTERS+CUR_REG*2,PV
	    goto MAIN
	    }
	if KS=3592 then CH=0:print "    ";:locpos-=8

	PRT=K
	K-='0'
	if K>9 then K-=7
	if (CH<4) and (K>=0) and (K<16) then
	    {
	    print bios chr PRT;:CH++
	    PV=PV*16+K
	    }
	goto GET_PLUS
	}
    REG_BAR(112)
    }

if ((KS=16384) or (KS=29952)) and (TX<TYPED) then TYPED=TX
if KS=18176 then TX=0
if KS=20224 then TX=TYPED
if (KS=19200) and (TX>0) then TX--
if (KS=19712) and (TX<62) then TX++
if KS=21248
   then {
	if TX<TYPED then TYPED--
	moveb 64-TX from COMMAND+TX+1 to COMMAND+TX
	}
if KS=15616 then move 32 from PREVIOUS to COMMAND:TYPED=LAST_TYPED:TX=TYPED

;Functions.
locate 24,0
if KS=15104 then print:gosub STATUS:goto MAIN2
if KS=15360 then WAITFKEY=0:print:FINISH=CUR_IP+UNASM_SCREEN:gosub UNASM_FUNC:goto MAIN2
if KS=15872 then WAITFKEY=0:print:FINISH=CUR_DX+127:gosub DUMP_FUNC:goto MAIN2

if KS=20992 then INSERT=not INSERT
if (KK=13) and (TYPED>0) then pokeb COMMAND+TYPED,13:goto ENDCOMMAND
if KK=27 then TYPED=0:TX=0
if KS=3592 then
  {
  moveb 63-TX from COMMAND+TX to COMMAND-1+TX
  if (TX<=TYPED) and (TX>0) then TYPED--
  if TX then tx--
  }

if KK>31 then
    {
    if TYPED>=62 then beep
    else
	{
	if INSERT=0 then
	    {
	    pokeb COMMAND+TX,KK
	    TX++
	    if TX>TYPED then TYPED=TX
	    }
	else
	    {
	    INSERT_CHAR(KK)
	    TYPED++
	    }
	}
    }
}
ENDCOMMAND:
move 32 from COMMAND to PREVIOUS
LAST_TYPED=TYPED

ANY_ERRORS=0
cursor 32,0
locate 24,0
for X=COMMAND to COMMAND+63:pokeb X,ucase peekb X:next X
WAITING=0  ;Say not line skip.

print
C1=peekb COMMAND
C2=peekb (COMMAND+1)
C3=peekb (COMMAND+2)
C4=peekb (COMMAND+3)

if C1='?' then
    {
    LINES=1
    ADDRESS=HELP_TEXT
    while peekb ADDRESS
	{
	if peekb ADDRESS=13 then
	    {
	    if BOXING and (LINES=16) then
		{
		wait for keypressed
		LINES=0
		K=key:if K=27 then print:goto GNULL
		}
	    LINES++
	    print
	    }
	else print chr peek ADDRESS;
	ADDRESS++
	}
    goto GNULL
    }

F=searchb 15 from KEY_TABLE for C1
WAITFKEY=1

POSITION=COMMAND+1  ;Current scan position, used for evaluation.
if F then
    {
    F-=KEY_TABLE
    call peek (KEY_ADDR+F*2)
    }
else
    {
    WHAT_KEY:
    print "What?"
    }
GNULL:
goto MAIN

SCRCHK:
pushall
push reg ax
reg ds=reg cs
pop AX
RT=reg cx:RB=reg dx

if AX=0601h then OUTPUT_LINE
if BOXING and (AX=0601h) and (RT=0) and (RB=184fh) then
    {
    popall
    push reg ax
    reg cx=0900h
    reg dx=1844h
    inline 58h
    pushall
    }

popall
inline 0eah
SOFF:
data 0
SSEG:
data 0

FILENAME:
string 64

REG_DISPLAY:
datab 'AX','CX','DX','BX','SP','BP','SI','DI','ES','CS','SS','DS','IP'

REGISTERS:
data 0,0,0,0
data 65535,0,0,0
REGSEGS:
data 0,0,0,0
REG_IP:
data 100h

OUTPUT_FILE:
string 40

DEFAULT_DISK:
fname 'DC.LST'

REGW:
datab 'AXCXDXBXSPBPSIDI'

REGB:
datab 'ALCLDLBLAHCHDHBH'

SEG:
datab 'ESCSSSDS'

GROUP1:
data I_TEST
data I_NULL
data I_NOT
data I_NEG
data I_MUL
data I_IMUL
data I_DIV
data I_IDIV

GROUP2:
data I_INC
data I_DEC
data I_CALL
data I_CALLF
data I_JUMP
data I_JMPF
data I_PUSH
data I_NULL

SHIFT:
data I_ROL
data I_ROR
data I_RCL
data I_RCR
data I_SHL
data I_SHR
data I_NULL
data I_SAR

IMMED:
data I_ADD
data I_OR
data I_ADC
data I_SBB
data I_AND
data I_SUB
data I_XOR
data I_CMP

TABLE_RM:
datab 0,'BX+SI',255
datab 1,'BX+DI',255
datab 2,'BP+SI',255
datab 3,'BP+DI',255
datab 4,'SI',255
datab 5,'DI',255
datab 6,'BP',255
datab 7,'BX',255


KEY_TABLE:
datab 'UGDRIOQTECSFMPV'

KEY_ADDR:
data KEY_UNASM,GNULL,KEY_DUMP,GNULL
data KEY_INPUT,KEY_OUTPUT,KEY_QUIT,GNULL
data KEY_ENTER,KEY_COMP,KEY_SEARCH,GNULL
data KEY_MOVE,GNULL,KEY_VERSION

HELP_TEXT:
datab 13
datab 'DC (DEBUG CONTROL) WRITTEN BY PETER CAMPBELL',13
datab 13
datab 'C    - source count destination (compare blocks of memory)',13
datab 'CLS  - (clear screen, full screen mode)',13
datab 'D    - start finish (dump memory in hex and ascii)',13
datab 'DRAW - (draw box, half screen mode)',13
datab 'E    - start (enter bytes from address)',13
datab 'F    - start repeat string/bytes (fill memory)',13
datab 'G    - breakpoint,=address (go to address)',13
datab 'I    - port (input byte from port)',13
datab 'IO   - (update current I/O settings)',13
datab 'M    - source count destination (move memory)',13
datab 'O    - port byte (output byte to port)',13
datab 'P    - (proceed, execute one instruction)',13
datab 'Q    - (quit DC)',13
datab 'R    - register (set/display register contents)',13
datab 'S    - start count string/bytes (search for bytes)',13
datab 'T    - count (trace instructions)',13
datab 'U    - start finish (unassemble)',13
datab 'V    - (version?)',13
datab 0

KEY_QUIT:
END_IT_ALL

KEY_DUMP:
if (C2='R') and (C3='A') and (C4='W') then
    {
    BOXING=1
    locate 0,0:print " ";
    return
    }
NEW_SEG(CUR_DS,CUR_DX):if ANY_ERRORS then return
CUR_DS=NEWS:CUR_DX=NEWO and 65520

SKIP
FINISH=HEX_NUMBER(65535):if ANY_ERRORS then return

DUMP_FUNC:
NSCR=0:OLD_NSCR=0
forever
{
if not NSCR then
    {
    if OLD_NSCR then print
    DUMP_HEX(CUR_DS,CUR_DX)
    OLD_NSCR=0
    CUR_DX+=16
    print
    if ENDED then return
    }
NSCR=0
if WAIT_KEY then return
if KS=18432 then CUR_DX-=272
if KS=20480 then CUR_DX-=240
if KS=18688 then CUR_DX-=512
if (KS=18432) or (KS=20480) or (KS=20736) or (KS=18688) then
    {
    NSCR=1
    locate 9,0
    for L=1 to 16
	DUMP_HEX(CUR_DS,CUR_DX):CUR_DX+=16
	if L<>16 then print
    next L
    OLD_NSCR=1
    }
}

KEY_COMP:
if (C2='L') and (C3='S') then cls:BOXING=0:return
if END_LINE then print "Compare what?":return
NEW_SEG(CUR_DS,0):if ANY_ERRORS then return
CDS=NEWS:CDX=NEWO
if END_LINE then print "How many?":return
COUNT=HEX_NUMBER(0):if ANY_ERRORS then return
if END_LINE then print "With what?":return
NEW_SEG(CUR_DS,0):if ANY_ERRORS then return
CES=NEWS:CEX=NEWO
DIFFERENCES=0

while COUNT
    {
    F=compareb COUNT at CDS|CDX with CES|CEX
    if (F<>0) or ((CDX=0) and (CDS[CDX]b<>CES[CEX]b)) then
	{
	COUNT-=F-CDX+1
	CEX+=F-CDX:CDX=F
	printh CDS":"CDX" ";
	printhb CDS[CDX]"    ";
	printh CES":"CEX" ";
	printhb CES[CEX]
	DIFFERENCES++:CDX++:CEX++
	if WAIT_KEY then return
	}
    else goto PRINT_DIFF
    }
PRINT_DIFF:
print DIFFERENCES" differences."
return

KEY_INPUT:
if C2='O' then goto KEY_IO
SKIP
PORT=HEX_NUMBER(65535):if ANY_ERRORS then return
if PORT=65535 then print "Port?":return
I=in PORT
printhb I;"H (";
printb I;")"
return

KEY_IO:
IO_OUT=0
print
print "Current output -"
colour 15
if DISK_OUTPUT then
    {
    print "  Disk file: ";
    M=OUTPUT_FILE+2
    while peekb M print chr lcase peek M;:M++
    print
    }
if PRINTER_OUTPUT then print "  Printer."
if SCROLLF then print "  Screen."
colour 7

print cr "Press D to turn disk output ";
PRINT_OO(not DISK_OUTPUT):print ","
print "      P to turn printer output ";
PRINT_OO(not PRINTER_OUTPUT):print ","
print "      S to turn screen output ";
PRINT_OO(not SCROLLF):print ".";:loctocur

wait for keypressed
print cr
K=ucase key
if K='D' then
    {
    DISK_OUTPUT=not DISK_OUTPUT
    if DISK_OUTPUT then
	{
	loctocur
	print bios "Disk file name: [DC.LST] ";
	inputs OUTPUT_FILE
	print
	if not peekb (OUTPUT_FILE+2)
	    then moveb 7 from DEFAULT_DISK to OUTPUT_FILE+2

	create #5,OUTPUT_FILE+2
	if error then print "Can't open file!":beep:DISK_OUTPUT=0
	DPLACE=DISK_START
	}
    else CLOSE5:if error then print "Error closing disk file!":beep
    goto KEY_IO
    }
if K='P' then
    {
    PRINTER_OUTPUT=not PRINTER_OUTPUT
    goto KEY_IO
    }
if K='S' then
    {
    SCROLLF=not SCROLLF
    goto KEY_IO
    }
IO_OUT=1
return

KEY_OUTPUT:
SKIP
PORT=HEX_NUMBER(65535):if ANY_ERRORS then return
if PORT=65535 then print "Port?":return
SKIP
O=HEX_NUMBER(1000):if ANY_ERRORS then return
if O=1000 then print "Out what?":return
out PORT,O
print "Done."
return

KEY_ENTER:
NEW_SEG(CUR_DS,CUR_DX):if ANY_ERRORS then return
forever
{
printh NEWS;":";NEWO;"  ";
printhb NEWS[NEWO];" - ";
PV=0:CH=0:OLDO=NEWO

GET_ENTER:
loctocur
cursor high curpos,low curpos+CH
wait for keypressed
KS=keyscan:K=ucase low KS
if K=27 then print:return
if KS=18432 then NEWO--
if KS=20480 then NEWO++
if K=13 then
    {
    if CH then NEWS[NEWO]b=PV
    NEWO++
    }
if KS=3592 then CH=0:print "  ";:locpos-=4

PRT=K
K-='0'
if K>9 then K-=7
if (CH<2) and (K>=0) and (K<16) then
    {
    print bios chr PRT;:CH++
    PV=PV*16+K
    }
if NEWO=OLDO then goto GET_ENTER
print
}

KEY_MOVE:
if END_LINE then print "Move what?":return
NEW_SEG(CUR_DS,0):if ANY_ERRORS then return
CDS=NEWS:CDX=NEWO
if END_LINE then print "How many?":return
COUNT=HEX_NUMBER(0):if ANY_ERRORS then return
if END_LINE then print "Move where?":return
NEW_SEG(CUR_DS,0):if ANY_ERRORS then return
CES=NEWS:CEX=NEWO

moveb COUNT from CDS|CDX to CES|CEX
print "Moved."
return

KEY_UNASM:
NEW_SEG(CUR_CS,CUR_IP):if ANY_ERRORS then return
CUR_CS=NEWS:CUR_IP=NEWO
SKIP
FINISH=HEX_NUMBER(CUR_IP+UNASM_SCREEN):if ANY_ERRORS then return

UNASM_FUNC:
SYMBOLS=0:OSF=SCROLLF:SCROLLF=0
GET_SYMBOL=1
ORG_IP=CUR_IP
while (CUR_IP below FINISH) and (SYMBOLS<MAX_SYMBOLS)
    {
    locate 24,0:printh chr '-';FINISH-CUR_IP;" ";
    CUR_IP=UNASSEMBLE(CUR_CS,CUR_IP)
    }

locate 24,0:UNASM_CLINE:locate 24,0

GET_SYMBOL=0
SCROLLF=OSF
CUR_IP=ORG_IP
while CUR_IP below FINISH
    {
    CUR_IP=UNASSEMBLE(CUR_CS,CUR_IP)
    if WAIT_KEY then return
    }
return

STATUS:
print "DC Options -"
print "Unassemble in hex? [";chr 'N'+11*HEX;"] ";
HEX=YESNO(HEX)
print "Scroll delay? (0-5) [";chr DELAY+'0'"] ";
loctocur

GET_DELAY:
wait for keypressed
K=key:if K=13 then print:goto NEXT_STAT
if (K<'0') or (K>'5') then goto GET_DELAY
DELAY=K-'0'
print chr K

NEXT_STAT:
print "Filter ascii dump? ["chr FILTER*11+'N'"] ";
FILTER=YESNO(FILTER)
return


KEY_SEARCH:
return

KEY_VERSION:
print cr"DC (DEBUG CONTROL) WRITTEN BY PETER CAMPBELL"
print "Version 1.55 (19/7/88)"cr
return


;Disassembly tables.
FIRST_MESSAGE:

I_XLAT:
fname 'XLAT'
I_RET:
fname 'RET'
I_LAHF:
fname 'LAHF'
I_SAHF:
fname 'SAHF'
I_POPF:
fname 'POPF'
I_PUSHF:
fname 'PUSHF'
I_AAA:
fname 'AAA'
I_DAA:
fname 'DAA'
I_AAS:
fname 'AAS'
I_DAS:
fname 'DAS'
I_CBW:
fname 'CBW'
I_CWD:
fname 'CWD'
I_MOVSB:
fname 'MOVSB'
I_MOVSW:
fname 'MOVSW'
I_STOSB:
fname 'STOSB'
I_STOSW:
fname 'STOSW'
I_LODSB:
fname 'LODSB'
I_LODSW:
fname 'LODSW'
I_SCASB:
fname 'SCASB'
I_SCASW:
fname 'SCASW'
I_CMPSB:
fname 'CMPSB'
I_CMPSW:
fname 'CMPSW'
I_REPNZ:
fname 'REPNZ'
I_REPZ:
fname 'REPZ'
I_RETF:
fname 'RETF'
I_INT3:
fname 'INT 3'
I_INTO:
fname 'INTO'
I_IRET:
fname 'IRET'
I_CLC:
fname 'CLC'
I_STC:
fname 'STC'
I_CMC:
fname 'CMC'
I_NOP:
fname 'NOP'
I_CLD:
fname 'CLD'
I_STD:
fname 'STD'
I_CLI:
fname 'CLI'
I_STI:
fname 'STI'
I_HLT:
fname 'HLT'
I_WAIT:
fname 'WAIT'
I_LOCK:
fname 'LOCK'
I_INT:
fname 'INT'
I_CALL:
fname 'CALL'
I_JUMP:
fname 'JMP'
I_DB:
fname 'DB'
I_JE:
fname 'JE'
I_JL:
fname 'JL'
I_JLE:
fname 'JLE'
I_JB:
fname 'JB'
I_JBE:
fname 'JBE'
I_JP:
fname 'JP'
I_JO:
fname 'JO'
I_JS:
fname 'JS'
I_JNE:
fname 'JNE'
I_JNL:
fname 'JNL'
I_JG:
fname 'JG'
I_JAE:
fname 'JAE'
I_JA:
fname 'JA'
I_JPO:
fname 'JPO'
I_JNO:
fname 'JNO'
I_JNS:
fname 'JNS'
I_LOOP:
fname 'LOOP'
I_LOOPZ:
fname 'LOOPZ'
I_LOOPNZ:
fname 'LOOPNZ'
I_JCXZ:
fname 'JCXZ'
I_PUSH:
fname 'PUSH'
I_POP:
fname 'POP'
I_XCHG:
fname 'XCHG'
I_INC:
fname 'INC'
I_DEC:
fname 'DEC'
I_IN:
fname 'IN'
I_OUT:
fname 'OUT'
I_ADC:
fname 'ADC'
I_ADD:
fname 'ADD'
I_SUB:
fname 'SUB'
I_SBB:
fname 'SBB'
I_CMP:
fname 'CMP'
I_AND:
fname 'AND'
I_TEST:
fname 'TEST'
I_OR:
fname 'OR'
I_XOR:
fname 'XOR'
I_MOV:
fname 'MOV'
I_JMPF:
fname 'JMPF'
I_CALLF:
fname 'CALLF'
I_JMPS:
fname 'JMPS'
I_LEA:
fname 'LEA'
I_LDS:
fname 'LDS'
I_LES:
fname 'LES'
I_NOT:
fname 'NOT'
I_NEG:
fname 'NEG'
I_MUL:
fname 'MUL'
I_IMUL:
fname 'IMUL'
I_DIV:
fname 'DIV'
I_IDIV:
fname 'IDIV'
I_ROL:
fname 'ROL'
I_ROR:
fname 'ROR'
I_RCL:
fname 'RCL'
I_RCR:
fname 'RCR'
I_SHL:
fname 'SHL'
I_SHR:
fname 'SHR'
I_SAR:
fname 'SAR'
I_AAD:
fname 'AAD'
I_AAM:
fname 'AAM'

I_NULL:
fname '*What?*'
I_SPACE:
fname ''

LAST_MESSAGE:


LOOKUP_CODES:
data R_IMMED,R_IMMED,R_IMMED,R_IMMED ;0
data R_ADD,R_ADD,R_PUSHS,R_POPS ;4
data R_IMMED,R_IMMED,R_IMMED,R_IMMED ;8
data R_OR,R_OR,R_PUSHS,0 ;12
data R_IMMED,R_IMMED,R_IMMED,R_IMMED ;16
data R_ADC,R_ADC,R_PUSHS,R_POPS ;20
data R_IMMED,R_IMMED,R_IMMED,R_IMMED ;24
data R_SBB,R_SBB,R_PUSHS,R_POPS ;28

data R_IMMED,R_IMMED,R_IMMED,R_IMMED ;32
data R_AND,R_AND,R_OVERS,I_DAA ;36
data R_IMMED,R_IMMED,R_IMMED,R_IMMED ;40
data R_SUB,R_SUB,R_OVERS,I_DAS ;44
data R_IMMED,R_IMMED,R_IMMED,R_IMMED ;48
data R_XOR,R_XOR,R_OVERS,I_AAA ;52
data R_IMMED,R_IMMED,R_IMMED,R_IMMED ;56
data R_CMP,R_CMP,R_OVERS,I_AAS ;60

data R_INC,R_INC,R_INC,R_INC ;64
data R_INC,R_INC,R_INC,R_INC ;68
data R_DEC,R_DEC,R_DEC,R_DEC ;72
data R_DEC,R_DEC,R_DEC,R_DEC ;76
data R_PUSH,R_PUSH,R_PUSH,R_PUSH ;80
data R_PUSH,R_PUSH,R_PUSH,R_PUSH ;84
data R_POP,R_POP,R_POP,R_POP ;88
data R_POP,R_POP,R_POP,R_POP ;92

data 0,0,0,0 ;96
data 0,0,0,0 ;100
data 0,0,0,0 ;104
data 0,0,0,0 ;108
data R_JO,R_JNO,R_JB,R_JAE ;112
data R_JE,R_JNE,R_JBE,R_JA ;116
data R_JS,R_JNS,R_JP,R_JPO ;120
data R_JL,R_JNL,R_JLE,R_JG ;124

data R_IMMEDI,R_IMMEDI,R_IMMEDI,R_IMMEDI ;128
data R_TESTW,R_TESTW,R_XCHGW,R_XCHGW ;132
data R_MOVR,R_MOVR,R_MOVR,R_MOVR ;136
data R_MOVST,R_LEA,R_MOVTS,R_POPRM ;140
data I_NOP,R_XCHG,R_XCHG,R_XCHG ;144
data R_XCHG,R_XCHG,R_XCHG,R_XCHG ;148
data I_CBW,I_CWD,R_CALLF,I_WAIT ;152
data I_PUSHF,I_POPF,I_SAHF,I_LAHF ;156

data R_MOVMA,R_MOVMA,R_MOVAM,R_MOVAM ;160
data I_MOVSB,I_MOVSW,I_CMPSB,I_CMPSW ;164
data R_TEST,R_TEST,I_STOSB,I_STOSW ;168
data I_LODSB,I_LODSW,I_SCASB,I_SCASW ;172
data R_MOVIR,R_MOVIR,R_MOVIR,R_MOVIR ;176
data R_MOVIR,R_MOVIR,R_MOVIR,R_MOVIR ;180
data R_MOVIR,R_MOVIR,R_MOVIR,R_MOVIR ;184
data R_MOVIR,R_MOVIR,R_MOVIR,R_MOVIR ;188

data 0,0,R_RETSP,I_RET ;192
data R_LES,R_LDS,R_MOVIRM,R_MOVIRM ;196
data 0,0,R_RETFSP,I_RETF ;200
data I_INTO,R_INT,I_INTO,I_IRET ;204
data R_SHIFT,R_SHIFT,R_SHIFT,R_SHIFT ;208
data R_AAM,R_AAD,0,I_XLAT ;212
data 0,0,0,0 ;216
data 0,0,0,0 ;220

data R_LOOPNZ,R_LOOPZ,R_LOOP,R_JCXZ ;224
data R_INP,R_INP,R_OUTP,R_OUTP ;228
data R_CALL,R_JUMP,R_JMPF,R_JMPS ;232
data R_INDX,R_INDX,R_OUTDX,R_OUTDX ;236
data I_LOCK,0,I_REPNZ,I_REPZ ;240
data I_HLT,I_CMC,R_MATH,R_MATH ;244
data I_CLC,I_STC,I_CLI,I_STI ;248
data I_CLD,I_STD,R_GROUP2,R_GROUP2 ;252


R_INT:
PRINT_8088(I_INT)
R_BYTE:
PRINT_BYTE(CS[IP+1])
IP+=2
return

R_RETSP:
PRINT_8088(I_RET)
R_WORD:
PRINT_WORD(CS[IP+1])
IP+=3
return

R_RETFSP:
PRINT_8088(I_RETF)
goto R_WORD

R_CALL:
PRINT_8088(I_CALL)
R_NEAR:
IA=CS[IP+1]:IP+=3
if IA>=0 then IA+=IP else IA=IP-(0-IA)
PRINT_ADDRESS(IA)
return

R_JUMP:
PRINT_8088(I_JUMP)
goto R_NEAR

R_JE:
PRINT_8088(I_JE)
PRINT_OFFSET:
OFS=CS[IP+1]b
IP+=2
if OFS<128
    then PRINT_ADDRESS(IP+OFS)
    else PRINT_ADDRESS(IP-(256-OFS))
return

R_JL:
PRINT_8088(I_JL)
goto PRINT_OFFSET

R_JLE:
PRINT_8088(I_JLE)
goto PRINT_OFFSET

R_JB:
PRINT_8088(I_JB)
goto PRINT_OFFSET

R_JBE:
PRINT_8088(I_JBE)
goto PRINT_OFFSET

R_JP:
PRINT_8088(I_JP)
goto PRINT_OFFSET

R_JO:
PRINT_8088(I_JO)
goto PRINT_OFFSET

R_JS:
PRINT_8088(I_JS)
goto PRINT_OFFSET

R_JNE:
PRINT_8088(I_JNE)
goto PRINT_OFFSET

R_JNL:
PRINT_8088(I_JNL)
goto PRINT_OFFSET

R_JG:
PRINT_8088(I_JG)
goto PRINT_OFFSET

R_JAE:
PRINT_8088(I_JAE)
goto PRINT_OFFSET

R_JA:
PRINT_8088(I_JA)
goto PRINT_OFFSET

R_JPO:
PRINT_8088(I_JPO)
goto PRINT_OFFSET

R_JMPS:
PRINT_8088(I_JMPS)
goto PRINT_OFFSET

R_JNO:
PRINT_8088(I_JNO)
goto PRINT_OFFSET

R_JNS:
PRINT_8088(I_JNS)
goto PRINT_OFFSET

R_LOOP:
PRINT_8088(I_LOOP)
goto PRINT_OFFSET

R_LOOPZ:
PRINT_8088(I_LOOPZ)
goto PRINT_OFFSET

R_LOOPNZ:
PRINT_8088(I_LOOPNZ)
goto PRINT_OFFSET

R_JCXZ:
PRINT_8088(I_JCXZ)
goto PRINT_OFFSET

R_PUSH:
PRINT_8088(I_PUSH)
PRINT_LOW_REGW:
PRINT_REG(REGW,BYTE)
IP++
return

R_POP:
PRINT_8088(I_POP)
goto PRINT_LOW_REGW

R_XCHG:
PRINT_8088(I_XCHG)
print "AX,";
goto PRINT_LOW_REGW

R_INC:
PRINT_8088(I_INC)
goto PRINT_LOW_REGW

R_DEC:
PRINT_8088(I_DEC)
goto PRINT_LOW_REGW

R_POPS:
PRINT_8088(I_POP)
PRINT_MID_SEG:
PRINT_REG(SEG,BYTE/8)
IP++
return

R_PUSHS:
PRINT_8088(I_PUSH)
goto PRINT_MID_SEG

R_INP:
PRINT_8088(I_IN)
PRINT_W(BYTE)
print ",";
goto R_BYTE

R_OUTP:
PRINT_8088(I_OUT)
gosub R_BYTE
print ",";
PRINT_W(BYTE)
return

R_INDX:
PRINT_8088(I_IN)
PRINT_W(BYTE)
print ",DX";
IP++
return

R_OUTDX:
PRINT_8088(I_OUT)
print "DX,";
PRINT_W(BYTE)
IP++
return

R_ADC:
PRINT_8088(I_ADC)
R_WDW1:
PRINT_W(BYTE)
print ",";
WDW1:
if BYTE and 1
    then PRINT_WORD(CS[IP+1]):IP+=3
    else PRINT_BYTE(CS[IP+1]):IP+=2
return

R_ADD:
PRINT_8088(I_ADD)
goto R_WDW1

R_SUB:
PRINT_8088(I_SUB)
goto R_WDW1

R_SBB:
PRINT_8088(I_SBB)
goto R_WDW1

R_CMP:
PRINT_8088(I_CMP)
goto R_WDW1

R_AND:
PRINT_8088(I_AND)
goto R_WDW1

R_TEST:
PRINT_8088(I_TEST)
goto R_WDW1

R_OR:
PRINT_8088(I_OR)
goto R_WDW1

R_XOR:
PRINT_8088(I_XOR)
goto R_WDW1

R_MOVMA:
PRINT_8088(I_MOV)
PRINT_W(BYTE)
print ",[";
PRINT_WORD(CS[IP+1])
print "]";
IP+=3
return

R_MOVAM:
PRINT_8088(I_MOV)
print "[";
PRINT_WORD(CS[IP+1])
print "],";
PRINT_W(BYTE)
IP+=3
return

R_CALLF:
PRINT_8088(I_CALLF)
PRINT_ABS:
printh CS[IP+1];"h:";CS[IP+3];"h";
IP+=5
return

R_JMPF:
PRINT_8088(I_JMPF)
goto PRINT_ABS

R_LEA:
PRINT_8088(I_LEA)
R_MRD:
BYTE=CS[IP+1]b
PRINT_REG(REGW,BYTE/8)
R_MRM:
BYTE=CS[IP+1]b
print ",";
MRM2:
IP+=2
PRINT_MRM(BYTE,1)
return

R_LDS:
PRINT_8088(I_LDS)
goto R_MRD

R_LES:
PRINT_8088(I_LES)
goto R_MRD

R_XCHGW:
PRINT_8088(I_XCHG)
REGT=REGB:if BYTE and 1 then REGT=REGW
BYTE=CS[IP+1]b
PRINT_REG(REGT,BYTE/8)
goto R_MRM

R_MATH:
SECI=CS[IP+1]b
SECM=peek(GROUP1+((SECI/8) and 7)*2)
if SECM=I_NULL then goto NULL_CONTINUE
PRINT_8088W(SECM)
IP+=2
PRINT_MRM(SECI,BYTE)
if SECM=I_TEST then
    {
    print ",";
    PRINT_WORD(CS[IP])
    IP+=2
    }
return

R_SHIFT:
SECI=CS[IP+1]b
SECM=peek(SHIFT+((SECI/8) and 7)*2)
if SECM=I_NULL then goto NULL_CONTINUE
PRINT_8088W(SECM)
IP+=2
PRINT_MRM(SECI,BYTE)
if BYTE and 2 then print ",CL"; else print ",1";
return

R_GROUP2:
SECI=CS[IP+1]b
SECM=peek(GROUP2+((SECI/8) and 7)*2)
if SECM=I_NULL then goto NULL_CONTINUE
if (SECM=I_DEC) or (SECM=I_INC)
    then PRINT_8088W(SECM)
    else PRINT_8088(SECM)
IP+=2
PRINT_MRM(SECI,BYTE)
return

R_IMMED:
PRINT_8088W(peek(IMMED+2*((BYTE/8) and 7)))
MOVR2:
REGT=REGB:if BYTE and 1 then REGT=REGW
REGS=CS[IP+1]b
IP+=2
if BYTE and 2 then
    {
    PRINT_REG(REGT,REGS/8)
    print ",";
    PRINT_MRM(REGS,BYTE)
    }
else
    {
    PRINT_MRM(REGS,BYTE)
    print ",";
    PRINT_REG(REGT,REGS/8)
    }
return

R_POPRM:
BYTE=CS[IP+1]b
if BYTE and 111000b then goto NULL_CONTINUE
PRINT_8088(I_POP)
goto MRM2

R_IMMEDI:
REGS=CS[IP+1]b
PRINT_8088W(peek(IMMED+2*((REGS/8) and 7)))
IP+=2
PRINT_MRM(REGS,BYTE)
print ",";
SW=BYTE and 3
IA=CS[IP]b:IP++
if SW=1 then IA=CS[IP-1]:IP++
if SW=3 then if IA>127 then IA=0-(256-IA)
PRINT_WORD(IA)
return

R_AAM:
PRINT_8088(I_AAM)
PBAA:
PRINT_BYTE(CS[IP+1]b)
IP+=2
return

R_AAD:
PRINT_8088(I_AAD)
goto PBAA

R_TESTW:
PRINT_8088W(I_TEST)
REGT=REGB:if BYTE and 1 then REGT=REGW
REGS=CS[IP+1]b
IP+=2
PRINT_REG(REGT,REGS/8)
print ",";
PRINT_MRM(REGS,BYTE)
return

R_MOVR:
PRINT_8088W(I_MOV)
goto MOVR2

R_MOVST:
PRINT_8088(I_MOV)
BYTE=CS[IP+1]b
gosub MRM2
print ",";
PRINT_REG(SEG,(BYTE/8) and 3)
return

R_MOVTS:
PRINT_8088(I_MOV)
BYTE=CS[IP+1]b
PRINT_REG(SEG,(BYTE/8) and 3)
print ",";
goto MRM2

R_MOVIRM:
REGS=CS[IP+1]b
if REGS and 111000b then goto NULL_CONTINUE
PRINT_8088W(I_MOV)
IP+=2
PRINT_MRM(REGS,BYTE)
print ",";
if BYTE and 1
    then PRINT_WORD(CS[IP]):IP+=2
    else PRINT_BYTE(CS[IP]b):IP++
return

R_MOVIR:
PRINT_8088(I_MOV)
IP++
REGT=REGB:if BYTE and 8 then REGT=REGW
PRINT_REG(REGT,BYTE)
print ",";
if BYTE and 8
    then PRINT_WORD(CS[IP]):IP+=2
    else PRINT_BYTE(CS[IP]b):IP++
return

R_OVERS:
PRINT_8088(I_SPACE)
IP++
PRINT_REG(SEG,(BYTE/8) and 3)
print ":";
return
