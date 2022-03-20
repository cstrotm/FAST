;Debugger written in FAST.
;Written by Peter Campbell Jan------ 1988.
;Modified March 1991: DC --> DL

;Simple disassembler for FAST programs.

;29/06/91 If lots of zeros convert them to 'ds nnn' (define space)
;	  Print 40 characters of text on one line, not 6, if longer than 6
;	  bytes then only the first 6 are shown.
;


#short

var position,finish,cur_ip,cur_dx
var ip,scrollf,un_data
var get_symbol,symbols,org_ip,byte,dplace
command    ? 25

#include extinput.fi

proc setup
    {
    ext_inpend=0
    cur_ip=100h
    cur_dx=100h

    hex=1
    filter=0

    sys=allocate 4096	;symbol table
    cs=allocate 4096	;code segment

    fill 32768 from sys|0 with 0    ;reset symbol table

    on error
	{
	print bios:error msg "\dos.err":print bios "!"
	stop
	}
    }

proc print_8088(p8_add)
    {
    len8=0
    while peekb p8_add print chr peek p8_add;:p8_add++:len8++
    repeat 7-len8 print " ";
    }

proc print_8088w(p8_add)
    {
    len8=0
    while peekb p8_add print chr peek p8_add;:p8_add++:len8++
    if byte and 1 then print "w"; else print "b";
    if 6-len8 then repeat 6-len8 print " ";
    }

proc skip if peekb position=' ' then position++

function digit
    {
    l=ucase peekb position
    if (l=' ') or (l=':') or (l=13) then return 100
    position++
    lnum=l-'0'
    if lnum>9 then lnum-=7
    if (lnum<0) or (lnum>15) then return 255
    return lnum
    }

function hex_number(default)
    {
    digcnt=0:value=0
    while digcnt<4
	{
	x=digit
	if x=100 then goto check_num
	if x=255 then goto check_num
	value=value*16+x:digcnt++
	}
    check_num:
    if not digcnt then return default
    return value
    }

proc dump_hex(dx)
    {
    printh dx;"  ";
    repeat 8 printhb cs[dx]b;cs[1+dx]b;" ";:dx+=2
    dx-=16
    print " ";
    repeat 16
	{
	p=cs[dx]
	if filter then
	    {
	    p=p and 127
	    if p<32 then p='.'
	    }
	print chr p;:dx++
	}
    }

proc make_symbol(ma)
    {
    if (ma below org_ip) or (ma above finish) then return
    if get_symbol then
	{
	if sys[ma]b then return
	symbols++
	sys[ma]b=1  ;set symbol
	}
    }

proc print_address(pa_num)
    {
    make_symbol(pa_num)
    if (ma below org_ip) or (ma above finish) then
	{
	if pa_num above 9fffh then print "0";
	printh pa_num;"h";
	}
    else printh "L";pa_num;
    }

proc print_w(oo)
    {
    if oo and 1 then print "ax"; else print "al";
    }

proc print_byte(pb_num)
    {
    if hex then
	{
	if pb_num above 9fh then print "0";
	printhb pb_num;"h";
	}
    else printb pb_num;
    }

proc print_word(pw_num)
    {
    if hex then
	{
	print_address(pw_num)
	}
    else
	{
	make_symbol(pw_num)
	if (ma below org_ip) or (ma above finish) then print pw_num;
	    else printh "L";pw_num;
	}
    }

proc print_reg(a,r)
    {
    a+=(r and 7)*2
    print chr peek a;chr peek (a+1);
    }

proc print_rw(rb)
    {
    if rb and 1
	then print_reg(regw,rb)
	else print_reg(regb,rb)
    }

proc print_mrm(mb,bw)
    {
    printh "[";
    m_mod=mb/64
    m_rm=mb and 7
    if (m_mod=0) and (m_rm=6) then
	{
	print_word(cs[ip]):ip+=2
	goto exit_prm
	}
    if m_mod=3 then
	{
	regt=regb:if bw and 1 then regt=regw
	locpos-=2
	print_reg(regt,m_rm)
	return
	}
    msg=(searchb 100 from table_rm for m_rm)+1
    while peekb msg<>255 print chr peek msg;:msg++
    if m_mod=2 then print "+";:print_word(cs[ip]):ip+=2:goto exit_prm
    if m_mod=1 then
	{
	disp=cs[ip]b:ip++
	if disp<128
	    then print "+";:print_byte(disp)
	    else print "-";:print_byte(256-disp)
	}
    exit_prm:
    print "]";
    }

function unassemble_line(ip)
    {
    byte=cs[ip]b
    #long
    if un_data=2 then
	{
	#short
	print "db     '";
	c=40
	while (c>0) and (cs[ip]b<>0)
	    {
	    print chr cs[ip]b;:ip++
	    c--
	    }
	print "'";
	if cs[ip]b=0 then print ",0";:un_data=0:ip++
	goto finish_unasm
	}

    if byte=0 then	;we'll forget addb [],r cos we never really use it?
	{
	dsc=0		;count of zeros
	while cs[ip]b=0 dsc++:ip++
	print "ds     ";dsc;
	goto finish_unasm
	}

    iadd=peek(lookup_codes+byte*2)
    if (iadd>=first_message) and (iadd<=last_message) then
	{
	print_8088(iadd)
	ip++
	goto finish_unasm
	}
    if not iadd then
	{
	goto null_not

	null_continue:
	pop drop_address

	null_not:
	print_8088(i_db)
	print_byte(byte):ip++
	goto finish_unasm
	}

    call iadd

    finish_unasm:
    return ip
    }

function unassemble(ip)
    {
    if not get_symbol then
	{
	start_ip=ip
	printh ip;" ";
	start_loc=locpos
	locpos+=15*2

	if sys[ip]b then printh "L";ip;"  ";
	    else print "       ";
	}

    ip=unassemble_line(ip)

    #long
    if not get_symbol then
	{
	#short
	l1=locpos
	locpos=start_loc
	ll=ip-start_ip
	if ll>6 then ll=6
	repeat ll printhb cs[start_ip]b;:start_ip++
	if 6-ll then repeat 6-ll print "  ";
	if scrollf then print else locpos=4000-160
	}
    else locpos=4000-160

    return ip
    }

proc load_file	    ;calls reset_regs from here.
    {
    n=82h
    e=searchb 40 from 81h for 13
    fill 20 from e with 0	;make asciiz
    #long
    if peekb 81h=0 then
	{
	#short
	print "Program name [.com] ";
	ext_clean_input(filename)
	print
	if peekb (filename+2)=0 then return
	n=filename+2
	}
    f=searchb 40 from n for '.'
    if not f then
	{
	f=searchb 40 from n for 0
	pokeb f,'.'
	pokeb f+1,'c'
	pokeb f+2,'o'
	pokeb f+3,'m'
	pokeb f+4,0
	}
    open_name=n
    open #1,n
    finish=100h+read #1,65000 to cs|100h
    close #1
    printh "Start 0100  End ";finish
    finend=finish

    org_ip=cur_ip
    symbols=0:scrollf=0
    get_symbol=1
    ol=locpos
    un_data=0
    while cur_ip below finish
	{
	cur_ip=unassemble(cur_ip)
	}
    fill 80 from video|ol with 0720h
    cur_ip=100h
    print "Symbols: ";symbols
    }

start:
setup
curtoloc
load_file     ;load from command line or get from input.

main:
print ">";
l=ext_clean_string(command,20)
print

if l=1 then finish=0:gosub unasm_func:goto main

c1=lcase peekb command

if c1='?' then
    {
    address=help_text
    while peekb address
	{
	if peekb address=13 then print
	    else print chr peek address;
	address++
	}
    goto main
    }

f=searchb 3 from key_table for c1
position=command+1  ;current scan position, used for evaluation.
if f then
    {
    f-=key_table
    call peek (key_addr+f*2)
    }
goto main

filename:
string 40

regw:
datab 'axcxdxbxspbpsidi'

regb:
datab 'alcldlblahchdhbh'

seg:
datab 'escsssds'

group1:
data i_test
data i_null
data i_not
data i_neg
data i_mul
data i_imul
data i_div
data i_idiv

group2:
data i_inc
data i_dec
data i_call
data i_callf
data i_jump
data i_jmpf
data i_push
data i_null

shift:
data i_rol
data i_ror
data i_rcl
data i_rcr
data i_shl
data i_shr
data i_null
data i_sar

immed:
data i_add
data i_or
data i_adc
data i_sbb
data i_and
data i_sub
data i_xor
data i_cmp

table_rm:
datab 0,'bx+si',255
datab 1,'bx+di',255
datab 2,'bp+si',255
datab 3,'bp+di',255
datab 4,'si',255
datab 5,'di',255
datab 6,'bp',255
datab 7,'bx',255


key_table:
datab 'udq'

key_addr:
data key_unasm,key_dump,key_quit

help_text:
datab 13
datab 'DL v1.01 (DEBUG LISTER) Written by Peter Campbell',13
datab 13
datab 'Dn   - Memory Display',13
datab 'Un n - Unassemble',13
datab 'Q    - quit',13
datab 13,0

key_quit:
stop

key_dump:
cur_dx=hex_number(cur_dx) and 65520
forever
    {
    for l=0 to 23
    locate l,0
    dump_hex(cur_dx)
    cur_dx+=16
    next l
    print
    wait for keypressed
    if scan=1 then return
    }

key_unasm:
cur_ip=hex_number(cur_ip)
skip
finish=hex_number(0)

unasm_func:
cls
locate 0,0

fscr=finish:if finish=0 then finish=finend
get_symbol=0
scrollf=1
lines=23
org_ip=cur_ip
while cur_ip below finish
    {
    cur_ip=unassemble(cur_ip)
    lines--
    if fscr=0 then
	{
	if lines<=0 then finish=100h
	}
    else
	{
	if keypressed then
	    {
	    if scan=1 then return
	    wait for keypressed
	    if scan=1 then return
	    }
	}
    }
return

;disassembly tables.
first_message:

i_xlat: fname 'xlat'
i_ret: fname 'ret'
i_lahf: fname 'lahf'
i_sahf: fname 'sahf'
i_popf: fname 'popf'
i_pushf: fname 'pushf'
i_aaa: fname 'aaa'
i_daa: fname 'daa'
i_aas: fname 'aas'
i_das: fname 'das'
i_cbw: fname 'cbw'
i_cwd: fname 'cwd'
i_movsb: fname 'movsb'
i_movsw: fname 'movsw'
i_stosb: fname 'stosb'
i_stosw: fname 'stosw'
i_lodsb: fname 'lodsb'
i_lodsw: fname 'lodsw'
i_scasb: fname 'scasb'
i_scasw: fname 'scasw'
i_cmpsb: fname 'cmpsb'
i_cmpsw: fname 'cmpsw'
i_repnz: fname 'repnz'
i_repz: fname 'repz'
i_retf: fname 'retf'
i_int3: fname 'int 3'
i_into: fname 'into'
i_iret: fname 'iret'
i_clc: fname 'clc'
i_stc: fname 'stc'
i_cmc: fname 'cmc'
i_nop: fname 'nop'
i_cld: fname 'cld'
i_std: fname 'std'
i_cli: fname 'cli'
i_sti: fname 'sti'
i_hlt: fname 'hlt'
i_wait: fname 'wait'
i_lock: fname 'lock'
i_int: fname 'int'
i_call: fname 'call'
i_jump: fname 'jmp'
i_db: fname 'db'
i_je: fname 'je'
i_jl: fname 'jl'
i_jle: fname 'jle'
i_jb: fname 'jb'
i_jbe: fname 'jbe'
i_jp: fname 'jp'
i_jo: fname 'jo'
i_js: fname 'js'
i_jne: fname 'jne'
i_jnl: fname 'jnl'
i_jg: fname 'jg'
i_jae: fname 'jae'
i_ja: fname 'ja'
i_jpo: fname 'jpo'
i_jno: fname 'jno'
i_jns: fname 'jns'
i_loop: fname 'loop'
i_loopz: fname 'loopz'
i_loopnz: fname 'loopnz'
i_jcxz: fname 'jcxz'
i_push: fname 'push'
i_pop: fname 'pop'
i_xchg: fname 'xchg'
i_inc: fname 'inc'
i_dec: fname 'dec'
i_in: fname 'in'
i_out: fname 'out'
i_adc: fname 'adc'
i_add: fname 'add'
i_sub: fname 'sub'
i_sbb: fname 'sbb'
i_cmp: fname 'cmp'
i_and: fname 'and'
i_test: fname 'test'
i_or: fname 'or'
i_xor: fname 'xor'
i_mov: fname 'mov'
i_jmpf: fname 'jmpf'
i_callf: fname 'callf'
i_jmps: fname 'jmps'
i_lea: fname 'lea'
i_lds: fname 'lds'
i_les: fname 'les'
i_not: fname 'not'
i_neg: fname 'neg'
i_mul: fname 'mul'
i_imul: fname 'imul'
i_div: fname 'div'
i_idiv: fname 'idiv'
i_rol: fname 'rol'
i_ror: fname 'ror'
i_rcl: fname 'rcl'
i_rcr: fname 'rcr'
i_shl: fname 'shl'
i_shr: fname 'shr'
i_sar: fname 'sar'
i_aad: fname 'aad'
i_aam: fname 'aam'

i_null:
fname '*What?*'
i_space:
fname ''

last_message:


lookup_codes:
data r_immed,r_immed,r_immed,r_immed ;0
data r_add,r_add,r_pushs,r_pops ;4
data r_immed,r_immed,r_immed,r_immed ;8
data r_or,r_or,r_pushs,0 ;12
data r_immed,r_immed,r_immed,r_immed ;16
data r_adc,r_adc,r_pushs,r_pops ;20
data r_immed,r_immed,r_immed,r_immed ;24
data r_sbb,r_sbb,r_pushs,r_pops ;28

data r_immed,r_immed,r_immed,r_immed ;32
data r_and,r_and,r_overs,i_daa ;36
data r_immed,r_immed,r_immed,r_immed ;40
data r_sub,r_sub,r_overs,i_das ;44
data r_immed,r_immed,r_immed,r_immed ;48
data r_xor,r_xor,r_overs,i_aaa ;52
data r_immed,r_immed,r_immed,r_immed ;56
data r_cmp,r_cmp,r_overs,i_aas ;60

data r_inc,r_inc,r_inc,r_inc ;64
data r_inc,r_inc,r_inc,r_inc ;68
data r_dec,r_dec,r_dec,r_dec ;72
data r_dec,r_dec,r_dec,r_dec ;76
data r_push,r_push,r_push,r_push ;80
data r_push,r_push,r_push,r_push ;84
data r_pop,r_pop,r_pop,r_pop ;88
data r_pop,r_pop,r_pop,r_pop ;92

data 0,0,0,0 ;96
data 0,0,0,0 ;100
data 0,0,0,0 ;104
data 0,0,0,0 ;108
data r_jo,r_jno,r_jb,r_jae ;112
data r_je,r_jne,r_jbe,r_ja ;116
data r_js,r_jns,r_jp,r_jpo ;120
data r_jl,r_jnl,r_jle,r_jg ;124

data r_immedi,r_immedi,r_immedi,r_immedi ;128
data r_testw,r_testw,r_xchgw,r_xchgw ;132
data r_movr,r_movr,r_movr,r_movr ;136
data r_movst,r_lea,r_movts,r_poprm ;140
data i_nop,r_xchg,r_xchg,r_xchg ;144
data r_xchg,r_xchg,r_xchg,r_xchg ;148
data i_cbw,i_cwd,r_callf,i_wait ;152
data i_pushf,i_popf,i_sahf,i_lahf ;156

data r_movma,r_movma,r_movam,r_movam ;160
data i_movsb,i_movsw,i_cmpsb,i_cmpsw ;164
data r_test,r_test,i_stosb,i_stosw ;168
data i_lodsb,i_lodsw,i_scasb,i_scasw ;172
data r_movir,r_movir,r_movir,r_movir ;176
data r_movir,r_movir,r_movir,r_movir ;180
data r_movir,r_movir,r_movir,r_movir ;184
data r_movir,r_movir,r_movir,r_movir ;188

data 0,0,r_retsp,i_ret ;192
data r_les,r_lds,r_movirm,r_movirm ;196
data 0,0,r_retfsp,i_retf ;200
data i_into,r_int,i_into,i_iret ;204
data r_shift,r_shift,r_shift,r_shift ;208
data r_aam,r_aad,0,i_xlat ;212
data 0,0,0,0 ;216
data 0,0,0,0 ;220

data r_loopnz,r_loopz,r_loop,r_jcxz ;224
data r_inp,r_inp,r_outp,r_outp ;228
data r_call,r_jump,r_jmpf,r_jmps ;232
data r_indx,r_indx,r_outdx,r_outdx ;236
data i_lock,0,i_repnz,i_repz ;240
data i_hlt,i_cmc,r_math,r_math ;244
data i_clc,i_stc,i_cli,i_sti ;248
data i_cld,i_std,r_group2,r_group2 ;252


r_int:
print_8088(i_int)
r_byte:
print_byte(cs[ip+1])
ip+=2
return

r_retsp:
print_8088(i_ret)
r_word:
print_word(cs[ip+1])
ip+=3
return

r_retfsp:
print_8088(i_retf)
goto r_word

r_call:
print_8088(i_call)
rp=1
r_near:
ia=cs[ip+1]:ip+=3
if ia>=0 then ia+=ip else ia=ip-(0-ia)
if rp then
    {
    if cs[ia]b=5eh then un_data=2   ;pop si
    }
print_address(ia)
return

r_jump:
print_8088(i_jump)
rp=0
goto r_near

r_je:
print_8088(i_je)
print_offset:
ofs=cs[ip+1]b
ip+=2
if ofs<128
    then print_address(ip+ofs)
    else print_address(ip-(256-ofs))
return

r_jl:
print_8088(i_jl)
goto print_offset

r_jle:
print_8088(i_jle)
goto print_offset

r_jb:
print_8088(i_jb)
goto print_offset

r_jbe:
print_8088(i_jbe)
goto print_offset

r_jp:
print_8088(i_jp)
goto print_offset

r_jo:
print_8088(i_jo)
goto print_offset

r_js:
print_8088(i_js)
goto print_offset

r_jne:
print_8088(i_jne)
goto print_offset

r_jnl:
print_8088(i_jnl)
goto print_offset

r_jg:
print_8088(i_jg)
goto print_offset

r_jae:
print_8088(i_jae)
goto print_offset

r_ja:
print_8088(i_ja)
goto print_offset

r_jpo:
print_8088(i_jpo)
goto print_offset

r_jmps:
print_8088(i_jmps)
goto print_offset

r_jno:
print_8088(i_jno)
goto print_offset

r_jns:
print_8088(i_jns)
goto print_offset

r_loop:
print_8088(i_loop)
goto print_offset

r_loopz:
print_8088(i_loopz)
goto print_offset

r_loopnz:
print_8088(i_loopnz)
goto print_offset

r_jcxz:
print_8088(i_jcxz)
goto print_offset

r_push:
print_8088(i_push)
print_low_regw:
print_reg(regw,byte)
ip++
return

r_pop:
print_8088(i_pop)
goto print_low_regw

r_xchg:
print_8088(i_xchg)
print "ax,";
goto print_low_regw

r_inc:
print_8088(i_inc)
goto print_low_regw

r_dec:
print_8088(i_dec)
goto print_low_regw

r_pops:
print_8088(i_pop)
print_mid_seg:
print_reg(seg,byte/8)
ip++
return

r_pushs:
print_8088(i_push)
goto print_mid_seg

r_inp:
print_8088(i_in)
print_w(byte)
print ",";
goto r_byte

r_outp:
print_8088(i_out)
gosub r_byte
print ",";
print_w(byte)
return

r_indx:
print_8088(i_in)
print_w(byte)
print ",dx";
ip++
return

r_outdx:
print_8088(i_out)
print "dx,";
print_w(byte)
ip++
return

r_adc:
print_8088(i_adc)
r_wdw1:
print_w(byte)
print ",";
wdw1:
if byte and 1
    then print_word(cs[ip+1]):ip+=3
    else print_byte(cs[ip+1]):ip+=2
return

r_add:
print_8088(i_add)
goto r_wdw1

r_sub:
print_8088(i_sub)
goto r_wdw1

r_sbb:
print_8088(i_sbb)
goto r_wdw1

r_cmp:
print_8088(i_cmp)
goto r_wdw1

r_and:
print_8088(i_and)
goto r_wdw1

r_test:
print_8088(i_test)
goto r_wdw1

r_or:
print_8088(i_or)
goto r_wdw1

r_xor:
print_8088(i_xor)
goto r_wdw1

r_movma:
print_8088(i_mov)
print_w(byte)
print ",[";
print_word(cs[ip+1])
print "]";
ip+=3
return

r_movam:
print_8088(i_mov)
print "[";
print_word(cs[ip+1])
print "],";
print_w(byte)
ip+=3
return

r_callf:
print_8088(i_callf)
print_abs:
printh cs[ip+1];"h:";cs[ip+3];"h";
ip+=5
return

r_jmpf:
print_8088(i_jmpf)
goto print_abs

r_lea:
print_8088(i_lea)
r_mrd:
byte=cs[ip+1]
print_reg(regw,byte/8)
r_mrm:
byte=cs[ip+1]b
print ",";
mrm2:
ip+=2
print_mrm(byte,1)
return

r_lds:
print_8088(i_lds)
goto r_mrd

r_les:
print_8088(i_les)
goto r_mrd

r_xchgw:
print_8088(i_xchg)
regt=regb:if byte and 1 then regt=regw
byte=cs[ip+1]b
print_reg(regt,byte/8)
goto r_mrm

r_math:
seci=cs[ip+1]b
secm=peek(group1+((seci/8) and 7)*2)
if secm=i_null then goto null_continue
print_8088w(secm)
ip+=2
print_mrm(seci,byte)
if secm=i_test then
    {
    print ",";
    print_word(cs[ip])
    ip+=2
    }
return

r_shift:
seci=cs[ip+1]b
secm=peek(shift+((seci/8) and 7)*2)
if secm=i_null then goto null_continue
print_8088w(secm)
ip+=2
print_mrm(seci,byte)
if byte and 2 then print ",cl"; else print ",1";
return

r_group2:
seci=cs[ip+1]b
secm=peek(group2+((seci/8) and 7)*2)
if secm=i_null then goto null_continue
if (secm=i_dec) or (secm=i_inc)
    then print_8088w(secm)
    else print_8088(secm)
ip+=2
print_mrm(seci,byte)
return

r_immed:
print_8088w(peek(immed+2*((byte/8) and 7)))
movr2:
regt=regb:if byte and 1 then regt=regw
regs=cs[ip+1]b
ip+=2
if byte and 2 then
    {
    print_reg(regt,regs/8)
    print ",";
    print_mrm(regs,byte)
    }
else
    {
    print_mrm(regs,byte)
    print ",";
    print_reg(regt,regs/8)
    }
return

r_poprm:
byte=cs[ip+1]b
if byte and 111000b then goto null_continue
print_8088(i_pop)
goto mrm2

r_immedi:
regs=cs[ip+1]b
print_8088w(peek(immed+2*((regs/8) and 7)))
ip+=2
print_mrm(regs,byte)
print ",";
sw=byte and 3
ia=cs[ip]:ip++
if sw=1 then ia=cs[ip-1]:ip++
if sw=3 then if ia>127 then ia=0-(256-ia)
print_word(ia)
return

r_aam:
print_8088(i_aam)
pbaa:
print_byte(cs[ip+1]b)
ip+=2
return

r_aad:
print_8088(i_aad)
goto pbaa

r_testw:
print_8088w(i_test)
regt=regb:if byte and 1 then regt=regw
regs=cs[ip+1]b
ip+=2
print_reg(regt,regs/8)
print ",";
print_mrm(regs,byte)
return

r_movr:
print_8088w(i_mov)
goto movr2

r_movst:
print_8088(i_mov)
byte=cs[ip+1]b
gosub mrm2
print ",";
print_reg(seg,(byte/8) and 3)
return

r_movts:
print_8088(i_mov)
byte=cs[ip+1]b
print_reg(seg,(byte/8) and 3)
print ",";
goto mrm2

r_movirm:
regs=cs[ip+1]b
if regs and 111000b then goto null_continue
print_8088w(i_mov)
ip+=2
print_mrm(regs,byte)
print ",";
if byte and 1
    then print_word(cs[ip]):ip+=2
    else print_byte(cs[ip]b):ip++
return

r_movir:
print_8088(i_mov)
ip++
regt=regb:if byte and 8 then regt=regw
print_reg(regt,byte)
print ",";
if byte and 8
    then print_word(cs[ip]):ip+=2
    else print_byte(cs[ip]b):ip++
return

r_overs:
print_8088(i_space)
ip++
print_reg(seg,(byte/8) and 3)
print ":";
return
