
;==============================================================================
; implementation of Byte Pair Encoding compression
;==============================================================================

;01/03/1994 - write routine

;== definitions ===============================================================

const blocksize=5000	;maximum block size
const hashsize=4096	;size of hash table
const maxchars=200	;character set per block
const threshold=3	;minimum pair count

unsigned buffer,leftcode,rightcode,bleft,cright,count

var size
unsigned a,b

fbuffer ? blocksize

;== functions =================================================================

proc abort(exit_code)
    {
    pop stack1,stack2
    print bios
    print bios "Aborted (";exit_code;") caller=";
    printh bios stack2
    beep
    stop
    }

function allocate_memory
    {
    buffer=allocate blocksize/16+1
    leftcode=allocate 16
    rightcode=allocate 16
    bleft=allocate hashsize/16+1
    cright=allocate hashsize/16+1
    count=allocate hashsize/16+1
    }

function getc
    {
    if key=27 then abort(1)
    r=read #1,1 to fbuffer
    if r then
	{
	return peekb fbuffer
	}
    return -1
    }

function putc(c)
    {
    if key=27 then abort(2)
    pokeb fbuffer,c
;    print bios "w";
    write #2,1 from fbuffer
    }

function fwrite(size)
    {
    if key=27 then abort(3)
    print bios "fwrite ";size;" bytes"
    write #2,size from buffer|0
    }

;==============================================================================

;return index of character pair in hash table
;deleted nodes have count of 1 for hashing

function lookup(a,b)
    {
    index=(a or (b*32)) and (hashsize-1)

    while ((bleft[index]b<>a) or (cright[index]b<>b)) and (count[index]b<>0)
	{
	index=(index+1) and (hashsize-1)
	}

    bleft[index]b=a
    cright[index]b=b
    return index
    }

;==============================================================================

;read next block from input file into buffer

function fileread
    {
    print bios "fileread"
    used=0

    fillb hashsize from count|0 with 0
    for c=0 to 255
	leftcode[c]b=c
	rightcode[c]b=0
    next c

    size=0

    ;read data until full or few unused chars
    read_next:
    c=getc	;-1 = eof

    if (size<blocksize) and (used<maxchars) and (c<>-1) then
	{
	if size>0 then
	    {
	    index=lookup(buffer[size-1]b,c)
	    if count[index]b<255 then count[index]b++
	    }

	size++:buffer[size]b=c

	if not rightcode[c]b then
	    {
	    rightcode[c]b=1
	    used++
	    }

	goto read_next
	}

    if c=-1 then return 1 else return 0
    }

;==============================================================================

;write each pair table and data block to output

function filewrite
    {
    print bios "filewrite"
    c=0

    while c<256
	{
	if c=leftcode[c]b then
	    {
	    len=1:c++
	    while (len<127) and (c<256) and (c=leftcode[c]b)
		{
		len++
		c++
		}
	    putc(len+127):len=0
	    if c=256 then goto break1
	    }
	else
	    {
	    len=0:c++
	    while ((len<127) and (c<256) and (c<>leftcode[c]b)) or ((len<125) and (c<254) and ((c+1)<>leftcode[c+1]b))
		{
		len++:c++
		}
	    putc(len)
	    c-=len+1
	    }

	;write range of pairs
	if len then
	    {
	    for i=0 to len
		putc(leftcode[c]b)
		if c<>leftcode[c]b then putc(rightcode[c]b)
		c++
	    next i
	    }
	}

    break1:
    ;write size bytes and compressed data block
    putc(size/256)
    putc(size and 255)
    fwrite(size)
    }

;==============================================================================

;compress from input file to output file

function compress
    {
    done=0

    ;compress each data block until end of file
    while not done
	{
	done=fileread
	code=256

	;compress this block
	forever
	    {
	    code--
	    while code>=0
		{
		if (code=leftcode[code]b) and (rightcode[code]b=0) then goto break_code
		code--
		}
	    break_code:
	    if code<0 then goto break_forever

	    best=2
	    for index=0 to hashsize-1
		if count[index]b>best then
		    {
		    best=count[index]b
		    leftch=bleft[index]b
		    rightch=cright[index]b
		    }
	    next index
	    if best<threshold then goto break_forever

	    oldsize=size-1
	    w=0
	    if size<2 then goto skip_old_loop
	    for r=0 to oldsize-1
		if (buffer[r]b=leftch) and (buffer[r+1]b=rightch) then
		    {
		    if r>0 then
			{
			index=lookup(buffer[w-1]b,leftch)
			if count[index]b>1 then count[index]b--
			index=lookup(buffer[w-1]b,code)
			if count[index]b<255 then count[index]b++
			}
		    if r<(oldsize-1) then
			{
			index=lookup(rightch,buffer[r+2]b)
			if count[index]b>1 then count[index]b--
			index=lookup(code,buffer[r+2]b)
			if count[index]b<255 then count[index]b++
			}
		    w++:buffer[w]b=code
		    r++:size--
		    }
		else
		    {
		    w++:buffer[w]b=buffer[r]b
		    }

	    next r
	    skip_old_loop:

	    buffer[w]b=buffer[r]b

	    ;add to pair substitution table
	    leftcode[code]b=leftch
	    rightcode[code]b=rightch

	    ;delete pair from hash table
	    index=lookup(leftch,rightch)
	    count[index]b=1
	    }
	break_forever:
	filewrite
	}
    }

;==============================================================================

;main - open input file, create output file

print bios "Compress utility using Byte Pair Encoding."
print bios "v1.00 - Fast version (c) Peter Campbell Software 1994"
print bios

allocate_memory

print bios "open files"
open #1,"tf.f"
create #2,"bpe.tst"

print bios "compress"
compress

print bios "close files"
close #1
close #2

print bios "ok"
stop
