
;==============================================================================
; Fast Word - Editing routines
;==============================================================================

#short

function print_buffer
    {
    if width=80 then ml=printm buffer+file_column,80,0
    else ml=printm buffer+file_column,132,0
    }

;==============================================================================

;npa = pointer to 32 bit address, update with backward address

function back_line(npa,nlines)
    {
    back32=peek npa

    while nlines
	{
	if32 back32>=max_width then
	    {
	    pa32=back32-max_width
	    xoff=max_width
	    }
	else
	    {
	    if32 back32=0 then goto back_return
	    pa32=0
	    xoff=low back32
	    }

	xms_get(max_width,work_seg,0,.pa32)

	xoff--:back32--

	if eol(work_seg[xoff]b) then xoff--:back32--
	if xoff<=0 then back32=0:goto back_return

	back_loop:
	b=work_seg[xoff]b

	if eol(b) then
	    {
	    xoff++:back32++
	    nlines--	    ;continue while loop?
	    }
	else
	    {
	    xoff--:back32--
	    goto back_loop
	    }
	}

    back_return:
    poke npa,low back32
    poke npa+2,high back32
    }

;==============================================================================

;npa = pointer to 32 bit address, update with forward address

function forward_line(npa,nlines)
    {
    forward32=peek npa

    while nlines
	{
	xms_get(max_width,work_seg,0,.forward32)
	xoff=0

	repeat max_width
	    {
	    c=work_seg[xoff]b:xoff++:forward32++
	    if eol(c) then goto forward_eol
	    }

	forward_eol:
	if32 forward32>file_size then forward32=file_size:goto forward_return

	nlines--
	}

    forward_return:
    poke npa,low forward32
    poke npa+2,high forward32
    }

;==============================================================================

function end_of_line
    {
    start=buffer+max_width-1
    while start>=buffer
	{
	if peekb start<>' ' then goto end_found
	start--
	}
    end_found:
    return 1+start-buffer
    }

function first_nonblank
    {
    fbx=0
    while (fbx<max_width) and (peekb (buffer+fbx)=' ') fbx++
    if fbx=max_width then fbx=0
    return fbx
    }

;==============================================================================

proc home
    {
    file_column=0
    xpos=0
    }

proc top_of_file
    {
    file_pos=0:ypos=0
    home
    }

;==============================================================================

function compress_buffer
    {
    cl=end_of_line
    poke buffer+cl,0a0dh
    return cl+2
    }

;==============================================================================

function uppercase_buffer
    {
    x=buffer
    repeat max_width
	{
	pokeb x,ucase peekb x
	x++
	}
    edit_line=1
    }

function lowercase_buffer
    {
    x=buffer
    repeat max_width
	{
	pokeb x,lcase peekb x
	x++
	}
    edit_line=1
    }

;==============================================================================

proc put_char(pchr)
    {
    pcx=file_column+xpos

    if pcx<max_width then
	{
	if insert_mode then moveb (max_width-1)-pcx from pcx+buffer to pcx+1+buffer
	pokeb buffer+pcx,pchr
	xpos++
	edit_line=1
	}

    #long
    ;word wrap? if exceeds width (printable characters by font?)
    if word_wrap then
    if (pcx>=77) and (pchr<>13) then
	{
	#short
	pcx=77
	while (pcx>40) and (peekb (buffer+pcx)<>' ') pcx--
	if pcx=40 then splitx=77
	else splitx=pcx+1
	wordx=xpos+file_column-splitx
	if wordx<0 then wordx=0

	slx=splitx:stx=0
	gosub split_line2	;split_line(splitx,0)

	ypos++
	file_column=0:xpos=wordx
	}
    }

;==============================================================================

function buffer_sept(sx)
    {
    sx=peekb (buffer+sx)
    return searchb septs from seperators for sx
    }

function word_left(wx)
    {
    x=wx
    if x then x--
    if buffer_sept(x) then
	{
	while x>=0
	    {
	    if not buffer_sept(x) then goto wl2
	    x--
	    }
	return 0
	}

    wl2:
    while x>=0
	{
	if buffer_sept(x) then return x+1
	x--
	}
    return 0
    }

function word_right(wx)
    {
    x=wx
    while x<max_width
	{
	if buffer_sept(x) then goto wr2
	x++
	}
    goto wr_end

    wr2:
    while x<max_width
	{
	if not buffer_sept(x) then return x
	x++
	}

    wr_end:
    if x>=end_of_line then return end_of_line
    return wx
    }

;==============================================================================

proc delete_char(xd)
    {
    moveb (max_width-1)-xd from buffer+xd+1 to buffer+xd:edit_line=1
    }

proc backspace(xd)
    {
    if xd then
	{
	if insert_mode then moveb max_width-xd from buffer+xd to buffer-1+xd
	else pokeb buffer+xd-1,' '
	xpos--
	}
    edit_line=1
    }

;==============================================================================

proc clear_line
    {
    fill max_width/2 from buffer with 2020h
    home
    edit_line=1
    }

proc clear_eol
    {
    pcx=file_column+xpos
    fillb max_width-pcx from buffer+pcx with 20h
    edit_line=1
    }

;==============================================================================

proc shift_left
    {
    file_column=0
    xpos=first_nonblank
    xn=(xpos/tab_spacing)*tab_spacing-tab_spacing
    if xn<0 then xn=0
    if xpos then
	{
	moveb max_width-xpos from buffer+xpos to buffer+xn
	edit_line=1
	}
    }

proc shift_right
    {
    file_column=0
    xpos=first_nonblank
    xn=((xpos+tab_spacing)/tab_spacing)*tab_spacing
    moveb max_width-xn from buffer+xpos to buffer+xn
    fillb xn-xpos from buffer+xpos with ' '
    edit_line=1
    }

;==============================================================================

proc clear_block_line(cy)
    {
    m=(cy+2)*fscreen_cols2+1
    repeat fscreen_cols1 video[m]b=colour_text:m+=2
    }

proc fill_block(x1,y1,x2,y2,fc)
    {
    if x1<0 then x1=0
    if x2<0 then x2=0
    if x1>=fscreen_cols1 then x1=fscreen_cols1-1
    if x2>=fscreen_cols1 then x2=fscreen_cols1-1

    y=0
    while y<y1 clear_block_line(y):y++

    while y<=y2
	{
	m=(y+2)*fscreen_cols2+1
	x=0
	while x<x1 video[m]b=colour_text:m+=2:x++
	while x<=x2 video[m]b=fc:m+=2:x++
	while x<fscreen_cols1 video[m]b=colour_text:m+=2:x++
	y++
	}

    while y<page_height clear_block_line(y):y++
    }

;==============================================================================

proc reset_block
    {
    column_block=0
    line_block=0
    fill_block(0,0,fscreen_cols1-1,fscreen_rows1-3,colour_text)
    }

;==============================================================================

function memory_move(db1,db2)
    {
    address1=peek db1
    address2=peek db2

    if32 address1>file_size then address1=file_size
;   if32 address2>file_size then address2=file_size

    ;move memory from (db1 to file_size) to (db2)
    displacement=address2-address1
    edit_file=1

    if32 address1>address2 then     ;delete block?
	{
	if32 block_startpos>address2 then
	    {
	    if32 block_startpos>=address1 then block_startpos+=displacement
	    else reset_block
	    }
	if32 file_pos>address2 then
	    {
	    if32 file_pos>=address1 then file_pos+=displacement
	    else file_pos=address2
	    }

	;xms_move only works with address1 > address2
	move_length=file_size-address1
	if32 move_length then xms_move(.move_length,.address1,.address2)
	file_size+=displacement
	}
    else
	{
	;insert memory by getting 20K block, move to += displacement
	;start from end of memory
	n32=file_size
	move_length=file_size-address1
	if (low move_length) and 1 then
	    {
	    n32++
	    move_length++
	    }

	memory_insert:
	if32 move_length>work_size then ml=work_size
	else ml=low move_length

	n32-=ml

	if ml then xms_get(ml,work_seg,0,.n32)
	pa32=n32+displacement
	if ml then xms_put(ml,work_seg,0,.pa32)

	move_length-=ml
	if ml=work_size then goto memory_insert
	file_size+=displacement
	must_show
	}
    }

;==============================================================================

proc centre_line
    {
    #long
    if ypos>page_middle then
	{
	forward_line(.file_pos,ypos-page_middle)
	ypos=page_middle
	}
    else
	{
	n32=file_pos
	back_line(.n32,page_middle-ypos)
	pa32=n32
	mc=0
	while mc<=page_middle
	    {
	    if32 pa32>=file_pos then file_pos=n32:ypos+=mc:return
	    forward_line(.pa32,1)
	    mc++
	    }
	}
    #short
    }

;==============================================================================

proc put_line
    {
    displacement=0
    last_line_address=9999999

    #long
    if32 line_address<>-1 then if edit_line then
	{
	#short
	line_len=compress_buffer
	pa32=line_address
	forward_line(.pa32,1)	    ;pa=address of eol
	pc32=line_address+line_len  ;pc=new address of eol

	memory_move(.pa32,.pc32)

	;if inserting odd number then get last byte
	if line_len and 1 then
	    {
	    n32=line_address+line_len
	    xms_get(2,reg cs,buffer+line_len,.n32)
	    line_len++
	    }

	xms_put(line_len,reg cs,buffer,.line_address)

	last_line_address=line_address

	if32 line_address<block_endpos then block_endpos+=displacement
	if32 line_address<block_startpos then block_startpos+=displacement
	if32 line_address<file_pos then file_pos+=displacement

	edit_file=1
	edit_line=0
	}
    }

;==============================================================================

function get_line(npa)
    {
    l32=peek npa
    if32 line_address=l32 then return

    put_line		    ;different line - save it if changed

    if32 l32>last_line_address then l32+=displacement
    line_address=l32

    fill max_width/2 from buffer with 2020h

    col=0
    edit_line=0
    xms_get(max_width,work_seg,0,.line_address)
    xoff=0

    if32 line_address>=file_size then mw=0
    else
	{
	n32=file_size-line_address
	if32 n32>max_width then mw=max_width
	else mw=low n32
	}

    while col<mw
	{
	byte=work_seg[xoff]b:xoff++
	if eol(byte) then return
	if byte=9 then col=(col and 248)+8
	else if byte<>10 then
	    {
	    pokeb buffer+col,byte
	    col++
	    }
	}
    }

;==============================================================================

proc delete_line
    {
    put_line
    n32=line_address
    forward_line(.n32,1)
    memory_move(.n32,.line_address)
    line_address=-1
    must_show
    }

;==============================================================================

proc join_line(jlx)
    {
    ex=end_of_line
    if jlx<ex then jlx=ex	;join to end of line (at least)

    moveb max_width from buffer to buffer2

    save_line=line_address
    n32=line_address
    forward_line(.n32,1)
    get_line(.n32)				    ;get next line
    moveb max_width-jlx from buffer to buffer2+jlx  ;join
    delete_line 				    ;delete joint line
    line_address=save_line
    moveb max_width from buffer2 to buffer	    ;update buffer
    edit_line=1

    must_show
    }

;==============================================================================

proc split_line(slx,stx)	;slx=xpos, stx=indent
    {
    split_line2:

    fillb max_width from buffer2 with ' '
    moveb max_width-slx from buffer+slx to buffer2+stx

    file_column=0
    xpos=slx:clear_eol
    xpos=end_of_line

    put_char(13)
    put_line

    forward_line(.line_address,1)
    edit_line=1
    moveb max_width from buffer2 to buffer

    must_show
    }

;==============================================================================

function repeat_line	    ;copies current buffer
    {
    moveb max_width from buffer to buffer2  ;copy line

    file_column=0:xpos=end_of_line
    put_char(13)
    put_line				    ;put line + blank line

    forward_line(.line_address,1)
    edit_line=1
    moveb max_width from buffer2 to buffer  ;restore buffer
    }

;==============================================================================

proc mark_columns(mcx)
    {
    put_line
    reset_block
    column_block=1
    block_startpos=line_address
    block_startx=mcx
    }

;==============================================================================

proc draw_block
    {
    newx1=xpos:newy1=ypos
    if line_block then newx1=255

    newx2=block_startx-file_column
    newy2=0

    n32=file_pos

    draw_while:
    if newy2<(page_height-1) then if32 n32<block_startpos then
	{
	newy2++
	forward_line(.n32,1)
	goto draw_while
	}

    if newx1>newx2 then swap newx1,newx2
    if newy1>newy2 then swap newy1,newy2

    fill_block(newx1,newy1,newx2,newy2,colour_block)
    }

;==============================================================================

function print_line(py)
    {
    slocate(2+py,0)
    n32=file_pos
    forward_line(.n32,py)
    get_line(.n32)
    last_ypos=py
    print_buffer
    }

;==============================================================================

proc set_old
    {
    last_pos=file_pos
    last_column=file_column
    }

;==============================================================================

proc set_positions
    {
    set_positions2:
    while xpos<0
	{
	xpos+=8
	file_column-=8:if file_column<0 then xpos=0
	}
    while xpos>=fscreen_cols1
	{
	xpos-=8
	file_column+=8
	}
    if ypos<0 then
	{
	ypos=0
	back_line(.file_pos,1)
	}
    if ypos>=page_height then
	{
	ypos=page_height-1
	forward_line(.file_pos,1)
	}
    if file_column<0 then file_column=0
    if file_column>(max_width-fscreen_cols1) then file_column=max_width-fscreen_cols1

    if32 file_pos>=file_size then file_pos=file_size

    flag=0	    ;screen position changed?
    if ypos<>last_ypos then flag=1
    else if32 file_pos<>last_pos then flag=1

    #long
    if flag then
	{
	#short
	n32=file_pos
	yo=0
	if ypos then
	    {
	    repeat ypos
		{
		old32=n32
		forward_line(.n32,1)
		if32 n32=old32 then ypos=yo:goto get_yo
		yo++
		}
	    }
	get_yo:
	get_line(.n32)
	last_ypos=ypos
	}

    flag=0
    if file_column<>last_column then flag=1
    else if32 file_pos<>last_pos then flag=1

    #long
    if flag then
	{
	#short
	if keypressed then
	    {
	    last_pos=-1
	    goto exe_key
	    }
	colour colour_text
	if32 file_pos=last_pos then goto page_all

	n32=file_pos
	forward_line(.n32,1)
	if32 n32=last_pos then
	    {
	    scroll down 0,2,fscreen_cols1-1,fscreen_rows1-1,1
	    print_line(0)
	    goto edit_page
	    }

	n32=file_pos
	back_line(.n32,1)
	if32 n32=last_pos then
	    {
	    scroll 0,2,fscreen_cols1-1,fscreen_rows1-1,1
	    print_line(fscreen_rows1-3)
	    goto edit_page
	    }

	page_all:
	display_text

	edit_page:
	set_old
	}

    exe_key:

    flag=0
    if ypos<>last_ypos then flag=1
    else if32 file_pos<>last_pos then flag=1

    if flag then
	{
	n32=file_pos
	forward_line(.n32,ypos)
	get_line(.n32)
	last_ypos=ypos
	}

    if column_block or line_block then draw_block

    slocate(2+ypos,0)
    print_buffer

    cursor size 14-(insert_mode*4),15
    cursor 2+ypos,xpos
    }

;==============================================================================

function scrap(do)
    {
    show_message("Saving block...")

    put_line
    line_address=file_pos
    forward_line(.line_address,ypos)

    if line_block or column_block then
	{
	block_endpos=line_address
	if32 block_startpos>block_endpos then
	    {
	    n32=block_startpos
	    block_startpos=block_endpos     ;swap start,end
	    block_endpos=n32
	    }
	}
    else    ;no block? assume current line
	{
	block_startpos=line_address
	block_endpos=line_address
	}
    forward_line(.block_endpos,1)

    create #1,fw_scrap
    if error then
	{
	warning("Cant Create Scrap File")
	close window
	return
	}

    #long
    if column_block then
	{
	x1=block_startx
	xc=xpos+file_column
	if xc<x1 then swap x1,xc
	scrap_len=1+xc-x1

	pokeb block_type,0:poke block_type+1,scrap_len
	write #1,3 from block_type
	if error then
	    {
	    fw_write_error:
	    warning("Error writing to Scrap")
	    close #1
	    close window
	    return
	    }

	write_columns:
	if32 block_startpos<block_endpos then
	    {
	    fw_working
	    get_line(.block_startpos)
	    write #1,scrap_len from buffer+x1
	    if error then goto fw_write_error
	    if do then
		{
		moveb max_width-x1-scrap_len from buffer+x1+scrap_len to buffer+x1
		edit_line=1
		put_line
		}
	    forward_line(.block_startpos,1)
	    goto write_columns
	    }
	}
    else	;line block
	{
	block_origin=block_startpos
	n32=block_endpos-block_startpos

	write_buffer:
	fw_working
	if32 n32>work_size then wlen=work_size
	else wlen=low n32

	xms_get(wlen,work_seg,0,.block_startpos)
	write #1,wlen from work_seg|0
	if error then goto fw_write_error

	block_startpos+=wlen
	n32-=wlen
	if32 n32 then goto write_buffer

	if do then
	    {
	    memory_move(.block_endpos,.block_origin)
	    }
	}
    #short

    close #1
    if do then must_show:line_address=-1
    close window
    reset_block
    }

;==============================================================================

function insert_scrap_columns
    {
    xpos+=file_column:file_column=0
    scrap_len=peek (block_type+1)
    block_origin=line_address

    forever
	{
	fw_working
	get_line(.block_origin)
	moveb max_width-xpos-scrap_len from xpos+buffer to xpos+scrap_len+buffer

	rlen=read #1,scrap_len to buffer+xpos
	if error then return		;eof?
	if rlen<>scrap_len then return	;file error?

	edit_line=1
	put_line
	forward_line(.block_origin,1)
	}
    }

;==============================================================================

function insert_scrap_lines
    {
    n32=line_address+block_size
    memory_move(.line_address,.n32)

    seek #1,0

    read_lines:
    fw_working
    read_size=read #1,work_size to work_seg|0
    if error then
	{
	warning("Error reading file - aborted!")
	return
	}

    if read_size and 1 then	;read odd number?
	{
	n32=line_address+read_size
	xms_get(2,work_seg,read_size,.n32)
	read_size++
	}

    xms_put(read_size,work_seg,0,.line_address)
    line_address+=read_size

    if read_size=work_size then goto read_lines
    }

;==============================================================================

proc insert_scrap
    {
    put_line
    show_message("Inputting block...")

    open #1,fw_scrap:if error then goto cs_error

    seek #1,eof:rax=reg ax:rdx=reg dx
    block_size=(rdx*65536)+rax

    if32 (file_size+block_size)>=memory_size then
	{
	warning("Scrap file is too large!")
	close #1
	close window
	return
	}

    seek #1,0
    r=read #1,3 to block_type
    if (r<>3) or (error<>0) then goto cs_error

    if peekb block_type then insert_scrap_lines
    else insert_scrap_columns

    close #1
    close window
    reset_block
    must_show
    line_address=-1
    return

    cs_error:
    warning("Error reading Scrap file")
    close window
    }

;==============================================================================

function find_in_buffer
    {
    f=buffer+x
    fend=buffer+max_width
    while f below fend
	{
	f=searchb fend-f from f for peekb findstr
	if f then
	    {
	    m=findstr
	    f2=f
	    repeat findl
		{
		c=peekb m
		if c='?' then goto fnchar
		if c<>peekb f2 then goto fnext
		fnchar:
		m++:f2++
		}
	    return 1+f-buffer

	    fnext:
	    f++
	    }
	else return 0
	}
    return 0
    }

;==============================================================================

function find_string
    {
    find_address=line_address
    x=xpos+file_column

    find_loop:
    fw_working
    get_line(.find_address)

    x=find_in_buffer
    if x then
	{
	file_pos=line_address
	xpos=x-1:file_column=0
	ypos=0:centre_line
	must_show
	return 1
	}

    forward_line(.find_address,1)
    x=0
    if32 find_address<file_size then goto find_loop
    return 0
    }

;==============================================================================

proc input_find(first)
    {
    #long
    if first then
	{
	#short
	x=low curpos:y=high curpos
	if x>(fscreen_cols1-60) then x=fscreen_cols1-60:y+=3
	if y>(fscreen_rows1-3) then y=fscreen_rows1-3
	pokeb window_find+2,x+2
	pokeb window_find+3,y-2
	pokeb window_find+4,x+52
	pokeb window_find+5,y+2
	pokeb window_find+6,15

	fw_window(window_find)
	slocate(y+2,x+2)
	print "Find : ";

	findl=ext_string(findstr,40)
	if findl<2 then close window:return 0
	findl--
	}
    else xpos++

    if find_string then
	{
	if first then close window
	return 1
	}

    warning("Text not found.")
    if first then close window
    must_show
    line_address=-1
    if not first then xpos--
    return 0
    }

;==============================================================================

proc input_replace(first)
    {
    ir=input_find(first):if ir=0 then return

    #long
    if first then
	{
	#short
	x=peekb (window_find+2)
	y=peekb (window_find+3)+3
	pokeb window_find+2,x+2
	pokeb window_find+3,y-2
	pokeb window_find+4,x+55
	pokeb window_find+5,y+2
	pokeb window_find+6,14

	fw_window(window_find)
	slocate(y+2,x+2)
	print "Replace : ";

	replacel=ext_string(replacestr,40)
	close window
	if replacel<2 then return 0
	replacel--
	}

    replace_next:
    #long
    if ir then
	{
	#short
	must_show:set_positions
	fw_window(window_replace)
	wait for keypressed:rk=lcase key:print chr rk;
	replace=0		  ;Default: replace none.
	if rk='y' then replace=1     ;One only.
	if rk='a' then replace=32767	;Max.
	close window
	if rk=27 then return

	if replace=0 then xpos++:ir=find_string
	while (replace<>0) and (ir<>0)
	    {
	    moveb max_width-xpos-findl from buffer+xpos+findl to buffer+xpos+replacel
	    moveb replacel from replacestr to buffer+xpos
	    edit_line=1
	    replace--
	    xpos+=replacel:ir=find_string
	    if (replace<>0) and (ir<>0) then set_positions
	    }
	if replace then must_show:set_positions
	goto replace_next
	}
    }
