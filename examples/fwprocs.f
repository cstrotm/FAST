
;==============================================================================
; Fast Word - Procedures
;==============================================================================

#short

proc slocate(sly,slx)
    {
    locpos=sly*lineb+slx*2
    }

proc set_video(width,depth)
    {
    lineb=width*2
    width1=width-1
    smiddle=depth/2-1

    if depth=25 then
	{
	if mono then screen 7
	else screen 3
	}
    else if depth=50 then
	{
	reg bx=0
	reg ax=1112h
	int 10h
	}
    else screen 56h

    fscreen_cols1=width
    fscreen_cols2=width*2
    fscreen_rows1=depth
    fscreen_size=fscreen_rows1*fscreen_cols2

    page_height=fscreen_rows1-2
    page_middle=page_height/2

    colour colour_title
    for x=0 to fscreen_cols1-1
	slocate(0,x):print " ";
	slocate(1,x):print " ";
    next x
    slocate(0,6)
    print """FW"" v1.01 Fast Word Processor - (c) Peter Campbell Software 1994.";
    }

;==============================================================================

proc change_video
    {
    if fscreen_rows1=25 then set_video(80,50)
    else if fscreen_rows1=50 then set_video(132,60)
    else set_video(80,25)
    }

;==============================================================================

proc draw_box(x1,y1,x2,y2,c,heading)
    {
    colour c
    for y=y1 to y2
	slocate(y,x1):print "º";
	slocate(y,x2):print "º";
    next y

    for x=x1 to x2
	slocate(y1,x):print "Í";
	slocate(y2,x):print "Í";
    next x

    slocate(y1,x1):print "É";
    slocate(y1,x2):print "»";
    slocate(y2,x1):print "È";
    slocate(y2,x2):print "¼";

    scroll x1+1,y1+1,x2-1,y2-1,0

    x=heading:len=2
    while peekb x len++:x++

    x=(x1+x2-len)/2
    slocate(y1,x)
    print " ";
    prints heading,0
    print " ";
    }

;==============================================================================

proc fw_window(fw)
    {
    open window fw
    colour peekb (fw+6)
    x=peekb (fw+2)
    y=peekb (fw+3)
    slocate(y+3,x+3)
    }

;==============================================================================

proc abort_fw
    {
    set_video(80,25)
    cursor 3,0
    if xms_memory then xms_deallocate
    print bios "FW: Ok"
    stop
    }

proc abort_error(aem)
    {
    fw_window(window_error)
    prints aem,0
    beep
    wait for key=27
    close window
    abort_fw
    }

;==============================================================================

proc warning(aem)
    {
    fw_window(window_warning)
    prints aem,0
    beep
    wait for keyscan
    close window
    }

;==============================================================================

proc show_message(aem)
    {
    fw_window(window_message)
    prints aem,0
    }

;==============================================================================

proc fw_working
    {
    working_counter++
    curtoloc
    colour colour_background
    print chr peekb (work_circle+(working_counter and 3));
    }

proc fw_not_working
    {
    }

;==============================================================================

function confirm(aem)
    {
    fw_window(window_confirm)
    prints aem,0
    print " ";
    loctocur

    confirm_wait:
    wait for keypressed
    k=lcase key
    if k=27 then close window:return 0
    if k='n' then close window:return 0
    if k='y' then close window:return 1
    beep
    goto confirm_wait
    }

;==============================================================================

proc must_show	    ;force new page display
    {
    last_pos=-1
    last_ypos=-1
    }

;==============================================================================

function eol(eolc)
    {
    return searchb 5 from eol_codes for eolc
    }

;==============================================================================

proc display_main
    {
    colour colour_title
    slocate(1,1)
    print "File : ";
    w=file_name
    repeat 30
	{
	c=peekb w:if c then w++
	print chr ucase c;
	}

    slocate(1,62)
    n32=(file_size+1023)/1024
    k=low n32
    print k;"K of ";largest;"K  ";
    }

;==============================================================================

proc display_help
    {
    }

;==============================================================================

proc display_text
    {
    last_pos=file_pos
    last_column=file_column

    xms_get(work_size,work_seg,0,.file_pos)
    page_pos=file_pos
    xoff=0

    for yp=2 to fscreen_rows1-1

    colour colour_text
    slocate(yp,0)
    col=0
    while col<file_column
	{
	c=work_seg[xoff]b
	if eol(c) then px=0:goto line_loop2
	if c<>10 then
	    {
	    if c=9 then col=(col and 248)+8 else col++
	    }

	xoff++:page_pos++
	}

    px=0
    col-=file_column
    if col then repeat col print " ";:px++

    line_loop2:
    if32 page_pos>=file_size then goto page_end

    while px<fscreen_cols1
	{
	c=work_seg[xoff]b
	#long
	if c>31 then
	    {
	    ;colour printing
	    if (c>='0') and (c<='9') then colour colour_numbers
	    else if (c>='A') and (c<='Z') then
		{
		nc=work_seg[xoff+1]b
		if (nc<'a') or (nc>'z') then colour colour_capitals
		else colour colour_text
		}
	    else colour colour_text

	    print chr c;:px++
	    xoff++:page_pos++
	    }
	else if c=9 then
	    {
	    col=(px and 248)+8
	    while px<col print " ";:px++
	    xoff++:page_pos++
	    }
	else if c=10 then
	    {
	    xoff++:page_pos++
	    }
	else if c=12 then
	    {
	    colour colour_text-1
	    print "Í";:px++
	    }
	else print " ";:px++
	#short
	}

    if32 page_pos>=file_size then goto page_end

    ffc=work_seg[xoff]b
    #long
    if eol(ffc) then
	{
	xoff++:page_pos++
	}
    else
	{
	#short
	xo=xoff
	repeat max_width
	    {
	    c=work_seg[xo]b:xo++
	    if eol(c) then goto stop_search
	    }
	goto page_end	    ;end of line not found?

	stop_search:
	page_pos+=xo-xoff
	xoff=xo

	if32 page_pos>=file_size then goto page_end
	}

    page_nl:
    next yp
    return

    page_end:
    if yp<(fscreen_rows1-1) then scroll 0,yp+1,fscreen_cols1-1,fscreen_rows1-1,0
    }

;==============================================================================

function get_directory_file
    {
    ;load directory, select and return file_name

    dir file_name,work_seg|0:files=work_seg[0]
    if not files then
	{
	none_dir:
	warning("No files found")
	return 0
	}
    if not sort(work_seg,2,13,files) then goto none_dir

    fw_window(window_directory)
    slocate(20,5):print " ";files;" file(s) ";
    x=0:lx=-1

    forever
	{
	#long
	if (x/80)<>lx then
	    {
	    #short
	    start=(x/80)*80
	    lx=x/80
	    px=4:py=4

	    while py<20
		{
		slocate(py,px):print "            ";:slocate(py,px)
		if start<files then
		    {
		    st=(start*13)+2
		    while work_seg[st]b print chr work_seg[st]b;:st++
		    start++
		    }
		px+=14:if px>70 then px=4:py++
		}
	    }

	py=(x mod 80)/5:px=x mod 5
	slocate(py+4,px*14+3):st=locpos+1:old=st
	repeat 12 video[st]b=colour_select2:st+=2

	wait for keypressed:s=scan
	if s=1 then close window:return 0
	if s=72 then x-=5
	if s=80 then x+=5
	if s=75 then x--
	if (s=77) or (s=15) then x++
	if s=73 then x-=80
	if s=81 then x+=80
	if (s=71) or (x<0) then x=0
	if (s=79) or (x>=files) then x=files-1

	st=old:repeat 12 video[st]b=peekb (window_directory+6):st+=2

	if s=28 then
	    {
	    move 7 from work_seg|x*13+2 to file_name
	    close window
	    return 1
	    }
	}

    return 0
    }

;==============================================================================

;if command line then parse this to the file name field - else null it
;display open window - no input if command line specified
;if wildcard(s) entered then get directory display and select

function open_file(commandq)
    {
    file_size=0
    fw_line=1

    xpos=0:ypos=0
    file_pos=0:last_pos=-1
    file_column=0

    enter_file:
    pokeb file_name,0	;null file name
    if commandq then
	{
	if cmd_nos>=1 then
	    {
	    moveb cmd_len(1) from cmd_add(1) to file_name
	    pokeb file_name+cmd_len(1),0
	    }
	}

    fw_window(window_file_name)
    print "File : ";

    push locpos
    prints file_name,0
    pop locpos

    if not peekb file_name then     ;filename entered?
	{
	length=ext_string(file_name,60)
	if (peekb file_name=0) or (length=0) then close window:return 0
	}

    ;check file name for wildcards
    wild=0
    x=file_name
    while peekb x
	{
	if (peekb x='*') or (peekb x='?') then wild=1
	x++
	}

    if wild then
	{
	if not get_directory_file then goto open_error
	}

    ;load file name
    #errors off
    open #1,file_name
    #long
    if error then
	{
	#short
	if error=2 then
	    {
	    if confirm("New File - Create?") then
		{
		create #1,file_name
		if error then
		    {
		    warning("File Creation Error")
		    goto open_error
		    }
		goto close_file_ok
		}
	    goto open_error
	    }
	else
	    {
	    warning("Can't Open File")
	    goto open_error
	    }
	}

    read_loop:
    n32=memory_size-file_size
    if32 n32<work_size then rmax=low n32 else rmax=work_size

    read_size=read #1,rmax to work_seg|0
    if error then
	{
	warning("Error reading file - aborted!")
	open_error:
	close window
	commandq=0
	goto enter_file
	}

    xms_put(read_size,work_seg,0,.file_size)
    file_size+=read_size
    display_main	    ;update memory display

    if read_size=work_size then goto read_loop
    if read_size=rmax then warning("File is too Large - Truncated!!!")

    close_file_ok:
    close #1

    close window

    edit_file=0:edit_line=0
    must_show:line_address=-1

    insert_mode=1
    word_wrap=1
    tab_spacing=8

    line_block=0:column_block=0

    return 1
    }

;==============================================================================

function save_file(save_askq)
    {
    fw_window(window_file_name)
    print "Save : ";

    push locpos
    prints file_name,0
    pop locpos

    if save_askq then
	{
	length=ext_string(file_name,60)
	if (peekb file_name=0) or (length=0) then close window:return 0
	}

    moveb length+1 from file_name to backup_name
    f=searchb length from backup_name for '.'
    if not f then f=searchb length+1 from backup_name for 0
    if f then moveb 5 from backup_ext to f

    ;create backup files?
    delete backup_name	;if error -> assume didn't exist
    rename file_name to backup_name
    if error>2 then
	{
	warning("Error Creating Backup File")
	}

    create #1,file_name
    if error then
	{
	warning("Error Creating File - aborted!")
	close window
	return
	}

    displacement=0

    write_next_block:
    if32 displacement<file_size then
	{
	n32=file_size-displacement

	if32 n32>work_size then write_size=work_size
	else write_size=low n32

	xms_get(write_size,work_seg,0,.displacement)
	write #1,write_size from work_seg|0
	if reg ax<>write_size then goto save_error
	if error then
	    {
	    save_error:
	    warning("Error Writing File - aborted!")
	    close window
	    return
	    }

	displacement+=write_size
	goto write_next_block
	}

    close #1
    close window

    edit_file=0
    edit_line=0
    }

;==============================================================================

proc fast_word_setup
    {
    on error
	{
	locate 0,0:print "system error #";error
	beep
	wait for keyscan
	abort_fw
	}

    xms_memory=0
    set_video(80,50)

    work_seg=allocate work_size/16+1	;general buffer, margin for MOVE

    ;allocate XMS memory for file
    if not xms_init then abort_error("XMS (Himem) not available!")

    largest=xms_query:memory_size=(largest-5)*1024
    if largest<10 then abort_error("Insufficient XMS Memory!")

    xms_allocate(largest)
    xms_memory=1

    file_size=0
    display_main
    if not open_file(1) then abort_fw	;open file with command line?

    fillb 50 from findstr with ' '
    fillb 50 from replacestr with ' '
    }
