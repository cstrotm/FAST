
;==============================================================================
; Fast Word - key tables
;==============================================================================

key_table:
data 18432	;up
data 20480	;down
data 19200	;left
data 19712	;right

data 18688	;pgup
data 20736	;pgdn

data 18176	;home
data 20224	;end
data 30464	;ctrl-home
data 29952	;ctrl-end
data 33792	;ctrl-pgup
data 30208	;ctrl-pgdn

data 29440	;ctrl-left
data 29696	;ctrl-right
data 16640	;shift-left  (f7)
data 16896	;shift-right (f8)
data 23040	;shift-left  (shift-f7)
data 23296	;shift-right (shift-f8)

data 12290	;ctrl-b
data 5140	;ctrl-t
data 11779	;ctrl-c

data 21248	;delete
data 3592	;backspace
data 4608	;alt-e
data 9472	;alt-k

data 5401	;ctrl-y
data 3711	;ctrl-backspace
data 7936	;alt-s
data 9216	;alt-j
data 5120	;alt-t
data 4864	;alt-r

data 283	;escape
data 20992	;insert
data 7181	;enter

data 7680	;alt-a
data 12288	;alt-b
data 9728	;alt-l
data 4352	;alt-w
data 11264	;alt-z
data 12032	;alt-v

data 5653	;ctrl-u
data 5897	;ctrl-i
data 4375	;ctrl-w

data 15104	;f1
data 15360	;f2
data 15616	;f3
data 15872	;f4
data 16128	;f5
data 16384	;f6
data 16640	;f7
data 16896	;f8
data 17152	;f9
data 17408	;f10

data 3840	;shift-tab
data 3849	;tab

data 20011	;plus - copy to scrap
data 18989	;minus - cut to scrap
data 21040	;insert block

data 22528	;shift-f5 find
data 22784	;shift-f6 replace
key_end:

;==============================================================================

proc_table:
data move_line_up
data move_line_down
data move_char_left
data move_char_right
data move_page_up
data move_page_down
data move_home
data move_end
data move_page_home
data move_page_end
data move_file_home
data move_file_end
data move_word_left
data move_word_right
data move_line_left
data move_line_right
data shift_left_down
data shift_right_down
data move_bottom
data move_top
data move_centre
data delete_key
data backspace_key
data key_clearline
data key_cleareol
data key_delete_line
data key_delete_line
data key_split_line
data key_join_line
data key_join_text
data key_repeat_line
data fw_escape
data insert_toggle
data enter_key
data block_makea
data block_markb
data block_markl
data block_markw
data block_markz
data change_video_mode
data key_uppercase
data key_lowercase
data key_wordwrap
data funtion_f1
data funtion_f2
data funtion_f3
data funtion_f4
data funtion_f5
data funtion_f6
data funtion_f7
data funtion_f8
data funtion_f9
data funtion_f10
data shift_tab_key
data tab_key
data scrap_plus
data scrap_minus
data scrap_insert
data find_again
data replace_again

;==============================================================================

move_line_up:
ypos--
return

move_line_down:
ypos++
return

move_char_left:
xpos--
return

move_char_right:
xpos++
return

;==============================================================================

move_page_up:
back_line(.file_pos,page_height)
return

move_page_down:
forward_line(.file_pos,page_height)
return

;==============================================================================

move_home:
home
return

move_end:
xpos=end_of_line-file_column
return

move_page_home:
ypos=0
return

move_page_end:
ypos=page_height-1
return

move_file_home:
top_of_file
return

move_file_end:
file_pos=file_size
ypos=page_height-1
back_line(.file_pos,ypos)
home
return

;==============================================================================

move_word_left:
xpos=word_left(xpos+file_column)-file_column
return

move_word_right:
xpos=word_right(xpos+file_column)-file_column
return

;==============================================================================

move_line_left:
shift_left
return

move_line_right:
shift_right
return

shift_left_down:
shift_left
ypos++
must_show
return

shift_right_down:
shift_right
ypos++
must_show
return

;==============================================================================

move_bottom:
back_line(.file_pos,(page_height-1)-ypos)
ypos=page_height-1
return

move_top:
forward_line(.file_pos,ypos)
ypos=0
return

move_centre:
centre_line
return

;==============================================================================

insert_toggle:
insert_mode=not insert_mode
return

;==============================================================================

enter_key:
enx=first_nonblank
split_line(end_of_line,enx)
ypos++
xpos=enx:file_column=0
return

;==============================================================================

delete_key:
delete_char(xpos+file_column)
return

backspace_key:
backspace(xpos+file_column)
return

key_clearline:
clear_line
return

key_cleareol:
clear_eol
return

;==============================================================================

key_delete_line:
delete_line
return

key_split_line:
split_line(xpos+file_column,0)
ypos++
home
return

key_join_line:
join_line(xpos+file_column)
return

key_join_text:
ex=end_of_line+1
join_line(ex)
ex++
repeat max_width-1
    {
    if peekb (buffer+ex)<>' ' then return
    delete_char(ex)
    }
return

;==============================================================================

key_repeat_line:
push xpos,file_column			;save X
repeat_line
pop file_column,xpos			;restore X
ypos++
return

;==============================================================================

fw_escape:
abort_fw

;==============================================================================

block_makea:
mark_columns(0)
return

block_markb:
if column_block then reset_block
else mark_columns(xpos+file_column)
return

block_markl:
if line_block then reset_block
else
    {
    put_line
    reset_block
    line_block=1
    block_startx=0
    block_startpos=line_address
    }
return

block_markw:
xright=word_right(xpos+file_column)
xpos=word_left(xright):file_column=0
mark_columns(xpos)
while xpos<max_width
    {
    if buffer_sept(xpos) then xpos--:return
    xpos++
    }
return

block_markz:
mark_columns(end_of_line)
return

;==============================================================================

change_video_mode:
change_video
last_pos=-1
return

;==============================================================================

key_uppercase:
uppercase_buffer
return

key_lowercase:
lowercase_buffer
return

;==============================================================================

key_wordwrap:
word_wrap=not word_wrap
return

;==============================================================================

funtion_f1:
return

;==============================================================================

funtion_f2:
return

;==============================================================================

funtion_f3:
put_line
save_file(1)
return

;==============================================================================

funtion_f4:
return

;==============================================================================

funtion_f5:
input_find(1)
return

;==============================================================================

funtion_f6:
input_replace(1)
return

;==============================================================================

funtion_f7:
return

;==============================================================================

funtion_f8:
return

;==============================================================================

funtion_f9:
return

;==============================================================================

funtion_f10:
push insert_mode
insert_mode=1
put_char(' ')
xpos--
pop insert_mode
return

;==============================================================================

find_again:
input_find(0)
return

replace_again:
input_replace(0)
return

;==============================================================================

shift_tab_key:
x=xpos and (0-tab_spacing)
if x=xpos then xpos-=tab_spacing
else xpos=x
return

tab_key:
newx=((xpos+tab_spacing)/tab_spacing)*tab_spacing
if insert_mode then
    {
    if (newx+file_column)>=max_width then newx=0
    while newx>xpos
	{
	put_char(' ')
	}
    }
else xpos=newx
return

;==============================================================================

scrap_plus:
if peekb 0|417h and 3 then
    {
    scrap(0)
    }
else put_char('+')
return

scrap_minus:
if peekb 0|417h and 3 then
    {
    scrap(1)
    }
else put_char('-')
return

;==============================================================================

scrap_insert:
insert_scrap
return
