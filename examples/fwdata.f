
;==============================================================================
; Fast Word - data & variables
;==============================================================================

;== constants =================================================================

const colour_background=23,colour_text=31
const colour_edit=31
const colour_title=112
const colour_block=78

const colour_capitals=30
const colour_numbers=27

const colour_select1=79,colour_select2=27

const max_width=1000

const work_size=10000	;work area

;== variables =================================================================

var work_seg,largest
var xpos,ypos,last_ypos,lineb
var insert_mode,tab_spacing,word_wrap
var xms_memory

var32 n32,l32,line32,old32,cp32
var32 file_size,memory_size
var32 block_startpos,block_endpos
var32 block_origin,block_size
var32 file_pos,last_pos,page_pos
var32 line_address,last_line_address,save_line
var32 address1,address2
var32 move_length,displacement
var32 pa32,pb32,pc32
var32 forward32,back32
var32 find_address,save_address

var file_column,last_column
var findl,replacel

var fw_line

;== data/tables ===============================================================

buffer	     ? max_width+2
buffer2      ? max_width+2

block_type   ? 3

file_name:
fname 'unknown'
space 64

backup_name: space 64
backup_ext:  fname '.bak'

seperators: datab ' !^*()-=+\|.,<>/?''":;[]'
sept_end: const septs=sept_end-seperators

fw_scrap: fname 'fw.ins'

eol_codes: datab 0,11,12,13,26

work_circle: datab '|/-\'

findstr     ? 50
replacestr  ? 50

;== windows ===================================================================

window_error:
    datab 0,0,28,15,38,22,49
    datab 22,17,0,20,207,' ERROR '
    datab 22,3,4,20,71,'--> Press ESC to Exit'
    datab 26

window_file_name:
    datab 0,0,4,10,76,16,112
    datab 26

window_warning:
    datab 0,0,25,16,60,22,78
    datab 26

window_message:
    datab 0,0,20,15,60,21,48
    datab 26

window_confirm:
    datab 0,0,12,16,42,22,11
    datab 26

window_directory:
    datab 0,0,2,3,77,20,63
    datab 26

window_find:
    datab 0,0,0,0,0,0,15
    datab 26

window_replace:
    datab 0,0,10,13,41,17,48
    datab 22,2,2,'Replace Yes/No/All or ESC?'
    datab 26
