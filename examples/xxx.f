
;read date from clock
reg ax=0400h:int 1ah	    ;CH=19/20, CL=year, DH=month, DL=day
date_cx=reg cx		    ;BINARY CODED DECIMAL (ALL VALUES)
date_dx=reg dx

date_year=bcd2dec(high date_cx)*100+bcd2dec(low date_cx)
date_month=bcd2dec(high date_dx)
date_day=bcd2dec(low date_dx)

print bios date_day;"/";date_month;"/";date_year

;set date in DOS
;reg cx=date_year
;dos 2bh		     ;DX is unchanged
