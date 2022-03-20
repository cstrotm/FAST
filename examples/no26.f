work ? 30000

open #1,"phone.txt"
create #2,"ph2"

rn:
len=read #1,30000 to work
l=len:m=work
while l
    {
    if peekb m=26 then pokeb m,' '
    l--:m++
    }
write #2,len from work
if len=30000 then goto rn
close #1,#2
