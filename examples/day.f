
var32 n32

function func1(fy,fm)
    {
    if fm<=2 then return fy-1
    return fy
    }

function func2(fm)
    {
    if fm<=2 then return fm+13
    return fm+1
    }

curtoloc
y=1990
m=12
for d=1 to 20
    f1=func1(y,m)
    f2=func2(m)
    f2=153*f2/5+d
    n32=((1461*f1)/4)+f2
    n32=(n32-621049) mod 7
    print d;"/";m;"/";y;"  ";
    day=low n32
    a=printm days+day*9,9
    print
next d
loctocur
stop

days:
datab 'Sunday   '
datab 'Monday   '
datab 'Tuesday  '
datab 'Wednesday'
datab 'Thursday '
datab 'Friday   '
datab 'Saturday '
