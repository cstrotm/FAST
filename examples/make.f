
buffer ? 512
load "f:\vr3d\cos.dat",buffer,512

m=buffer
repeat 64
    {
    print bios "data ";
    print bios peek m;",";peek (m+2);",";peek (m+4);",";peek (m+6)
    m+=8
    }


