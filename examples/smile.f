
;===========================================================================
; SMILEY Face v1.00
;===========================================================================

buffer ? 200

counter=10

proc smiley_face(do)
    {
    restore smiley_shape
    mem=buffer
    forever
	{
	read addr
	if addr=0 then return
	addr--
	if do then
	    {
	    pokeb mem,video[addr]b
	    video[addr]b=219
	    }
	else video[addr]b=peekb mem
	mem++
	}
    }

on int 1
    {
    if counter then counter--
    else
	{
	smiley_face(1)
	repeat 200 {}
	smiley_face(0)
	counter=10+(rnd and 5)
	}
    }

smiley_face(1)
wait for key

stop resident

;===========================================================================

smiley_shape:
data 1333,1335		    ;left eye
data 1491,1493,1495,1497
data 1653,1655

data 1385,1387		    ;right eye
data 1543,1545,1547,1549
data 1705,1707

data 1839,1841		    ;nose
data 1999,2001
data 2157,2159,2161,2163
data 2317,2319,2321,2323

data 2601,2603,2605,2607,2609	;smiley lips
data 2763,2765,2767,2769,2771
data 2927,2929,2931,2933,2935
data 3093,3095,3097,3099,3101
data 3261,3263,3265,3267,3269,3271,3273,3275,3277,3279
data 3281,3283,3285,3287,3289,3291,3293,3295,3297,3299
data 3425,3427,3429,3431,3433,3435,3437,3439
data 3441,3443,3445,3447,3449,3451,3453,3455
data 2671,2673,2675,2677,2679
data 2829,2831,2833,2835,2837
data 2985,2987,2989,2991,2993
data 3139,3141,3143,3145,3147

data 0
