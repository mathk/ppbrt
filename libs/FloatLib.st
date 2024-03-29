{ import: Angle }
{ import: st80 }
{ include <math.h> }

Radian rad
[
    ^value
]

Radian deg 
[
    ^(value * 180) / Float pi
]


Degree rad
[
    ^(value * Float pi) / 180 
]

Degree deg
[
    ^value
]

Float maxVal
[
    ^1.7976931348623159e308
]

Float inf
[
    ^Float maxVal * Float maxVal
]

Float nan
[
    ^(0.0 /0.0)
]

Float sqrt
[
    self := self copy.
    {
	double rcvr, sqrtr;
	memcpy(&rcvr, v_self, sizeof(double));
	sqrtr = sqrt(rcvr);
	memcpy(v_self, &sqrtr, sizeof(double));
    }
]

Float printOn: aStream
[
    | s |
    {
	double rcvr;
	int i = 79;
	v_s = _libid->balloc(i + 1);
	char *buffer = (char*)v_s;
	memcpy(&rcvr, self, sizeof(double));

	snprintf(buffer, 79, "%f", rcvr);
	while (buffer[i] == '\0')
	  {
	      i--;
	  }
	while (buffer[i] == '0')
	  {
	    i--;
	  }
	if(buffer[i] == '.')
	  {
	    buffer[i + 2] = '\0';
	  }
	else
	  {
	      buffer[i + 1] = '\0';
	  }
    }.
    aStream nextPutAll: (String value_: s)
]

Float infinity
[
    ^100000000000000.0
]

Float epsilon
[
    ^0.001
]

Float cos
[
    self := self copy.
    {
        double d;
        memcpy(&d, v_self, sizeof(double));
        d = cos(d);
        memcpy(v_self, &d, sizeof(double));
    }
]

Float acos
[
    self := self copy.
    {
        double d;
        memcpy(&d, v_self, sizeof(double));
        d = acos(d);
        memcpy(v_self, &d, sizeof(double));
    }
]

Float pow: exp
[
    self := self copy.
    {
        double x, y, d;
        memcpy(&x, v_self, sizeof(double));
        memcpy(&y, v_exp, sizeof(double));
        d = pow(x, y);
        memcpy(v_self, &d, sizeof(double));
    }
]

Float atan2: x
[
    self := self copy.
    {
        double x, y, d;
        memcpy(&x, v_self, sizeof(double));
        memcpy(&y, v_x, sizeof(double));
        d = atan2(x, y);
        memcpy(v_self, &d, sizeof(double));
    }
]

Float round2Int
[
    {
        double x;
        int round;
        memcpy(&x, v_self, sizeof(double));
#define _doublemagicroundeps (.5-1.4e-11)
//#ifdef FAST_INT
#define _doublemagic (double)(6755399441055744.0)
        x = x + _doublemagic;
        round =  ((long*)(&x))[0];
//#else
//        round = int (x+_doublemagicroundeps)
//#endif
        return (oop)(long)((((int)(long)round) << 1) | 1);
    }
]

Float float2Int
[
    ^(self negated 
        ifTrue: [self + self doublemagicroundeps]
        ifFalse: [self - self doublemagicroundeps]) round2Int
]

Float doublemagicroundeps
[
    self := self copy.
    {   
        double d;
        d = .5-1.4e-11;
        memcpy(v_self, &d, sizeof(double));
    }
]

Number deg
[
    ^Degree value: self
]

Number rad
[
    ^Radian value: self
]
