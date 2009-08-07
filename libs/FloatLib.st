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
        memcpy(&d v_self, sizeof(double));
        d = acos(d);
        memcpy(v_self, $d, sizeof(double));
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
