{ import: Point }
{ import: st80 }

CoordinateSystem : Object( v1 v2 v3)

CoordinateSystem v1 [ ^v1 ]
CoordinateSystem v2 [ ^v2 ]
CoordinateSystem v3 [ ^v3 ]

CoordinateSystem fromV1: aVector
[
    self assert: [ aVector length printString = 1.0 printString ].
    self := super new.
    v1 := aVector.
    v2 := aVector orthogonal.
    v3 := v1 cross: v2
]

CoordinateSystem printOn: aStream
[
    aStream nextPutAll: 'v1: ';
            print: v1; nextPut: $\n;
            nextPutAll: 'v2: ';
            print: v2; nextPut: $\n;
            nextPutAll: 'v3: ';
            print: v3
]

