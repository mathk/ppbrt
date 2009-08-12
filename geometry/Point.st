{ import: Vector }
{ import: Matrix }

Point : Object ( x y z )

Point new
[
    ^self x: 0.0 y: 0.0 z: 0.0
]

Point origin
[
    ^self x: 0.0 y: 0.0 z: 0.0
]

Point x: anX y: anY z: anZ
[
    self := super new.
    x := anX.
    y := anY.
    z := anZ
]

Point x [ ^x ]
Point y [ ^y ]
Point z [ ^z ]

Point x: anX [ x := anX ]
Point y: anY [ y := anY ]
Point z: anZ [ z := anZ ]

Point toHomogeneous
[
    | array |
    array := Array new: 4.
    array at: 1 put: x.
    array at: 2 put: y.
    array at: 3 put: z.
    array at: 4 put: 1.
    ^Matrix rows: 4 cols: 1 values: array
]

Point fromHomogeneous: aMatrix
[
    | adjust |
    adjust := 1 / (aMatrix i: 4 j: 1).
    ^self x: (aMatrix i: 1 j: 1) * adjust
          y: (aMatrix i: 2 j: 1) * adjust
          z: (aMatrix i: 3 j: 1) * adjust
]

Point + anObject [ ^anObject additionToPoint: self ]
Point - anObject [ ^anObject substractionToPoint: self ]
Point * anNumber [ ^anNumber multiplicationToPoint: self ]
Point / anNumber [ ^anNumber divisionToPoint: self ]

Point add: anObject [ ^anObject addToPoint: self ]
Point sub: anObject [ ^anObject subToPoint: self ]
Point mult: anObject [ anObject multToPoint: self ]
Point div: anObject [ anObject divToPoint: self ]
Point squaredDist: aPoint [ ^(self - aPoint) squaredLength ]
Point dist: aPoint [ ^(self - aPoint) length ]


Point substractionToPoint: aPoint
[
    ^Vector x: aPoint x - self x
            y: aPoint y - self y
            z: aPoint z - self z
]

Point additionToNormal: aNormal
[
    ^self x: self x + aNormal x
          y: self y + aNormal y
          z: self z + aNormal z
]

Point substractionToNormal: aNormal
[
    ^self x: aNormal x - x
          y: aNormal y - y
          z: aNormal z - z
]

Point unionBBox: aBBox
[
    ^aBBox unionBBox: (aBBox one: self)
]

Point printOn: aStream
[
    aStream
       nextPutAll: 'P(';
       print: self x;
       nextPutAll: ', ';
       print: self y;
       nextPutAll: ', ';
       print: self z;
       nextPutAll: ')'
]

Point applyTransformation: t
[
    ^self fromHomogeneous: (t transformPoint: self toHomogeneous)
]

"

Number multiplicationToPoint: aPoint
[
    ^aPoint x: aPoint x * self y: aPoint y * self z: aPoint z * self
]

Number divisionToPoint: aPoint
[
    self = 0 ifTrue:[self error: 'Division by zero not allowed'].
    ^aPoint x: aPoint x / self y: aPoint y / self z: aPoint z / self
]

Number multToPoint: aPoint
[
    aPoint x: aPoint x * self;
            y: aPoint y * self;
            z: aPoint z * self
]

Number divToPoint: aPoint
[
    aPoint x: aPoint x / self;
            y: aPoint y / self;
            z: aPoint z / self
]

"
