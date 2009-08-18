{ import: Vector }
{ import: Matrix }

Point : Object ( x y z cosphi sinphi phi theta u v )

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

Point initialize
[
    u := 0.0.
    v := 0.0
]

Point toVector [ ^Vector x: self x y: self y z: self z ]

Point u [ ^u ]
Point v [ ^v ]
Point u: aU [ u := aU ]
Point v: aV [ v := aV ]
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
Point xradius [ ^self dist: (self x: self x y: 0.0 z: 0.0) ]
Point yradius [ ^self dist: (self x: 0.0 y: self y z: 0.0) ]
Point zradius [ ^self dist: (self x: 0.0 y: 0.0 z: self z) ]
Point radius [ ^self dist: (self origin)]

Point cosphi 
[
    ^cosphi ifNil: [ cosphi := self x / self zradius ]
]

Point sinphi
[
    ^sinphi ifNil: [ sinphi := self y / self zradius ]
]

"
Phi is the angle between Ox and the projection of 
the point on xOy.
The answer is always between 0 and 2*pi
"
Point phi 
[
    ^phi ifNil: [ | phiout |
        phiout := self cosphi acos.
        phiout negative ifTrue: [ phiout := phiout + (2 * Float pi) ].
        'Phi : ' put.
        phiout putln.
        phi := phiout
    ]
]

"
Angle between Op and the plan xOy
The answer is between 0 and pi
"
Point theta
[
    ^theta ifNil: [ theta := (self z / self radius) acos. 'Theta: 'put. theta putln;yourself]
]

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

