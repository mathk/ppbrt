{ import: Normal }
{ import: st80 }
"{ import: Point }"

Vector : Normal ()

Vector name [ ^'V' ]

Vector volume [ ^x * y * z ]
Vector orthogonal
[
    self x abs > self y abs 
	ifTrue: [
	    ^(Vector x: self z y: 0.0 z: self x) normalize
        ]
        ifFalse: [
            ^(Vector x: 0.0 y: self z z: self y) normalize
        ]
    
]

Vector cross: aVector
[
    ^Vector x: (self y * aVector z) - (self z * aVector y)
            y: (self z * aVector x) - (self x * aVector z)
            z: (self x * aVector y) - (self y * aVector x)
]

Vector substractionToPoint: aPoint
[
    ^aPoint x: self x - aPoint x
            y: self y - aPoint y
            z: self z - aPoint z
]
 
Vector additionToPoint: aPoint
[
    ^aPoint x: self x + aPoint x
            y: self y + aPoint y
            z: self z + aPoint z
]

Vector addToPoint: aPoint
[
    aPoint x: x + aPoint x;
           y: y + aPoint y;
           z: z + aPoint z
]

Vector subToPoint: aPoint
[
    aPoint x: aPoint x - x;
           y: aPoint y - y;
           z: aPoint z - z
]

Vector applyTransformation: t
[
    ^self fromHomogeneous: (t transformVector: self toHomogeneous)
]
