{ import: NumberLib }
{ import: FloatLib }
{ import: ObjectLib }
{ import: Matrix }
{ import: Triplet }
{ import: st80}

Normal : Triplet ()

Normal name [ ^'N' ]

Normal + anObject [ ^anObject additionToNormal: self ]
Normal - anObject [ ^anObject substractionToNormal: self ]
Normal * anNumber [ ^anNumber multiplicationToNormal: self ]
Normal / anNumber [ ^anNumber divisionToNormal: self ]

Normal squaredLength [ ^(self x * self x) + (self y * self y) + (self z * self z) ]
Normal length [ ^self squaredLength sqrt ]
Normal negated [ ^self x: self x negated y: self y negated z: self z negated]
Normal normalize [ ^self / self length ]
Normal dot: aNormal [ ^(aNormal x * x) + (aNormal y * y) + (aNormal z * z) ]
Normal add: anObject [ anObject addToNormal: self ]
Normal sub: anObject [ anObject subToNormal: self ]
Normal mult: anObject [ anObject multToNormal: self ]
Normal div: anObject [ anObject divToNormal: self ]

Normal substractionToNormal: aNormal
[
    ^Normal x: aNormal x - x y: aNormal y - y z: aNormal z - z
]

Normal additionToNormal: aNormal
[
    ^Normal x: aNormal x + x y: aNormal y + y z: aNormal z + z
]

Normal subToNormal: aNormal
[
    aNormal x: aNormal x - x;
            y: aNormal y - y;
            z: aNormal z - z
]

Normal addToNormal: aNormal
[
    aNormal x: x + aNormal x;
            y: y + aNormal y;
            z: z + aNormal z
]

Normal adaptToFloat: aFloat andSend: operator
[
    ^self perform: operator with: aFloat
]

Normal adaptToInteger: anInteger andSend: operator
[
    ^self perform: operator with: anInteger
]

Normal toHomogeneous
[
    | matrix |
    matrix := Matrix rows: 4 cols: 1.
    matrix i: 1 j: 1 put: x.
    matrix i: 2 j: 1 put: y.
    matrix i: 3 j: 1 put: z.
    matrix i: 4 j: 1 put: 0.
    ^matrix
]

Normal fromHomogeneous: aMatrix
[
    ^self x: (aMatrix i: 1 j: 1)
          y: (aMatrix i: 2 j: 1)
          z: (aMatrix i: 3 j: 1)
]

Normal applyTransformation: t
[
    ^self fromHomogeneous: (t transformNormal: self toHomogeneous)
]

