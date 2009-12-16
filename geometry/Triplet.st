{ import: st80 }

Triplet : Object ( x y z )

Triplet x: anX y: anY z: anZ
[
    self := super new.
    x := anX.
    y := anY.
    z := anZ
]

Triplet + aNumber [
    ^self x: self x + aNumber y: self y + aNumber z: self z + aNumber
]

Triplet - aNumber [
    ^self x: self x - aNumber y: self y - aNumber z: self z - aNumber
]

Triplet xyz: aNum 
[
    self := super new.
    x := aNum.
    y := aNum.
    z := aNum
]

Triplet new [ {printf(_backtrace());}. self error: 'Use x:y:z: insthead' ]

Triplet x [ ^x ]
Triplet y [ ^y ]
Triplet z [ ^z ]

Triplet x: anX [ x := anX ]
Triplet y: anY [ y := anY ]
Triplet z: anZ [ z := anZ ]

Triplet do: aBlock
[
    aBlock value: self x.
    aBlock value: self y.
    aBlock value: self z
]

Triplet to: endTriplet do: aBlock
[
    self x to: endTriplet x do: [:xcoord|
        self y to: endTriplet y do: [:ycoord |
            self z to: endTriplet z do: [:zcoord |
                aBlock value: (self x: xcoord y: ycoord z: zcoord)
            ]
        ]
    ]

]

Triplet withIndexDo: aBlock
[
    aBlock value: 1 value: self x.
    aBlock value: 2 value: self y.
    aBlock value: 3 value: self z
]

Triplet at: index put: value
[
    index = 1 ifTrue: [x := value].
    index = 2 ifTrue: [y := value].
    index = 3 ifTrue: [z := value].
]

Triplet at: index
[
    index = 1 ifTrue: [ ^x ].
    index = 2 ifTrue: [ ^y ].
    index = 3 ifTrue: [ ^z ].
]

Triplet with: aTriplet do: aBlock
[
    aBlock value: self x value: aTriplet x.
    aBlock value: self y value: aTriplet y.
    aBlock value: self z value: aTriplet z
]

Triplet with: aTriplet1 with: aTriplet2 do: aBlock
[
    aBlock value: self x value: aTriplet1 x value: aTriplet2 x.
    aBlock value: self y value: aTriplet1 y value: aTriplet2 y.
    aBlock value: self z value: aTriplet1 z value: aTriplet2 z
]

Triplet with: aTriplet1 with: aTriplet2 with: aTriplet3 do: aBlock
[
    aBlock value: self x value: aTriplet1 x value: aTriplet2 x value: aTriplet3 x.
    aBlock value: self y value: aTriplet1 y value: aTriplet2 y value: aTriplet3 y.
    aBlock value: self z value: aTriplet1 z value: aTriplet2 z value: aTriplet3 z
]

Triplet printOn: aStream
[
    aStream
       nextPutAll: self name, '(';
       print: self x;
       nextPutAll: ', ';
       print: self y;
       nextPutAll: ', ';
       print: self z;
       nextPutAll: ')'
]
