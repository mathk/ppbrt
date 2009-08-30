{ import: st80 }

Number multiplicationToNormal: aNormal
[
    ^aNormal x: aNormal x * self y: aNormal y * self z: aNormal z * self
]

Number additionToNormal: aNormal
[
    ^aNormal x: aNormal x + self y: aNormal y + self z: aNormal z + self
]

Number multiplicationToPoint: aPoint
[
    ^aPoint x: aPoint x * self y: aPoint y * self z: aPoint z * self
]

Number multiplicationToMatrix: aMatrix
[
    ^aMatrix collect: [:elt | elt * self ]
]

Number divisionToNormal: aNormal
[
    self = 0 ifTrue:[self error: 'Division by zero not allowed'].
    ^aNormal x: aNormal x / self y: aNormal y / self z: aNormal z / self
]

Number divisionToPoint: aPoint
[
    self = 0 ifTrue:[self error: 'Division by zero not allowed'].
    ^aPoint x: aPoint x / self y: aPoint y / self z: aPoint z / self
]

Number multToNormal: aNormal
[
    aNormal x: aNormal x * self;
            y: aNormal y * self;
            z: aNormal z * self
]

Number divToNormal: aNormal
[
    aNormal x: aNormal x / self;
            y: aNormal y / self;
            z: aNormal z / self
]

Number divToPoint: aPoint
[
    aPoint x: aPoint x / self;
            y: aPoint y / self;
            z: aPoint z / self
]

Number clampMin: min max: max
[
    self > max ifTrue: [ ^max ].
    self < min ifTrue: [ ^min ]
]

Number sign [ self negative ifTrue: [ ^1.0 negated ] ifFalse: [ ^1 ] ]
Number acos [ ^self asFloat acos]
Number atan2: x [ ^self asFloat atan2: x asFloat ]
Number cos [ ^self asFloat cos ]
