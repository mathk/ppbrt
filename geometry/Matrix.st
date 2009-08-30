{ import: IntegerLib }
{ import: NumberLib }
{ import: FloatLib }
{ import: ObjectLib }


Matrix : Object ( array minors coMatrix imax jmax inverse )

Matrix rows: i cols: j
[
    self := super new.
    array := Array new: i * j.
    minors := Array new: i * j.
    imax := i.
    jmax := j
]

Matrix rows: i cols: j values: anArray
[
    self := super new.
    self assert: [ anArray size = (i * j)].
    array := anArray.
    minors := Array new: i * j.
    imax := i.
    jmax := j
]

Matrix rows: i cols: j values: anArray inverse: inverseMat
[
    self := self rows: i cols: j values: anArray.
    inverse inverse: self.
    inverse := inverseMat.
]

Matrix identity
[
    | idt |
    idt := self rows: self rows cols: self cols.
    idt indexDo: [:i :j :v |
        i = j ifTrue:[idt i: i j: j put: 1] 
              ifFalse: [ idt i: i j: j put: 0 ]
    ].
    ^idt
]

Matrix inverse: aMatrix [ inverse := aMatrix ]
Matrix rows [ ^imax ]
Matrix cols [ ^jmax ]

Matrix i: i do: aBlock
[
    1 to: self cols do: [:j |
        aBlock value: (self i: i j: j). 
    ] 
]

Matrix indexI: i do: aBlock
[
    1 to: self cols do: [:j |
        aBlock value: j value: (self i: i j: j). 
    ] 
]

Matrix j: j do: aBlock
[
    1 to: self rows do: [:i |
        aBlock value: (self i: i j: j)
    ]
]

Matrix minorI: i j: j
[
    (minors at: (self privIndexI: i j: j)) ifNil: [
        minors at: (self privIndexI: i j: j) 
               put: (self removeI: i j: j) det
    ].
    ^minors at: (self privIndexI: i j: j)
]

Matrix removeI: i j: j
[
    | newValues place |
    newValues := Array new: (self rows - 1) * (self cols - 1).
    place := 1.
    1 to: self rows do: [ :ci |
        i = ci ifFalse: [
            1 to: self cols do: [:cj |
                j = cj ifFalse:[
                    newValues at: place put: (self i: ci j: cj).
                    place := place + 1
                ]
            ]
        ]
    ].
    ^self rows: (self rows - 1) cols: (self cols - 1) values: newValues
]

Matrix do: aBlock
[
    array do: aBlock.
]

Matrix indexDo: aBlock
[
    1 to: self rows do: [ :i |
        1 to: self  cols do: [:j |
            aBlock value: i value: j value: (self i: i j: j)
        ]
    ]
]

Matrix collect: aBlock
[
    ^self rows: imax cols: jmax values: (array collect: aBlock)
]

Matrix privIndexI: i j: j
[
    ^((jmax * (i - 1)) + j)
]

Matrix i: i j: j
[
    self assert: [ i <= imax and: [j <= jmax]].
    ^array at: (self privIndexI: i j: j)
]

Matrix i: i j: j put: value
[
    self assert: [ i <= imax and: [j <= jmax]].
    array at: (self privIndexI: i j: j) put: value.
]

Matrix * anObject
[
    | m |
    ^anObject multiplicationToMatrix: self.
]

Matrix multiplicationToMatrix: aMatrix
[
    | m |
    self assert: [ aMatrix cols = self rows ].
    m := self rows: aMatrix rows cols: self cols. 
    1 to: aMatrix rows do: [ :i |
        1 to: self cols do: [ :j |
            | v |
            v := 0.0.
            1 to: aMatrix cols do: [:mid |
                v := v + ((aMatrix i: i j: mid) * (self i: mid j: j)).
            ].
            m i: i j: j put: v
        ]
    ].
    ^m
]

Matrix adaptToFloat: aFloat andSend: operator
[
    ^self perform: operator with: aFloat
]

Matrix adaptToInteger: anInteger andSend: operator
[
    ^self perform: operator with: anInteger
]

Matrix det
[
    | det |
    det := 0.0.
    self assert: [self cols = self rows].
    self rows = 1 ifTrue:[^self i: 1 j: 1].
    self indexI: 1 do: [:j :v | 
        | a |
        a := j odd ifTrue:[ v ] ifFalse: [ v negated ].
        det := det + (a * (self minorI: 1 j: j))
    ].
    ^det
]

Matrix printOn: aStream
[
    1 to: self rows do: [ :i |
        aStream nextPut: $(.
        self i: i do: [:v |
            aStream nextPut: $ ;
	                print: v;
	                nextPut: $ 
        ].
	    aStream nextPut: $);
	            nextPut: $\n.
    ]   
]

Matrix coMatrix
[
    coMatrix ifNil: [
        coMatrix := self rows: self rows cols: self cols.
        self indexDo: [:i :j :v |
            | sign |
            sign := (i + j) even ifTrue: [1] ifFalse: [-1].
            coMatrix i: i j: j put: sign * (self minorI: i j: j)
        ]
    ].
    ^coMatrix
]

Matrix t
[
    | transpose |
    transpose := self rows: self cols cols: self rows.
    self indexDo: [:i :j :v |
        transpose i: j j: i put: v
    ].
    ^transpose
]

Matrix inverse
[
    inverse ifNil: 
    [
        | det |
        det := self det.
        self assert: [det ~= 0].
        inverse := (1/det) * self coMatrix t.
        inverse inverse: self.
    ].
    ^inverse
]
