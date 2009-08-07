{ import: Point }
{ import: Matrix }

Transform : Object ( matrix inverseTransform )

Transform inverse
[
    ^inverseTransform ifNil: 
    [
        inverseTransform := self new.
        inverseTransform matrix: matrix inverse; yourself
    ]
]

Transform translat: delta
[
    | inverse |
    self := self new.   
    matrix := (Matrix rows: 4 cols: 4) identity.
    matrix i: 1 j: 4 put delta x.
    matrix i: 2 j: 4 put delta y.
    matrix i: 3 j: 4 put delta z.
    inverse := matrix identity.
    inverse i: 1 j: 4 put delta x negated.
    inverse i: 2 j: 4 put delta y negated.
    inverse i: 3 j: 4 put delta z negated.
    matrix inverse: inverse.
    inverse inverse: matrix.
]

Transform scale: factor
[
    | inverse |
    self := self new. 
    matrix := (Matrix rows: 4 cols: 4) identity.
    matrix i: 1 j: 1 put: factor x.
    matrix i: 2 j: 2 put: factor y.
    matrix i: 3 j: 3 put: factor z.
    inverse := matrix identity.
    inverse i: 1 j: 1 put: 1 / factor x.
    inverse i: 2 j: 2 put: 1 / factor y.
    inverse i: 3 j: 3 put: 1 / factor z.
    matrix inverse: inverse.
    inverse inverse: matrix.   
]

Transform rotX: teta
[
    | inverse cos sin |
    self := self new. 
    cos := teta rad cos.
    sin := teta rad sin.
    matrix := (Matrix rows: 4 cols: 4) identity.
    matrix i: 2 j: 2 put: cos.
    matrix i: 2 j: 3 put: sin negated.
    matrix i: 3 j: 2 put: sin.
    matrix i: 3 j: 3 put: cos.
    inverse := matrix t.
    matrix inverse: inverse.
    inverse inverse: matrix.   
]

Transform rotY: teta
[
    | inverse cos sin |
    self := self new. 
    cos := teta rad cos.
    sin := teta rad sin.
    matrix := (Matrix rows: 4 cols: 4) identity.
    matrix i: 1 j: 1 put: cos.
    matrix i: 1 j: 3 put: sin.
    matrix i: 3 j: 1 put: sin negated.
    matrix i: 3 j: 3 put: cos.
    inverse := matrix t.
    matrix inverse: inverse.
    inverse inverse: matrix.   
]

Transform rotZ: teta
[
    | inverse cos sin |
    self := super new. 
    cos := teta rad cos.
    sin := teta rad sin.
    matrix := (Matrix rows: 4 cols: 4) identity.
    matrix i: 1 j: 1 put: cos.
    matrix i: 1 j: 2 put: sin negated.
    matrix i: 2 j: 1 put: sin.
    matrix i: 2 j: 2 put: cos.
    inverse := matrix t.
    matrix inverse: inverse.
    inverse inverse: matrix.   
]

Transform rotAxis: axis  angle: teta
[
    | inverse cos sin d |
    self := self new. 
    d := axis normalize.
    cos := teta rad cos.
    sin := teta rad sin.
    matrix := (Matrix rows: 4 cols: 4) identity.
    matrix i: 1 j: 1 put: (d x * d x * (1.0 - cos)) + cos.
    matrix i: 1 j: 2 put: (d x * d y * (1.0 - cos)) - (d z * sin).
    matrix i: 1 j: 3 put: (d x * d z * (1.0 - cos)) + (d y * sin).

    matrix i: 2 j: 1 put: (d y * d x * (1.0 - cos)) + (d z * sin).
    matrix i: 2 j: 2 put: (d y * d y * (1.0 - cos)) + cos .
    matrix i: 2 j: 3 put: (d y * d z * (1.0 - cos)) - (d x * sin).

    matrix i: 3 j: 1 put: (d z * d x * (1.0 - cos)) - (d y * sin).
    matrix i: 3 j: 2 put: (d z * d y * (1.0 - cos)) + (d x * sin).
    matrix i: 3 j: 3 put: (d z * d z * (1.0 - cos)) + cos.
    inverse := matrix t.
    matrix inverse: inverse.
    inverse inverse: matrix.
]

Transform lookAt: look from: position up: upDir
[
    | inverse dir right up |
    self := self new. 
    dir := (look - position) normalize.
    right := dir cross: upDir normalize.
    up := right cross: dir.
    matrix := (Matrix rows: 4 cols: 4) identity.
    matrix i: 1 j: 4 put: position x.
    matrix i: 2 j: 4 put: position y.
    matrix i: 3 j: 4 put: position z.

    matrix i: 1 j: 3 put: dir x.
    matrix i: 2 j: 3 put: dir y.
    matrix i: 3 j: 3 put: dir z.

    matrix i: 1 j: 2 put: up x.
    matrix i: 2 j: 2 put: up y.
    matrix i: 3 j: 2 put: up z.
    
    matrix i: 1 j: 1 put: right x.
    matrix i: 2 j: 1 put: right y.
    matrix i: 3 j: 1 put: right z.   

    matrix inverse: inverse.
    inverse inverse: matrix.
]

Transform apply: element
[
    ^element applyTransformation: self
]

Transform applyInverse: element
[
    ^self inverse apply: element
]

Transform applyTransformation: t
[
    | m |
    m := t matrix * self matrix.
    m inverse: self matrix inverse * t matrix inverse.
    self := super new.
    matrix := m
]

Transform transformNormal: normal
[
    ^matrix inverse t * normal
]

Transform transformVector: vector
[
    ^matrix * vector
]

Transform transformPoint: point
[
    | new |
    new := matrix * point.
    self assert: [(new i: 4 j: 1) ~= 0].
    ^new
]

Transform isHandednessSwaped
[
    ^(matrix removeI: 4 j: 4) det < 0
]

Transform matrix [ ^matrix ]
Transform matrix: m [ ^matrix := m ]

Transform printOn: aStream
[
    aStream nextPutAll: 'T : ';
            nextPut: $\n;
            print: matrix
]
