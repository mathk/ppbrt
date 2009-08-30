{ import: DifferentialGeometry }
{ import: CoordinateSystem }

DifferentialTriangleGeometry : DifferentialGeometry (b0 b1 b2 e1 e2)

DifferentialTriangleGeometry e1 [ ^e1 ifNil: [ e1 := shape p2 - shape p1 ] ]
DifferentialTriangleGeometry e2 [ ^e2 ifNil: [ e2 := shape p3 - shape p1 ] ]
DifferentialTriangleGeometry b0 [ ^b0 ifNil: [ b0 := 1 - b1 - b2] ]
DifferentialTriangleGeometry b1 [ ^b1 ]
DifferentialTriangleGeometry b2 [ ^b2 ]
DifferentialTriangleGeometry b0: aInt [ b0 := aInt ]
DifferentialTriangleGeometry b1: aInt [ b1 := aInt ]
DifferentialTriangleGeometry b2: aInt [ b2 := aInt ]

DifferentialTriangleGeometry computeBarycentricHit: aRay ifFailed: failed
[
    | s1 s2 s divisor invDivisor t |
    s1 := aRay direction cross: self e2.
    divisor := s1 dot: self e1.
    divisor = 0.0 ifTrue: [ ^failed value ].
    invDivisor := 1.0 / divisor.
    s := aRay origin - shape p1.
    b1 := (s dot: s1) * invDivisor.
    (self b1 < 0.0 or: [ self b1 > 1.0]) ifTrue: [ ^failed value ].
    s2 := s cross: self e1.
    b2 := (aRay direction dot: s2) * invDivisor.
    (self b2 < 0.0 or: [ (self b2 + self b1) > 1.0]) ifTrue: [ ^failed value ].
    t := (e2 dot: s2) * invDivisor.
    ^t
]

DifferentialTriangleGeometry computeParametric
[
    self u: (self b0 * shape p1 u) + (self b1 * shape p2 u) + (self b2 * shape p3 u).
    self v: (self b0 * shape p1 v) + (self b1 * shape p2 v) + (self b2 * shape p3 v).
]

DifferentialTriangleGeometry computeDerivativesParametric
[
    | a b |
    a := Matrix rows: 2 cols: 2.
    b := Matrix rows: 2 cols: 1.
    b i: 1 j: 1 put: (shape p1 - shape p3).
    b i: 2 j: 1 put: (shape p2 - shape p3).
    a i: 1 j: 1 put: (shape p1 u - shape p3 u).
    a i: 1 j: 2 put: (shape p1 v - shape p3 v).
    a i: 2 j: 1 put: (shape p2 u - shape p3 u).
    a i: 2 j: 2 put: (shape p2 v - shape p3 v).
    a det = 0.0 
        ifTrue: [
            | cs |
            cs := CoordinateSystem fromV1: (e1 cross: e2) normalize.
            self dpdu: cs v2.
            self dpdv: cs v3
        ]
        ifFalse: [
            | solve |
            solve := (a inverse * b).
            self dpdu: (solve i:1 j: 1).
            self dpdv: (solve i:2 j: 1)
        ]
]
