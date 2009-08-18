{ import: Shape }
{ import: Matrix }
{ import: DifferentialGeometry }
{ import: CoordinateSystem }

Triangle : Shape ( p1 p2 p3 n1 n2 n3 uv1 uv2 uv3 )

Triangle toWorld: o2w orientation: ro p1: point1 p2: point2 p3: point3 mesh: tmesh
         "n1: normal1 n2: normal2 n3: normal3 uv1: parametric1 uv2: parametric2 uv3: parametric3"
[
    self := self toWorld: o2w orientation: ro.
    p1 := point1.
    p2 := point2.
    p3 := point3
]

Triangle intersect: ray ifFailed: aBlock
[
    ^ self objectSpaceIntersect: ray ifFailed: aBlock
]

Triangle objectSpaceIntersect: aRay ifFailed: failed
[
    | t  e1 e2 s1 b0 b1 b2 s s2 divisor invDivisor phit diff a b |
    e1 := p2 - p1.
    e2 := p3 - p1.
    s1 := aRay direction cross: e2.
    divisor := s1 dot: e1.
    divisor = 0.0 ifTrue: [ ^failed value ].
    invDivisor := 1.0 / divisor.
    s := aRay origin - p1.
    b1 := (s dot: s1) * invDivisor.
    (b1 < 0.0 and: [ b1 > 1.0]) ifTrue: [ ^failed value ].
    s2 := s cross: e1.
    b2 := (aRay direction dot: s2) * invDivisor.
    (b2 < 0.0 and: [ (b2 + b1) > 1.0]) ifTrue: [ ^failed value ].
    t := (e2 dot: s2) * invDivisor.
    phit := self hitPoint: aRay at: t ifFailed: [ ^failed value ].  
    b0 := 1 - b1 - b2.
    phit u: (b0 * p1 u) + (b1 * p2 u) + (b2 * p3 u).
    phit v: (b0 * p1 v) + (b1 * p2 v) + (b2 * p3 v).
    diff := DifferentialGeometry shape: self hit: phit.
    a := Matrix rows: 2 cols: 2.
    b := Matrix rows: 2 cols: 1.
    b i: 1 j: 1 put: (p1 - p3).
    b i: 2 j: 1 put: (p2 - p3).
    a i: 1 j: 1 put: (p1 u - p3 u).
    a i: 1 j: 2 put: (p1 v - p3 v).
    a i: 2 j: 1 put: (p2 u - p3 u).
    a i: 2 j: 2 put: (p2 v - p3 v).
    a det = 0.0 
        ifTrue: [
            | cs |
            cs := CoordinateSystem fromV1: (e1 cross: e2) normalize.
            diff dpdu: cs v2.
            diff dpdv: cs v3
        ]
        ifFalse: [
            | solve |
            solve := (a inverse * b).
            diff dpdu: solve i:1 j: 1.
            diff dpdv: solve i:2 j: 1
        ]
    diff dndu: Vector null.
    diff dndv: Vector null.
    ^diff
]

Triangle clipingCheck: hitPoint [ ^true ]


Triangle area
[
    ^0.5 * ((p2 - p1) cross: p3 - p1) length
]
