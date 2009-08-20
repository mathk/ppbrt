{ import: Shape }
{ import: Matrix }
{ import: DifferentialTriangleGeometry }
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

Triangle p1 [ ^p1 ]
Triangle p2 [ ^p2 ]
Triangle p3 [ ^p3 ]

Triangle intersect: ray ifFailed: aBlock
[
    ^ self objectSpaceIntersect: ray ifFailed: aBlock
]

Triangle objectSpaceIntersect: aRay ifFailed: failed
[
    | t  phit diff |
    diff := DifferentialTriangleGeometry shape: self.
    t := diff computeBarycentricHit: aRay ifFailed: [ ^failed value ].
    phit := self hitPoint: aRay at: t ifFailed: [ ^failed value ].
    diff p: phit.
    self fillDifferentialGeometric: diff at: phit ifFailed: [ ^failed value ]
    ^diff
]

Triangle addDerivativesParametric: diff at: p
[
    diff computeDerivativesParametric
]

Triangle addParametric: diff at: p
[
    diff computeParametric.
]

Triangle addDerivativesNormal: diff at: p
[
    diff dndu: Vector null.
    diff dndv: Vector null
]


Triangle clipingCheck: hitPoint [ ^true ]


Triangle area
[
    ^0.5 * ((p2 - p1) cross: p3 - p1) length
]
