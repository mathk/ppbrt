{ import: Shape }

Triangle : Shape ( p1 p2 p3 n1 n2 n3 uv1 uv2 uv3 )

Triangle toWorld: o2w orientation: ro p1: point1 p2: point2 p3: point3
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

Triangle findRayHit: aRay ifFailed: failed
[
    | e1 e2 s1 divisor |
    e1 := p2 - p1.
    e2 := p3 - p1.
    s1 := aRay direction cross: e2.
    divisor := s1 dot: e1.
    divisor = 0.0 ifTrue: [ ^failed value ].
    
]
