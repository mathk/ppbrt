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
