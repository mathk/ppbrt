{ import: DifferentialGeometry }

DifferentialTriangleGeometry : DifferentialTriangleGeometry (b0 b1 b2 e1 e2)

DifferentialTriangleGeometry e1 [ ^e1 ifNil: [ e1 := shape p2 - shape p1 ] ]
DifferentialTriangleGeometry e1 [ ^e2 ifNil: [ e2 := shape p3 - shape p1 ] ]
DifferentialTriangleGeometry b0 [ ^b0 ]
DifferentialTriangleGeometry b1 [ ^b1 ]
DifferentialTriangleGeometry b2 [ ^b2 ]
DifferentialTriangleGeometry b0: aInt [ b0 ifNil: [ b0 := 1 - b1 - b2 ] ]
DifferentialTriangleGeometry b1: aInt [ b1 := aInt ]
DifferentialTriangleGeometry b2: aInt [ b2 := aInt ]

DifferentialTriangleGeometry computeBarycentric: aRay ifFailed: failed
[

]
