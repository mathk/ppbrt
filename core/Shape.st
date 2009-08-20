{ import: QuadraticSolver }
{ import: DifferentialGeometry }
{ import: FloatLib }

Shape : Object ( objectToWorld reverseOrientation )

Shape toWorld: o2w orientation: ro
[
    objectToWorld := o2w.
    reverseOrientation := ro.
]

Shape bbox
[
    ^self subclassResponsibility: 
               'Should return the object space bounding box of the shape'
]

Shape worldBbox
[
    ^objectToWorld apply: self bbox
]

Shape toObjectSpace: anObject
[
    ^objectToWorld inverse apply: anObject
]

Shape toWorldSpace: anObject
[
    ^objectToWorld apply: anObject
]

"
Tell wether a ray can intersect the shape.
This is usefull for shape that need to be refine
befor computing intersect
"
Shape canIntersect [ ^true ]
Shape refine [ self subclassResponsibility: 'Use to refine object' ]

Shape isIntersect: ray 
[ 
    self intersect: ray ifFailed: [ ^false ].
    ^true
]

Shape intersect: ray [ ^self intersect: ray ifFailed: [ self error: 'Ray do not intersect the shape'  ] ]

Shape intersect: ray ifFailed: aBlock 
[
    ^self objectSpaceIntersect: 
            (self toObjectSpace: ray) ifFailed: aBlock
]

Shape objectSpaceIntersect: ray ifFailed: aBlock  
[
    | hitPoint |
    hitPoint := self findRayHit: ray ifFailed: [ ^aBlock value ].
    ^self differntialAt: hitPoint ifFailed: [ ^aBlock value ]
]

Shape findRayHit: ray ifFailed: aBlock
[
    ^self subclassResponsibility: 'Should return the hit point'.
]

Shape differntialAt: p ifFailed: failed
[
    | diff |
    diff := DifferentialGeometry shape: self hit: p.
    self fillDifferentialGeometric: diff at: p ifFailed: [ ^failed value ].
    ^diff
]

Shape fillDifferentialGeometric: diff at: p ifFailed: failed
[
    self addParametric: diff at: p ifFailed: [ ^failed value ].
    self addDerivativesParametric: diff at: p ifFailed: [ ^failed value ].
    self addDerivativesNormal: diff at: p ifFailed: [ ^failed value ]
]

Shape addParametric: diff at: p ifFailed: failed
[
    self addParametric: diff at: p
]

Shape addDerivativesParametric: diff at: p ifFailed: failed
[
    self addDerivativesParametric: diff at: p
]

Shape addDerivativesNormal: diff at: p ifFailed: failed
[
    self addDerivativesNormal: diff at: p
]

Shape shading: aDiffGeo [ ^aDiffGeo ]
Shape area [ ^0.0 ]

Shape quadraticFindRayHit: ray a: a b: b c: c ifFailed: failed
[
    | solve t0 t1 |
    solve := QuadraticSolver solveA: a b: b c: c.
    solve hasSolution ifFalse: [ ^failed value ].
    t0 := solve firstSolution.
    t1 := solve secondSolution.
    (t0 > ray maxt or: [t1 < ray mint])
        ifTrue: [ ^failed value ].
    ^self hitPoint: ray at: solve firstSolution ifFailed: [
        self  hitPoint: ray at: solve secondSolution ifFailed: [ ^failed value ]
    ]
]

Shape hitPoint: aRay at: t ifFailed: failed
[
    | hitPoint |
    (aRay checkInBound: t) ifTrue: [ ^failed value ].
    hitPoint := (aRay at: t).
    (self clipingCheck: hitPoint) ifFalse: [ ^failed value ].
    ^hitPoint
]

Shape clipingCheck: hitPoint
[
    ^self subclassResponsibility: 'Return if the point can hit the shape'.
]
