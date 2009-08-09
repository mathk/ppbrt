{ import: FloatLib }

Shape : Object ( objectToWorld reverseOrientation )

Shape bbox
[
    ^self subclassResponsibility: 
               'Should return the object space bounding box of the shape'
]

Shape worldBbox
[
    ^objectToWorld apply: self bbox
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
Shape intersect: ray ifFailed: aBlock [ self subclassResponsibility: 'Return intersection information' ]

Shape shading: aDiffGeo [ ^aDiffGeo ]
Shape area [ ^0.0 ]
