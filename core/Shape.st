{ import: st80 }

Shape : Object ( objectToWorld reverseOrientation )

Shape bbox
[
    ^self subclassResponsibility: 'Should return the object space bounding box of the shape'
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
Shape canIntersect
[
    ^true
]

Shape refine
[
    self subclassResponsibility: 'Use to refine object'
]
