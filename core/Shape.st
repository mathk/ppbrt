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
