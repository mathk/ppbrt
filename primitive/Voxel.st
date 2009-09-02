{ import: GridAccelerator }

Voxel : Object (primitives)

Voxel initialize
[
    primitives := OrderedCollection new: 30
]

Voxel addPrimitive: aPrimitive
[
    primitives add: aPrimitive
]

Voxel intersect: ray ifFailed: failed
[
    | intersection |
    intersection := nil.
    primitives do: [:each |
        each canIntersect ifFalse: [
            each refined: (GridAccelerator primitives: each refine refineBefor: false)
        ].
        (each canHit: ray) ifTrue: [
            intersection := each intersect: ray ifFailed: [each lastNotHitRay: ray. intersection].
        ]
    ].
    intersection ifNil: [^failed value].
    ^intersection
]
