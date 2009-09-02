{ import: st80 }

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
    primitives do: [:each |
        each canIntersect ifTrue: [
            | diff |
            diff := each intersect: ray ifFailed: [each lastNotHitRay: ray. false].
            (diff ~== false) ifTrue: []
        ]
    ]
]
