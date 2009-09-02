{ import: st80 }

Primitive : Object ( lastRay )

Primitive depRefine
[
    | result |
    result := OrderedCollection new.
    self refine do: [:each |
        each canIntersect ifFalse: [ result addAll: each depRefine ]
    ].
    ^result
]

Primitive canHit: ray
[
    ^ray ~== lastRay
]

Primitive lastNotHitRay: ray
[
    lastRay := ray
]
