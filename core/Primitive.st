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

Primitive isIntersecting: ray
[
    self intersect: ray ifFailed: [^false].
    ^true
]

Primitive intersect: ray ifFailed: failed
[
    self subclassResponsibility: 'Return the intersection of the ray if exist'
]
