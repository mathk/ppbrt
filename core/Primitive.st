{ import: st80 }

Primitive : Object ()

Primitive depRefine
[
    | result |
    result := OrderedCollection new.
    self refine do: [:each |
        each canIntersect ifFalse: [ result addAll: each depRefine ]
    ].
    ^result
]
