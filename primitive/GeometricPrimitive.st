{ import: Primitive }
{ import: Intersection }

GeometricPrimitive : Primitive ( shape material areaLight lastRay refined )

GeometricPrimitive shape: s material: m areaLight: l
[
    shape := s.
    material := m.
    areaLight := l
]

GeometricPrimitive canHit: ray
[
    ^ray ~== lastRay
]

GeometricPrimitive lastNotHitRay: ray
[
    lastRay := ray
]

GeometricPrimitive refined: primitiveRefine 
[
    refined := primitiveRefine
]

GeometricPrimitive refine
[
    ^refined ifNil: [ 
        shape refine collect: [:ishape |
            self shape: ishape material: material areaLight: areaLight
        ]
    ]
]

GeometricPrimitive areaLight [ ^areaLight ]

GeometricPrimitive canIntersect [ ^shape canIntersect ]

GeometricPrimitive bsdf: diff
[
    ^material bsdf: (shape shading: diff)
]

GeometricPrimitive isIntersecting: ray
[
    ^shape isIntersecting: ray
]

GeometricPrimitive worldBbox
[
    ^shape worldBbox
]

GeometricPrimitive intersect: ray ifFailed: failed
[
    | diff |
    diff := shape intersect: ray ifFailed: [^failed].
    ^Intersection differential: diff primitive: self
]

GeometricPrimitive objectToWorld: object
[
    ^shape objectToWorld: object
]

GeometricPrimitive worldToObject: object
[
    ^shape worldToObject: object
]
