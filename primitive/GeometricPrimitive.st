{ import: Primitive }

GeometricPrimitive : Primitive ( shape material areaLight )

GeometricPrimitive shape: s material: m areaLight: l
[
    shape := s.
    material := m.
    areaLight := l
]

GeometricPrimitive refine
[
    ^shape refine collect: [:ishape |
        self shape: ishape material: material areaLight: areaLight
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

GeometricPrimitive intersect: ray
[

]
