{ import: Primitive }

GeometricPrimitive : Primitive ( shape material areaLight )

GeometricPrimitive shape: s material: m areaLight: l
[
    shape := s.
    material := m.
    areaLight := l
]

GeometricPrimitive intersect: ray
[

]
