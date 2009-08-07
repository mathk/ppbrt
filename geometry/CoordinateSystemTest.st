{ import: CoordinateSystem }
{ import: st80 }

[
    | cs |
    cs := CoordinateSystem fromV1: (Vector x: 1.0 y: 1.0 z: 0.0) normalize.
    cs putln
]
