{ import: Normal }
{ import: st80 }

DifferentialGeometry : Object ( u v shape p n dpdu dpdv dndu dndv rayT )

DifferentialGeometry initialize
[
    u := 0.
    v := 0
]


DifferentialGeometry normal
[
    ^n ifNull: [
        | norm |
        norm := (dpdu cross: dpdv) normalize.
        n := Normal x: norm x y: norm y z: norm z.
    ]
]

DifferentialGeometry rayT [ ^rayT ]
