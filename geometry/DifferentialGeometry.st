{ import: Normal }
{ import: st80 }

DifferentialGeometry : Object ( u v shape p n dpdu dpdv dndu dndv hitP )

DifferentialGeometry shape: s hit: phit u: anU v: anV 
                     dpdu: aDpdu dpdv: aDpdv dndu: aDndu dndv: aDndv
[
    self := self new.
    dpdu := aDpdu.
    dpdv := aDpdv.
    dndu := aDndu.
    dndv := aDndv.
    shape := s.
    u := anU.
    v := anV.
    hitP := phit
]

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

DifferentialGeometry p [ ^p ]
