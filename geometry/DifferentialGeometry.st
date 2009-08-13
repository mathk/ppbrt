{ import: Normal }
{ import: st80 }

DifferentialGeometry : Object ( u v shape p n dpdu dpdv dndu dndv hitP )

DifferentialGeometry shape: s hit: phit
[
    self := self new.
    shape := s.
    hitP := phit
]

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

DifferentialGeometry u: aU [ u := aU ]
DifferentialGeometry v: aV [ v := aV ]
DifferentialGeometry dpdu: vector [ dpdu := vector ]
DifferentialGeometry dpdv: vector [ dpdv := vector ]
DifferentialGeometry dndu: vector [ dndu := vector ]
DifferentialGeometry dvdv: vector [ dndv := vector ]
DifferentialGeometry dpdu [ ^dpdu ]
DifferentialGeometry dpdv [ ^dpdv ]
DifferentialGeometry dndu [ ^dndu ]
DifferentialGeometry dvdv [ ^dndv ]
DifferentialGeometry u [ ^u ]
DifferentialGeometry v [ ^v ]

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
