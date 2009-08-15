{ import: Normal }
{ import: st80 }

DifferentialGeometry : Object ( u v shape p n dpdu dpdv dndu dndv )

DifferentialGeometry shape: s hit: phit
[
    self := self new.
    shape := s.
    p := phit
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
    p := phit
]

DifferentialGeometry u: aU [ u := aU ]
DifferentialGeometry v: aV [ v := aV ]
DifferentialGeometry dpdu: vector [ dpdu := vector ]
DifferentialGeometry dpdv: vector [ dpdv := vector ]
DifferentialGeometry dndu: vector [ dndu := vector ]
DifferentialGeometry dndv: vector [ dndv := vector ]
DifferentialGeometry dpdu [ ^dpdu ]
DifferentialGeometry dpdv [ ^dpdv ]
DifferentialGeometry dndu [ ^dndu ]
DifferentialGeometry dndv [ ^dndv ]
DifferentialGeometry u [ ^u ]
DifferentialGeometry v [ ^v ]

DifferentialGeometry initialize
[
    u := 0.
    v := 0
]


DifferentialGeometry normal
[
    ^n ifNil: [
        | norm |
        norm := (dpdu cross: dpdv) normalize.
        n := Normal x: norm x y: norm y z: norm z.
    ]
]

DifferentialGeometry p [ ^p ]

DifferentialGeometry printOn: aStream
[
    aStream nextPutAll: 'Hit at: ';
            print: p;
            nextPutAll: ' Normal: ';
            print: self normal
]
