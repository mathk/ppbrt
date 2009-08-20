{ import: Normal }
{ import: Vector }
{ import: st80 }

DifferentialGeometry : Object ( u v shape p n dpdu dpdv dndu dndv )

DifferentialGeometry shape: s
[
    self := self new.
    shape := s
]

DifferentialGeometry shape: s hit: phit
[
    self := self shape: s.
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
    v := 0.
    dpdu := Vector null.
    dpdv := Vector null.
    dndu := Vector null.
    dndv := Vector null
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
DifferentialGeometry p: point [ p := point ]

DifferentialGeometry printOn: aStream
[
    aStream nextPutAll: 'Hit at: ';
            print: p;
            nextPutAll: ' Normal: ';
            print: self normal
]

DifferentialGeometry weingartenD2pd2u: d2pd2u d2pduv: d2pduv d2pd2v: d2pd2v
[
    | invEGF2 eE fF gG nN e f g |
    eE := self dpdu dot: self dpdu.
    fF := self dpdu  dot: self dpdv.
    gG := self dpdv dot: self dpdv.
    nN := self dpdu cross: self dpdv.
    e := nN dot: d2pd2u.
    f := nN dot: d2pduv.
    g := nN dot: d2pd2v.
    invEGF2 := 1.0 / ((eE * gG) - (fF * fF)).
    self dndu: (((f * fF) - (e * gG)) * invEGF2 * self dpdu) + 
            (((e * fF) - (f * eE)) * invEGF2 * self dpdv).
    self dndv: (((g * fF) - (f * gG)) * invEGF2 * self dpdu) + 
            (((f * fF) - (g * eE)) * invEGF2 * self dpdv).
]
