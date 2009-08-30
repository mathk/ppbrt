{ import: Normal }
{ import: Vector }
{ import: Transform }
{ import: st80 }

DifferentialGeometry : Object ( u v shape p n dpdu dpdv dndu dndv isTransformToWorld toWorld)

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

DifferentialGeometry shape: s hit: phit toWorld: transform
[
    self := self shape: s hit: phit.
    toWorld := transform
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
DifferentialGeometry toWorld [ ^toWorld ifNil: [Transform identity] ]
DifferentialGeometry toWorld: t 
[
    isTransformToWorld := false.
    toWorld := t 
]

DifferentialGeometry initialize
[
    u := 0.
     v := 0.
    dpdu := Vector null.
    dpdv := Vector null.
    dndu := Vector null.
    dndv := Vector null.
    isTransformToWorld := false
]


DifferentialGeometry normal
[
    ^n ifNil: [
        | norm |
        norm := (dpdu cross: dpdv) normalize.
        n := Normal x: norm x y: norm y z: norm z.
    ]
]

DifferentialGeometry normal: normal [ n := normal]

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

DifferentialGeometry transformToWorld
[
    isTransformToWorld ifFalse: [
        isTransformToWorld := true.
        self dndu: (self toWorld apply: self dndu).
        self dndv: (self toWorld apply: self dndv).
        self dpdu: (self toWorld apply: self dpdu).
        self dpdv: (self toWorld apply: self dpdv).
        self p: (self toWorld apply: self p).
        n := nil
    ]
]
