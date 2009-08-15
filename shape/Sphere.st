{ import: RevolutionShape }
{ import: BBox }
{ import: DifferentialGeometry }

Sphere : RevolutionShape ( thetaMin thetaMax radius zmin zmax )

Sphere toWorld: o2w orientation: ro radius: r
       minz: minz maxz: maxz maxphi: maxphi
[
    self := self phiMax: maxphi.
    radius := r.
    objectToWorld := o2w.
    reverseOrientation := ro.
    zmin := minz clampMin: radius negated max: radius.
    zmax := maxz clampMin: radius negated max: radius.
    thetaMin := (zmin / radius) acos.
    thetaMax := (zmax / radius) acos. "Todo: may be swap min max"
]

Sphere bbox
[
    ^BBox pMin: (Point x: radius negated y: radius negated z: zmin)
          pMax: (Point x: radius y: radius z: zmax)
]

Sphere clipingCheck: hitPoint
[
    (hitPoint z < zmin or: [hitPoint z > zmax]) ifTrue: [ ^false ].
    ^true
]

Sphere differntialAt: p ifFailed: failed
[
    | phi theta u v invzradius 
      cosphi sinphi thetaDiff d2pd2u d2pd2v
      d2pduv eE fF gG nN e f g invEGF2 dndu
      dndv diff |

    diff := DifferentialGeometry shape: self hit: p.
    (self addParametricValue: diff at: p) ifTrue: [ ^failed value ].
    invzradius := 1 / p zradius.
    cosphi := p x * invzradius.
    sinphi := p y * invzradius.
    diff dpdu: (Vector x:  thetaMax negated * p y y: thetaMax * p x z: 0.0).
    diff dpdv: (thetaDiff * (Vector x: p z * cosphi 
                                y: p z * sinphi
                                z: (thetaMin + (v * thetaDiff)) sin * radius negated)).
    d2pd2u := phiMax * phiMax negated * (Vector x: p x y: p y z: 0.0).
    d2pduv := thetaDiff * p z * phiMax * (Vector x: sinphi negated y: cosphi z: 0.0).
    d2pd2v := thetaDiff * thetaDiff negated * (Vector x: p x y: p y z: p z).
    eE := diff dpdu dot: diff dpdu.
    fF := diff dpdu  dot: diff dpdv.
    gG := diff dpdv dot: diff dpdv.
    nN := diff dpdu cross: diff dpdv.
    e := nN dot: d2pd2u.
    f := nN dot: d2pduv.
    g := nN dot: d2pd2v.
    invEGF2 := 1.0 / ((eE * gG) - (fF * fF)).
    diff dndu: (((f * fF) - (e * gG)) * invEGF2 * diff dpdu) + 
            (((e * fF) - (f * eE)) * invEGF2 * diff dpdv).
    diff dndv: (((g * fF) - (f * gG)) * invEGF2 * diff dpdu) + 
            (((f * fF) - (g * eE)) * invEGF2 * diff dpdv).
    ^diff
]

Sphere addParametricValue: differntial at: p
[
    | phi theta |
    phi := self hitPhi: p.
    phi > phiMax ifTrue: [ ^false ].
    theta := (p z / radius) acos.
    differntial u: phi / phiMax.
    differntial v: (theta - thetaMin) / (thetaMax - thetaMin).
    ^true
]


Sphere findRayHit: aRay ifFailed: failed
[
    ^self quadraticFindRayHit: aRay
                a: aRay direction squaredLength  
                b: 2 * ((aRay origin x * aRay direction x) + 
                        (aRay origin y * aRay direction y) + 
                        (aRay origin z * aRay direction z))
                c: (aRay origin squaredDist: Point origin) - 
                   (radius * radius)
                ifFailed: [ ^failed value ]
]
