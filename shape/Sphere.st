{ import: RevolutionShape }
{ import: BBox }
{ import: DifferentialGeometry }

Sphere : RevolutionShape ( thetaMin thetaMax thetaDiff radius zmin zmax )

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

Sphere thetaDiff
[
    ^thetaDiff ifNil: [ thetaDiff := thetaMax - thetaMin ]
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
    | diff |
    diff := DifferentialGeometry shape: self hit: p.
    (self addParametricValue: diff at: p) ifFalse: [ ^failed value ].
    self addDerivativesParametric: diff at: p.
    self addDerivativesNormal: diff at: p.
    ^diff
]

Sphere addDerivativesParametric: differential at: p
[
    differential dpdu: (Vector x:  thetaMax negated * p y y: thetaMax * p x z: 0.0).
    differential dpdv: (self thetaDiff * (Vector x: p z * p cosphi 
                                y: p z * p sinphi
                                z: (thetaMin + (differential v * self thetaDiff)) sin * 
                                    radius negated)).
]

Sphere addParametricValue: differntial at: p
[
    p phi > phiMax ifTrue: [ ^false ].
    differntial u: p phi / phiMax.
    differntial v: (p theta - thetaMin) / self thetaDiff.
    ^true
]

Sphere addDerivativesNormal: differential at: p
[
    | d2pd2u d2pd2v d2pduv eE fF gG nN e f g invEGF2 |
    d2pd2u := phiMax * phiMax negated * (Vector x: p x y: p y z: 0.0).
    d2pduv := self thetaDiff * p z * phiMax * (Vector x: p sinphi negated y: p cosphi z: 0.0).
    d2pd2v := self thetaDiff * self thetaDiff negated * p toVector.
    eE := differential dpdu dot: differential dpdu.
    fF := differential dpdu  dot: differential dpdv.
    gG := differential dpdv dot: differential dpdv.
    nN := differential dpdu cross: differential dpdv.
    e := nN dot: d2pd2u.
    f := nN dot: d2pduv.
    g := nN dot: d2pd2v.
    invEGF2 := 1.0 / ((eE * gG) - (fF * fF)).
    differential dndu: (((f * fF) - (e * gG)) * invEGF2 * differential dpdu) + 
            (((e * fF) - (f * eE)) * invEGF2 * differential dpdv).
    differential dndv: (((g * fF) - (f * gG)) * invEGF2 * differential dpdu) + 
            (((f * fF) - (g * eE)) * invEGF2 * differential dpdv).
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
