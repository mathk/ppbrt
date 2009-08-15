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
    |  d2pd2u d2pd2v
      d2pduv eE fF gG nN e f g invEGF2 diff |

    diff := DifferentialGeometry shape: self hit: p.
    (self addParametricValue: diff at: p) ifTrue: [ ^failed value ].
    self addDerivateivesParametric: diff at: p.
    d2pd2u := phiMax * phiMax negated * (Vector x: p x y: p y z: 0.0).
    d2pduv := self thetaDiff * p z * phiMax * (Vector x: p sinphi negated y: p cosphi z: 0.0).
    d2pd2v := self thetaDiff * self thetaDiff negated * p toVector.
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

Sphere addDerivateivesParametric: differential at: p
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
