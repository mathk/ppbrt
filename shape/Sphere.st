{ import: RevolutionShape }
{ import: BBox }
{ import: DifferentialGeometry }

Sphere : RevolutionShape ( thetaMin thetaMax thetaDiff radius zmin zmax )

Sphere toWorld: o2w orientation: ro radius: r
       minz: minz maxz: maxz maxphi: maxphi
[
    self := self toWorld: o2w orientation: ro phiMax: maxphi.
    radius := r.
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

Sphere addDerivativesParametric: differential at: p
[
    differential dpdu: (Vector x:  thetaMax negated * p y y: thetaMax * p x z: 0.0).
    differential dpdv: (self thetaDiff * (Vector x: p z * p cosphi 
                                y: p z * p sinphi
                                z: (thetaMin + (differential v * self thetaDiff)) sin * 
                                    radius negated)).
]

Sphere addParametric: diff at: p ifFailed: failed
[
    p phi > phiMax ifTrue: [ ^failed value ].
    diff u: p phi / phiMax.
    diff v: (p theta - thetaMin) / self thetaDiff.
]

Sphere addDerivativesNormal: differential at: p
[
    | d2pd2u d2pd2v d2pduv |
    d2pd2u := phiMax * phiMax negated * (Vector x: p x y: p y z: 0.0).
    d2pduv := self thetaDiff * p z * phiMax * (Vector x: p sinphi negated y: p cosphi z: 0.0).
    d2pd2v := self thetaDiff * self thetaDiff negated * p toVector.
    differential weingartenD2pd2u: d2pd2u d2pduv: d2pduv d2pd2v: d2pd2v
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
