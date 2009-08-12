{ import: Shape }
{ import: BBox }
{ import: QuadraticSolver }
{ import: DifferentialGeometry }

Sphere : Shape ( thetaMin thetaMax phiMax radius zmin zmax )

Sphere toWorld: o2w orientation: ro radius: r
       minz: minz maxz: maxz maxphi: maxphi
[
    self := self new.
    radius := r.
    objectToWorld := o2w.
    reverseOrientation := ro.
    zmin := minz clampMin: radius negated max: radius.
    zmax := maxz clampMin: radius negated max: radius.
    thetaMin := (zmin / radius) acos rad.
    thetaMax := (zmax / radius) acos rad.
    phiMax := (maxphi deg clampMin: 0 max: 360) deg rad
    
]

Sphere bbox
[
    ^BBox pMin: (Point x: radius negated y: radius negated z: zmin)
          pMax: (Point x: radius y: radius z: zmax)
]

Sphere intersect: ray ifFailed: failed
[
    | oRay solve |
    oRay := objectToWorld inverse apply: ray.
    solve := self findRayT: oRay ifFailed: [ ^failed value ].
    ^self clipingCheck: oRay hitT: solve firstSolution ifFailed: [
        self  clipingCheck: oRay hitT: solve secondSolution ifFailed: [ ^failed value ]
    ]
]

Sphere clipingCheck: aRay hitT: t ifFailed: failed
[
    | hitPoint |
    (t > aRay maxt or: [t < aRay mint]) ifTrue: [ ^failed value ].
    hitPoint := aRay at: t.
    (hitPoint z < zmin or: [hitPoint z > zmax]) ifTrue: [ ^failed value ].
    ^self parametricValueAt: hitPoint ifFailed: [ ^failed value ]
]

Sphere parametricValueAt: p ifFailed: failed
[
    | phi theta u v invzradius dpdu dpdv 
      cosphi sinphi thetaDiff d2pd2u d2pd2v
      d2pduv eE fF gG nN e f g invEGF2 dndu
      dndv |

    phi := p y atan2: p x.
    phi negative ifTrue: [ phi := phi + (2 * Float pi) ].
    phi > phiMax ifTrue: [ ^failed value ].
    theta = (p z / radius) acos.
    thetaDiff := (thetaMax - thetaMin).
    u := phi / phiMax.
    v := (theta - thetaMin) / thetaDiff.
    invzradius := 1 / p zradius.
    cosphi := p x * invzradius.
    sinphi := p y * invzradius.
    dpdu := Vector x:  thetaMax negated * p y z: thetaMax * p x z: 0.0.
    dpdv := thetaDiff * (Vector x: p z * cosphi 
                                y: p z * sinphi
                                z: (thetaMin + (v * thetaDiff)) sin * radius negated).
    d2pd2u := phiMax * phiMax negated * (Vector x: p x y: p y z: 0.0).
    d2pduv := thetaDiff * p z * phiMax * (Vector x: sinphi negated y: cosphi z: 0.0).
    d2pd2v := thetaDiff * thetaDiff negated * (Vector x: p x y: p y z: p z).
    eE := dpdu dot: dpdu.
    fF := dpdu  dot: dpdv.
    gG := dpdv dot: dpdv.
    nN := dpdu cross: dpdv.
    e := nN dot: d2pd2u.
    f := nN dot: d2pduv.
    g := nN dot: d2pd2v.
    invEGF2 := 1.0 / ((eE * gG) - (fF * fF)).
    dndu := ((f * fF) - (e * gG)) * invEGF2 * dpdu + 
            ((e * fF) - (f * eE)) * invEGF2 * dpdv.
    dndv := ((g * fF) - (f * gG)) * invEGF2 * dpdu + 
            ((f * fF) - (g * eE)) * invEGF2 * dpdv.
    ^DifferentialGeometry shape: self hit: p u: u v: v 
                          dpdu: dpdu dpdv: dpdv dndu: dndu dndv: dndv
] 


Sphere findRayT: aRay ifFailed: failed
[
    | solve t0 t1 |
    solve := QuadraticSolver 
                solveA: aRay direction squaredLength  
                b: 2 * ((aRay origin x * aRay direction x) + 
                        (aRay origin y * aRay direction y) + 
                        (aRay origin z * aRay direction z))
                c: (aRay origin squaredDist: Point origin) - (radius * radius).
    solve hasSolution ifFalse: [ ^failed value ].
    t0 := solve firstSolution.
    t1 := solve secondSolution.
    (t0 > aRay maxt or: [t1 < aRay mint])
        ifTrue: [ ^failed value ].
    ^solve
]
