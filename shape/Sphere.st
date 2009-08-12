{ import: Shape }
{ import: BBox }
{ import: QuadraticSolver }

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
    | phi hitPoint |
    (t > aRay maxt or: [t < aRay mint]) ifTrue: [ ^failed value ].
    hitPoint := aRay at: t.
    (hitPoint z < zmin or: [hitPoint z > zmax]) ifTrue: [ ^failed value ].
    phi := hitPoint y atan2: hitPoint x.
    phi negative ifTrue: [ phi := phi + (2 * Float pi) ].
    phi > phiMax ifTrue: [ ^failed value ]
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
