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
    | oRay t0 t1 |
    oRay := objectToWorld inverse apply: ray.
    solve := QuadraticSolver 
                solveA: oRay d squaredLength  
                b: 2 * (aRay o dot: aRay d)
                c: aRay o squaredLength - (radius * radius).
    solve hasSolution ifTrue: [ ^failed value ].
    t0 := solve firstSolution.
    t1 := solve secondSolution.
    t1 < t0 ifTrue: [ 
        t1 := solve secondSolution.
        t0 := solve firstSolution
    ].
    (t0 > ray maxt or: [t1 < ray maxt])
        ifTrue: [ ^failed value ].
]

Sphere isIntersect
[
    ^(QuadraticSolver 
                solveA: oRay d squaredLength  
                b: 2 * (aRay o dot: aRay d)
                c: aRay o squaredLength - (radius * radius)) hasSolution
]
