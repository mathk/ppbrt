{ import: Shape }
{ import: BBox }

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

Sphere intersect: ray
[
    | oRay |
    oRay := objectToWorld inverse apply: ray.
]
