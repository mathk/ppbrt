{ import: RevolutionShape }
{ import: BBox }

Cylinder : RevolutionShape ( radius zmin zmax height )

Cylinder toWorld: o2w orientation: ro radius: r
       minz: minz maxz: maxz maxphi: maxphi
[
    self := self phiMax: maxphi.
    radius := r.
    objectToWorld := o2w.
    reverseOrientation := ro.
    zmin := minz.
    zmax := maxz
]

Cylinder bbox
[
    ^BBox pMin: (Point x: radius negated y: radius negated z: zmin)
          pMax: (Point x: radius y: radius z: zmax )
]

Cylinder findRayHit: aRay ifFailed: failed
[
    ^self quadraticFindRayHit: aRay
                a: (aRay direction x * aRay direction x) + 
                   (aRay direction y * aRay direction y)  
                b: 2 * ((aRay origin x * aRay direction x) + 
                        (aRay origin y * aRay direction y))
                c: (aRay origin x * aRay origin x) + 
                   (aRay origin y * aRay origin y) -
                   (radius * radius)
                ifFailed: [ ^failed value ]
]

Cylinder clipingCheck: hitPoint
[
    (hitPoint z < zmin or: [hitPoint z > zmax]) ifTrue: [ ^false ].
    ^true
]

Cylinder height
[
   ^height ifNil: [ height := zmax - zmin ] 
]

Cylinder addParametricValue: differential at: p ifFailed: failed
[
    p phi > phiMax ifTrue: [ ^failed value ].
    differential u: p phi / phiMax.
    differential v: (p z - zmin) / self height.
]

Cylinder addDerivativesParametric: differential at: p
[
    differential dpdu: (Vector x: (phiMax negated * p y)
                               y: (phiMax * p x) z: 0.0).
    differential dpdv: (Vector x: 0.0 y: 0.0 z: self height)
]

Cylinder addDerivativesNormal: differential at: p
[
    differential weingartenD2pd2u: (phiMax * phiMax negated) * 
                                   (Vector x: p x y: p y z: 0.0)
                    d2pd2uv: (Vector null)
                    d2pd2v: (Vector null)
]
