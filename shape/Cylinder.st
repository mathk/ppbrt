{ import: RevolutionShape }
{ import: BBox }

Cylinder : RevolutionShape ( radius zmin zmax )

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
