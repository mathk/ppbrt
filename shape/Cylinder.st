{ import: Shape }

Cylinder : RevolutionShape ( radius zmin zmax )

Cylinder toWorld: o2w orientation: ro radius: r
       minz: minz maxz: maxz maxphi: maxphi
[
    self := self phiMax: maxphi.
    radius := r.
    objectToWorld := o2w.
    reverseOrientation := ro.
    
]
