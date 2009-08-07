
{ import: Shape }

Shpere : Shape ( thetaMin thetaMax phiMax radius )

Shpere toWorld: o2w orientation: ro radius: r minz: minz maxz: maxz maxphi: maxphi
[
    self := self new.
    radious := r.
    objectToWorld := o2w.
    reverseOrientation := ro.
    
]
