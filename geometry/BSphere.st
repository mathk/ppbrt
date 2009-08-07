{ import: st80 }


BSphere : Object ( center radius )

BSphere center: aCenter radius: aRadius
[
    self := super new.
    center := aCenter.
    radius := aRadius
]
