{ import: st80 }

Angle : Object ( value )
    Radian : Angle ()
    Degree : Angle ()

Angle value: aFloat
[
    self := super new.
    value := aFloat
]
