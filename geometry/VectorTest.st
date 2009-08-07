{ import: st80 }
{ import: Vector }

[
    | v1 v2 |
    v1 := Vector x: 0.0 y: 1.0 z: -1.0.
    v2 := Vector x: 1.0 y: 2.0 z: 3.0.
    (v1 dot: v2) printString putln.
    (v1 cross: v2) printString putln.
    v1 squaredLength putln.
    v1 length putln.

]
