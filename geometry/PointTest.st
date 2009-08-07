{ import: st80 }
{ import: Point }

[
    | p1 p2 |
    Float epsilon putln.
    p1 := Point x: 1.0 y: -1.0 z: 0.0.
    p2 := Point x: 0.0 y: 2.0 z: 1.0.
    (p1 - p2) printString putln.
]
