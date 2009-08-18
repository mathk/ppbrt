{ import: Ray }

[
    | r |
    r := Ray origin: Point origin
             direction: (Vector x: 1.0 y: 1.0 z: 1.0) normalize.
    (r at: 2.0) printString putln
]
