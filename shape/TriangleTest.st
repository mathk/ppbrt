{ import: Point }
{ import: Transform }
{ import: Ray }
{ import: Triangle }



[
    | t r |
    t := Triangle toWorld: Transform identity
                  orientation: false
                  p1: (Point x: 1.0 y: 0.0 z: 0.0)
                  p2: (Point x: 0.0 y: 1.0 z: 0.0)
                  p3: (Point x: 0.0 y: 0.0 z: 1.0).
    r := Ray origin: (Point x: 1.0 y: 1.0 z: 0.0)
             direction: (Vector x: -1.0 y: -0.4 z: 0.4).
    (t intersect: r ifFailed: [ {printf(_backtrace());}]) putln
]
