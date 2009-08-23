{ import: Point }
{ import: Transform }
{ import: Ray }
{ import: Triangle }



[
    | t r z |
    t := Triangle toWorld: Transform identity
                  orientation: false
                  p1: (Point x: 1.0 y: 0.0 z: 0.0)
                  p2: (Point x: 0.0 y: 1.0 z: 0.0)
                  p3: (Point x: 0.0 y: 0.0 z: 1.0).
    r := Ray origin: (Point x: 1.0 y: 1.0 z: 0.0)
             direction: (Vector x: -1.0 y: -0.4 z: 0.4).
    (t intersect: r ifFailed: [ {printf(_backtrace());}]) putln.
    Float nan > 2 ifTrue: ['DED' put].
    Float nan <= 2 ifTrue: ['DD' put].
    Float nan putln.
    r := (1.0 /0.0) putln; yourself.
    z := (r * 0.0) putln; yourself.
    z > 2 ifTrue: ['DED' put].
    z <= 2 ifTrue: ['DD' put].
]
