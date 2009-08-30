{ import: Vertice }
{ import: Transform }
{ import: TransformShape }
{ import: Ray }
{ import: Triangle }



[
    | t r z tr ts trtr |
    trtr := Transform translate: (Vector x:0.0 y: 0.0 z: 1.0).
    tr := Transform identity.
    t := Triangle toWorld: tr
                  orientation: false
                  p1: (Vertice x: 1.0 y: 0.0 z: 0.0 u: 1.0 v: 0.0)
                  p2: (Vertice x: 0.0 y: 1.0 z: 0.0 u: 0.0 v: 1.0)
                  p3: (Vertice x: 0.0 y: 0.0 z: 1.0 u: 1.0 v: 1.0).
    r := Ray origin: (tr apply: (Point x: 1.0 y: 0.9 z: 0.0))
             direction: (tr apply: (Vector x: -1.0 y: -0.4 z: 0.4)).
    (t intersect: r ifFailed: [ {printf(_backtrace());}]) putln.
    ts := TransformShape toWorld: trtr shape: t.
    (ts intersect: (trtr apply: r) ifFailed: [ {printf(_backtrace());}]) putln.
    1 to: 100 do: [:i | ts intersect: (trtr apply: r) ifFailed: [ {printf(_backtrace());}]].
    '100 done' putln.
    Float nan > 2 ifTrue: ['DED' put].
    Float nan <= 2 ifTrue: ['DD' put].
    Float nan putln.
    r := (1.0 /0.0) putln; yourself.
    z := (r * 0.0) putln; yourself.
    z > 2 ifTrue: ['DED' put].
    z <= 2 ifTrue: ['DD' put].

]
