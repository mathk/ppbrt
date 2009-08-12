{ import: Sphere }
{ import: Transform }
{ import: Ray }

[ 
    | r s |
    s := Sphere toWorld: (Transform translate: (Vector x:0.0 y: 0.0 z: 0.0)) 
                orientation: false
                radius: 1.0
                minz: 0.9 negated
                maxz: 0.9
                maxphi: 135.0 deg.
    r := Ray origin: (Point x: -1.5 y: 2.0 z: 0.0) 
             direction: (Vector x: 1.5 y: -2.0 z: 0.0 ).
    (s intersect: r ifFailed: [{printf(_backtrace());}. 's do not intesct r' putln]) putln.
    r := Ray origin: (Point x: -2.5 y: 2.0 z: 0.0) 
             direction: (Vector x: 2.5 y: -2.0 z: 0.0 ).
    s intersect: r ifFailed: [{printf(_backtrace());}. 's do not intesct r' putln] .
    r := Ray origin: (Point x: -2.5 y: -2.0 z: 0.0) 
             direction: (Vector x: 2.5 y: 2.0 z: 0.0 ).
    (s intersect: r ifFailed: [{printf(_backtrace());}. 's do not intesct r' putln]) putln.
]
