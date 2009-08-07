{ import: Transform }
{ import: Ray }
[
    | tR |
    tR := Transform rotX: ((1/3) * Float pi) rad.
"    tR putln.
    tR := Transform rotY: ((1/3) * Float pi) rad.
    tR putln.
    tR := Transform rotZ: ((1/3) * Float pi) rad.
    tR putln."
    (tR matrix * tR matrix inverse) putln.
    tR := Transform rotAxis: (Vector x: 1.0 y: 0.0 z: 0.0) angle: 90 deg.
    tR putln.
    (tR apply: (Normal x: 0.0 y: 1.0 z: 0)) putln.
    (tR apply: (Ray origin: (Point x: 1.0 y: 2.0 z: -1.0) 
                    direction: (Vector x: -1.0 y: -2.0 z: 1.0))) putln.
]
