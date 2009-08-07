{ import: Matrix }

[
    | m  m3 a b |
    m := Matrix rows: 2 cols: 2.
    m i: 1 j: 1 put: 1.
    m i: 2 j: 1 put: 2.
    m i: 1 j: 2 put: 3.
    m i: 2 j: 2 put: 4.
    m putln.
    (3 * m) putln.
    (m * 3.0) putln.
    (m removeI: 1 j: 2) putln.
    (5 \\ 2) putln.
    (3 \\ 6) putln.
    m det putln.
    m3 := Matrix rows: 3 cols: 3.
    m3 i: 1 j: 1 put: 2.
    m3 i: 1 j: 2 put: -1.
    m3 i: 1 j: 3 put: 0.
    m3 i: 2 j: 1 put: -1.
    m3 i: 2 j: 2 put: 2.
    m3 i: 2 j: 3 put: -1.
    m3 i: 3 j: 1 put: 0.
    m3 i: 3 j: 2 put: -1.
    m3 i: 3 j: 3 put: 2.
    m3 putln.  
    m3 det putln.
    m3 coMatrix putln.
    m3 coMatrix coMatrix putln.
    m3 coMatrix coMatrix coMatrix coMatrix putln.
    "m3 coMatrix coMatrix coMatrix coMatrix coMatrix coMatrix putln."
    m3 putln.
    m3 inverse putln.
    m3 putln.
    (m3 inverse * m3) putln.
    a := Matrix rows: 2 cols: 2.
    a i: 1 j: 1 put: 1.
    a i: 1 j: 2 put: 2.
    a i: 2 j: 1 put: 3.
    a i: 2 j: 2 put: 4.
    b := Matrix rows: 2 cols: 1.
    b i: 1 j: 1 put: 2.
    b i: 2 j: 1 put: 1.
    a putln.
    b putln.
    ((2 * 0.75) + (-1 * 0.5)) putln.
    (a * b) putln.
    "(b * a) putln"
    a := Matrix rows: 4 cols: 4 values: 
            #( 1 0 0 2
               0 1 0 0
               0 0 1 2
               0 0 0 1).
    a putln.
    a inverse putln
]
