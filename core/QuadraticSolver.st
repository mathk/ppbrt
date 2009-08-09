{ import: NumberLib }
{ import: FloatLib }

QuadraticSolver : Object( a b c q discrim rootDiscrm )


QuadraticSolver solveA: anA b: anB c: anC
[
    self := self new.
    a := anA.
    b := anB.
    c := anC
]

Quadraticsolver discrim
[
    ^discrim ifNil:
    [  
        discrim := (b * b) + (4.0 * a * c)
    ]
]

Quadraticsolver rootDiscrm
[
    ^rootDiscrm ifNil: [ rootDiscrm := self discrim sqrt ]
]

Quadraticsolver hasSolution [ ^self discrim > 0 ]

QuadraticSolver firstSolution
[
    ^self q / a
]

QuadraticSolver secondSolution
[
    ^c / self q
]

QuadraticSolver q
[
    ^q ifNil: [ q := -0.5 * (b + (b sign * self rootDiscrm ) ]
]
