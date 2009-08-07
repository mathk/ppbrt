{ import: st80 }

Integer even
[
    ^((self digitAt: 1)  bitAnd: 1) = 0
]

Integer odd
[
    ^self even not
]
