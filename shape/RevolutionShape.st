{ import: Shape }

RevolutionShape : Shape ( phiMax )

RevolutionShape phiMax: phi
[
    self := self new.
    phiMax := (phi deg clampMin: 0 max: 360) deg rad
]

RevolutionShape hitPhi: point
[
    | phi |
    phi := point y atan2: point x.
    phi negative ifTrue: [ phi := phi + (2 * Float pi) ].
    ^phi
]
