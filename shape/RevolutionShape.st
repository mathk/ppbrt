{ import: Shape }

RevolutionShape := Shape ( phiMax )

RevolutionShape phiMax: phi
[
    self := self new.
    phiMax := (maxphi deg clampMin: 0 max: 360) deg rad
]

RevolutionShape hitPhi: point
[
    | phi |
    phi := p y atan2: p x.
    phi negative ifTrue: [ phi := phi + (2 * Float pi) ].
    ^phi
]
