{ import: Shape }

RevolutionShape : Shape ( phiMax )

RevolutionShape phiMax: phi
[
    self := self new.
    phiMax := (phi deg clampMin: 0 max: 360) deg rad
]
