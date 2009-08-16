{ import: Shape }

RevolutionShape : Shape ( phiMax )

RevolutionShape  toWorld: o2w orientation: ro phiMax: phi
[
    self := self toWorld: o2w orientation: ro.
    phiMax := (phi deg clampMin: 0 max: 360) deg rad
]
