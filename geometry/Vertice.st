{ import: Point }

Vertice : Point ( u v )

Vertice fromPoint: aPoint u: uP v: vP
[
    ^self x: aPoint x y: aPoint y z: aPoint z u: uP v: vP
]

Vertice x: xcoor y: ycoor z: zcoor u: uP v: vP
[
    self := self x: xcoor y: ycoor z: zcoor.
    u := uP.
    v := vP.
] 

Vertice u [ ^u ]
Vertice v [ ^v ]
Vertice u: aU [ u := aU ]
Vertice v: aV [ v := aV ] 

Vertice name [ ^'VP' ]

Vertice fromHomogeneous: aMatrix
[
    | vr |
    vr := super fromHomogeneous: aMatrix.
    vr u: u.
    vr v: v.
    ^vr
]
