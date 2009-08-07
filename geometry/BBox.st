{ import: Point }
{ import: BSphere }

BBox : Object ( pMin pMax )

BBox pMin [ ^pMin ]

BBox pMax [ ^pMax ]

BBox new
[
    self := super new.
    pMin := Point x: Float infinity y: Float infinity z: Float infinity.
    pMax := Point x: Float infinity negated
                  y: Float infinity negated
                  z: Float infinity negated
]

BBox volume [^(pMax - pMin) volume]

BBox pMin: aPMin pMax: aPMax
[
    self := super new.
    pMin := aPMin.
    pMax := aPMax
]

BBox one: aPoint
[
    self := super new.
    pMin := aPoint.
    pMax := aPoint
]

BBox union: anObject
[
    ^anObject unionBBox: self.
]

BBox unionBBox: aBBox
[
    | newPMin newPMax |
    newPMin := Point x: (aBBox pMin x min: pMin x)
                     y: (aBBox pMin y min: pMin y)
                     z: (aBBox pMin z min: pMin z).
    newPMax := Point x: (aBBox pMax x max: pMax x)
                     y: (aBBox pMax y max: pMax y)
                     z: (aBBox pMax z max: pMax z).
    ^BBox pMin: newPMin pMax: newPMax
]

BBox applyTransformation: t
[
    | tBBox |
    tBBox := (self one: (t apply: pMin)).
    tBBox := tBBox union: (t apply: (Point x: pMax x y: pMin y z: pMin z)).
    tBBox := tBBox union: (t apply: (Point x: pMin x y: pMax y z: pMin z)).
    tBBox := tBBox union: (t apply: (Point x: pMin x y: pMin y z: pMax z)).
    tBBox := tBBox union: (t apply: (Point x: pMin x y: pMax y z: pMax z)).
    tBBox := tBBox union: (t apply: (Point x: pMax x y: pMin y z: pMax z)).
    tBBox := tBBox union: (t apply: (Point x: pMax x y: pMax y z: pMin z)).
    ^tBBox union: (t apply: pMax).
]

BBox overlaps: aBBox
[
    | cX cY cZ |
    cX := pMax x >= aBBox pMin x 
               and: [ pMin x <= aBBox pMax x].
    cY := pMax y >= aBBox pMin y 
               and: [ pMin y <= aBBox pMax y].
    cZ := pMax z >= aBBox pMin z 
               and: [ pMin z <= aBBox pMax z].
    ^cX and: [cY and: [cZ]]
]

BBox inside: aPoint
[
    | cX cY cZ |
    cX := pMin x <= aPoint x and: [ aPoint x <= pMax x].
    cY := pMin y <= aPoint y and: [ aPoint y <= pMax y].
    cZ := pMin z <= aPoint z and: [ aPoint z <= pMax z].
    ^cX and: [cY and: [cZ]]
]

BBox expand: delta
[
    pMin sub: (Vector xyz: delta).
    pMax add: (Vector xyz: delta)
]

BBox toBSphere
[
    | c |
    c := 0.5 * (pMin + pMax).
    ^BSphere center: c
             radius: pMax dist: c
]
