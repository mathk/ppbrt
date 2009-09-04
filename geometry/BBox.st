{ import: Point }
{ import: BSphere }

BBox : Object ( pMin pMax delta invMaxWidth maxWidth )

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

BBox expand: deltaExp
[
    pMin sub: (Vector xyz: deltaExp).
    pMax add: (Vector xyz: deltaExp)
]

BBox toBSphere
[
    | c |
    c := 0.5 * (pMin + pMax).
    ^BSphere center: c
             radius: pMax dist: c
]

BBox isIntersecting: ray
[
    self intersectParametric: ray ifFailed: [^false].
    ^true
]


BBox intersectParametric: ray ifFailed: failed
[
    | t0 t1 |
    t0 := ray mint.
    t1 := ray maxt.
    pMin with: pMax with: ray d with: ray o do: [:cmin :cmax :cray :cori |
        | tnear tfar cinvray |
        cinvray := 1.0 / cray.
        tnear := (cmin - cori) * cinvray.
        tfar := (cmax - cori) * cinvray.
        tfar < tnear ifTrue: [ | temp| 
            temp := tnear.
            tnear := tfar.
            tfar := temp
        ].
        tnear > t0 ifTrue: [t0 := tnear].
        tfar < t1 ifTrue: [t0 := tfar].
        t0 > t1 ifTrue: [^failed value].
    ]
    ^t0
]

BBox delta
[
    ^delta ifNil: [delta := pMax - pMin]
]

BBox maxWidth
[
    ^maxWidth ifNil: [maxWidth := (self delta x max: self delta y) max: self delta z]
]

BBox invMaxWidth
[
    ^invMaxWidth ifNil: [invMaxWidth := 1.0 / self maxWidth]
]
