{ import: Point }
{ import: FloatLib }

Ray : Object ( o d mint maxt time testedPrimitives )

Ray origin: orig direction: dir
[
    self := super new.
    o := orig.
    d := dir
]

Ray initialize
[
    mint := Float epsilon.
    maxt := Float infinity.
    o := Point origin.
    d := Vector x: 0.0 y: 0.0 z: 1.0.
    time := 0.0.
    testedPrimitives := Dictionary new
]

Ray mint [ ^mint ]
Ray maxt [ ^maxt ]
Ray minp [ ^self at: self mint ]
Ray maxp [ ^self at: self maxt ]
Ray time [ ^time ]
Ray mint: t [ mint := t ]
Ray maxt: t [ maxt := t ]
Ray time: t [ time := t ]


Ray isTested: aPrimitive
[
    testedPrimitives at: aPrimitive ifAbsent: [ ^false ].
    ^true
]

Ray printOn: aStream
[
    aStream nextPutAll: 'Ray ( mint: ';
            print: mint;
            nextPutAll: ' maxt: ';
            print: maxt;
            nextPutAll: ' time: ';
            print: time;
            nextPutAll:' )\nDirection: ';
            print: d;
            nextPutAll: '\nOrigin: ';
            print: o
]

Ray direction [ ^d ]
Ray origin [ ^o ]

Ray at: t
[
    self assert: [ (self checkInBound: t) not ].
    ^o + (d * t)
]

Ray applyTransformation: t
[
    | new |
    new := self origin: (t apply: o) direction: (t apply: d).
    new mint: self mint.
    new maxt: self maxt.
    new time: self time.
    ^new
]

Ray checkInBound: t
[
    ^(t > self maxt or: [t < self mint])
]
