{ import: Point }
{ import: FloatLib }

Ray : Object ( o d mint maxt time )

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
    o := Point new.
    d := Vector x: 0.0 y: 0.0 z: 1.0.
    time := 0.0
]

Ray mint [ ^mint ]
Ray maxt [ ^maxt ]
Ray time [ ^time ]
Ray mint: t [ mint := t ]
Ray maxt: t [ maxt := t ]
Ray time: t [ time := t ]

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

Ray at: aNumber 
[
    self assert: [(aNumber >= mint) and: [(aNumber <= maxt)] ].
    ^o + (d * aNumber) 
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
