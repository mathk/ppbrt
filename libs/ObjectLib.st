{ import: st80 }


Object put
[
    self printString put
]

Object assert: aBlock
[
    {
	#ifndef DO_ASSERTION
	return v_self;
	#endif
    }.
    aBlock value assert.
]
