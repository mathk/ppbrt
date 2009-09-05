{ import: st80 }


Array at: index ifNilPut: block
[
    ^(self at: index)
        ifNil: [self at: index put: block value]
        ifNotNil: [self at: index]
]
