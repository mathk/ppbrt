{ import: st80 }

SmallInteger to: anInteger by: step do: unaryBlock
[
    | index |
    index := self.
    [index <= anInteger]
	whileTrue:
	   [unaryBlock value: index.
	    index := index + step].
]
