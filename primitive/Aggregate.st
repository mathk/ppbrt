{ import: Primitive }

Aggregate : Primitive (primitives)

Aggregate primitives: allPrimitives refineBefor: refineNeeded 
[
    self := self new.   
    refineNeeded 
        ifTrue: [ allPrimitives do: [:each | 
                    primitives addAll: each depRefine ]]
        ifFalse: [ primitives := allPrimitives ]
]

Aggregate initialize
[
    primitives := OrderedCollection new
]
