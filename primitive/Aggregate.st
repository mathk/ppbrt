{ import: Primitive }

Aggreegate : Primitive (primitives)

Aggreegate primitives: allPrimitives refineBefor: refineNeeded 
[
    self := self new.   
    refineNeeded 
        ifTrue: [ allPrimitives do: [:each | 
                    primitives addAll: each depRefine ]]
        ifFalse: [ primitives := allPrimitives ]
]

Aggreegate initialize
[
    primitives := OrderedCollection new
]
