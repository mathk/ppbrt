{ import: Shape }

TransformShape : Shape ( instanceToWorld shape )

TransformShape toWorld: i2w shape: s
[
    self := self toWorld: s worldToObject * i2w orientation: false.
    instanceToWorld := i2w.
    shape := s
]



TransformShape objectSpaceIntersect: ray ifFailed: failed
[
    | diff |
    diff := shape objectSpaceIntersect:ray ifFailed: [ ^failed value ].
    diff toWorld: instanceToWorld.
    ^diff transformToWorld; yourself.
]

TransformShape area
[
    ^shape area
]

TransformShape shading: diff
[
    ^shape shading: diff
]
