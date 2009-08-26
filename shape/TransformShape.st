{ import: st80 }

TransfomShape : Shape ( shape )

TransfomShape toWorld: i2w shape: s
[
    self := self toWorld: i2w orientation: false.
    shape := s
]

TransformShape objectSpaceIntersect: ray ifFailed: failed
[
   "TODO Transform diff to world"
    ^shape objectSpaceIntersect:ray ifFailed: [ ^failed value ]
]

TransformShape area
[
    ^shape area
]

TransformShape shading: diff
[
    ^shape shading: diff
]
