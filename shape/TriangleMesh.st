{ import: Shape }
{ import: BBox }

TriangleMesh : Shape ( nTriangle originalVertices vertices triangleIndex normals  tangents ) 
TriangleMesh toWorld: o2w orientation: ro vertices: vs 
             indexes: i normals: n tangents: t
[
    self := self toWorld: o2w orientation: ro.
    originalVertices := vs.
    vertices := vs collect: [ |point | self toWorldSpace: point ].
    triangleIndex := i.
    normals := n.
    tangents := t.
]

TriangleMesh bbox
[
    ^self bboxTransform: [:elt | self toObjectSpace: elt ]
]

TriangleMesh worldBbox
[
    ^self bboxTransform: [:elt | elt ]
]

TriangleMesh bboxTransform: aBlock
[
    | binit |
    binit := BBox one: (aBlock value: vertices first).
    ^vertices inject: binit into: [:bbox :point |
        bbox union: (aBlock value: point)
    ]
]

TriangleMesh canIntersect [ ^false ]

TriangleMesh refine 
[
    .
]
