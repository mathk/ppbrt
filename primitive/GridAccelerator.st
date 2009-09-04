{ import: Aggregate }
{ import: Voxel }

GridAccelerator : Aggregate (nVoxel width worldBbox voxelPerUnitDist voxelss)

GridAccelerator primitives: allPrimitives refineBefor: refineNeeded
[
    self := super primitives: allPrimitives refineBefor: refineNeeded.
    primitives do: []
]

GridAccelerator voxelsPerUnitDist
[
    ^voxelPerUnitDist ifNil: [
        voxelPerUnitDist := 3.0 *  (primitives size asFloat pow: 1.0 / 3.0) 
                                * self worldBbox invMaxWidth
    ]
]

GridAccelerator voxelPosition: point
[
    | p |
    p := Triplet xyz: 0.
    point withIndexDo: [:index :value |
        p at: index put: (value - self worldBbox at: index) / (self width at: index)
    ].
    ^p
]

GridAccelerator nVoxel
[
    ^nVoxel ifNil: [
        nVoxel := Tripet xyz: 0.
        1 to: 3 do: [:index |  
            nVoxel at: index put: 
                (((self worldBbox delta at: index) * self voxelPerUnitDist) asInteger clampMin: 1 max: 64)
        ]
        nVoxel
    ]
]

GridAccelerator width
[
    ^width ifNil: [
        width := Triplet xyz: 0.
        1 to: 3 do: [:index | width at: index put: (self delta at: index) / (self nVoxel at: index)].
        width
    ]
]

GridAccelerator worldBbox
[
    ^worldBbox ifNil: [
        worldBbox := primitives inject: (primitives at: 1) worldBbox into: [:el :rsl |
            el union: rsl
        ]
    ]
]
