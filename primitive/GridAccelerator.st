{ import: Aggregate }
{ import: Voxel }
{ import: Triplet }
{ import: Vector }

GridAccelerator : Aggregate (nVoxel width worldBbox voxelPerUnitDist voxels)

GridAccelerator primitives: allPrimitives refineBefor: refineNeeded
[
    self := super primitives: allPrimitives refineBefor: refineNeeded.
    primitives do: [:each |
        | minCoordVoxel maxCoordVoxel |
        minCoordVoxel := self voxelPositionAt: each worldBbox pMin.
        maxCoordVoxel := self voxelPositionAt: each worldBbox pMax.
        minCoordVoxel to: maxCoordVoxel do: [:coord |
            (self voxelAt: coord ifNil: [Voxel new]) addPrimitive: each
        ]
    ]
]

GridAccelerator voxels
[
    ^voxels ifNil: [
        voxels := Array new: self nVoxel x.
        1 to: self nVoxel x do: [:xindex | 
            voxels at: xindex put: (Array new: self nVoxel y).
            1 to: self nVoxel y do: [:yindex | 
                (voxels at: xindex) at: yindex put: (Array new: self nVoxel z )
            ]
        ]
    ]
 ]

GridAccelerator voxelsPerUnitDist
[
    ^voxelPerUnitDist ifNil: [
        voxelPerUnitDist := 3.0 *  (primitives size asFloat pow: 1.0 / 3.0) 
                                * self worldBbox invMaxWidth
    ]
]

GridAccelerator voxelPositionAt: point
[
    | p |
    p := Triplet xyz: 0.
    point withIndexDo: [:index :value |
        p at: index put: 
            ((value - self worldBbox at: index) / (self width at: index)) float2Int
                clampMin: 1 max: (self nVoxel at: index)
    ].
    ^p
]

GridAccelerator minPointAt: voxelCoord
[
    ^self worldBbox pMin + (Vector x: (voxelCoord x - 1) * width x
                                  y: (voxelCoord y - 1) * width y
                                  z: (voxelCoord z - 1) * width z)
]

GridAccelerator nVoxel
[
    ^nVoxel ifNil: [
        nVoxel := Triplet xyz: 0.
        1 to: 3 do: [:index |  
            nVoxel at: index put: 
                (((self worldBbox delta at: index) * self voxelPerUnitDist) round2Int clampMin: 1 max: 64)
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

GridAccelerator voxelAt: tripletCoord ifNil: block
[
    ^((voxels at: tripletCoord x) at: tripletCoord y) at: tripletCoord z ifNilPut: block
]

GridAccelerator voxelAt: tripletCoord
[
    ^((voxels at: tripletCoord x) at: tripletCoord y) at: tripletCoord z
]

GridAccelerator canIntersect [ ^true ]

GridAccelerator intersect: ray ifFailed: failed
[
    | currentT gridIntersect currentVoxelPos nextCrossingT deltaT step out |
    currentT := (self worldBbox isInside: ray minp)
                        ifTrue: [ ray mint ]
                        ifFalse: [self worldBbox 
                                        intersect: ray 
                                        ifFailed: [ ^failed value]].
    gridIntersect := ray at: currentT.
    currentVoxelPos := self voxelPositionAt: gridIntersect.
    nextCrossingT := Triplet xyz: 0.0.
    ray direction withIndexDo: [:index :dcoord |
        dcoord negated 
            ifFalse: [
                nextCrossingT at: index put: currentT + 
                    ((((self minPointAt: currentVoxelPos) at: index) - gridIntersect at: index) / (ray direction at: index)).
            ]
            ifTrue: [
                nextCrossingT at: index put: currentT + 
                    ((((self minPointAt: currentVoxelPos + 1) at: index) - gridIntersect at: index) / (ray direction at: index)).
            ]
    ]
]
