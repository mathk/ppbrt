{ import: Aggregate }

GridAccelerator : Aggregate (nVoxelX nVoxelY nVoxelZ)

GridAccelerator primitives: allPrimitives refineBefor: refineNeeded
[
    self := super primitives: allPrimitives refineBefor: refineNeeded.
    
]

GridAccelerator worldBbox
[
    ^primitives inject: (primitives at: 1) worldBbox into: [:el :rsl |
        el union: rsl
    ]
]
