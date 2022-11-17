
#ifndef _PC_STYLIZATION_VERTEX_INPUT_LIDAR_H_
#define _PC_STYLIZATION_VERTEX_INPUT_LIDAR_H_

struct VertexInput
{
    float3 voxelPos: POSITION;
    float4 voxelColor: COLOR;

    // 0-13 normal 14 bits
    // 14-21 layer 8 bits
    // 22-30 class 8 bits
    int    voxelMisc: TEXCOORD0;  

#if VOXEL_CONSOLIDATION
    int                 lowLeftIdx : TEXCOORD2;
#endif

};

#endif