#ifndef _PC_STYLIZATION_VERTEX_UTILITY_LIDAR_H_
#define _PC_STYLIZATION_VERTEX_UTILITY_LIDAR_H_

#include "VertexCommon.h"

float3 GetPosition(VertexInput input)
{
    float3 lowLeft = GetBasePos(input);

    return float3(input.voxelPos.x*0.001f + lowLeft.x,
	              input.voxelPos.y*0.001f + lowLeft.y,
		          input.voxelPos.z*0.001f + lowLeft.z);
}

float4 GetColor(VertexInput input)
{
   return float4(input.voxelColor.r,
                 input.voxelColor.g,
                 input.voxelColor.b,
                 input.voxelColor.a);
}

int GetClassID(VertexInput input)
{
    return ((input.voxelMisc>>14)&0xff);
}

int GetClipped(VertexInput input)
{
    return (input.voxelMisc >> 31) & 0x01;
}

int GetFiltered(VertexInput input)
{
    return (input.voxelMisc >> 30) & 0x01;
}
#endif