#ifndef _PC_STYLIZATION_VERTEX_UTILITY_H_
#define _PC_STYLIZATION_VERTEX_UTILITY_H_

#include "VertexCommon.h"

float3 GetPosition(VertexInput input)
{
    float3 lowLeft = GetBasePos(input);

	return float3((input.voxelInfoX&0xfff)*0.001 + lowLeft.x,
	             ((input.voxelInfoX>>12)&0xfff)*0.001 + lowLeft.y,
		      	  (input.voxelInfoY&0xfff)*0.001 + lowLeft.z);
}

float4 GetColor(VertexInput input)
{
   return float4(input.voxelColor.a,
                 input.voxelColor.b,
                 input.voxelColor.g,
                 input.voxelColor.r);
}

int GetClipped(VertexInput input)
{
    return (input.voxelInfoY >> 27) & 0x01;
}

int GetFiltered(VertexInput input)
{
    return (input.voxelInfoY >> 26) & 0x01;
}

#endif