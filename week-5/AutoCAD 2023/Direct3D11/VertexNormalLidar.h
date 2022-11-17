#ifndef _PC_STYLIZATION_VERTEX_NORMAL_LIDAR_H_
#define _PC_STYLIZATION_VERTEX_NORMAL_LIDAR_H_

#include "VertexCommon_Normal.h"

float3 GetNormal(VertexInput input)
{
    int normalIndex = (input.voxelMisc)&0x3fff;

	return LoadNormal(normalIndex);
}

#endif