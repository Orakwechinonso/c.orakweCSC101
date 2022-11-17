#ifndef _PC_STYLIZATION_VERTEX_NORMAL_H_
#define _PC_STYLIZATION_VERTEX_NORMAL_H_

#include "VertexCommon_Normal.h"

float3 GetNormal(VertexInput input)
{
    int normalIndex = (input.voxelInfoY>>12)&0x3fff;

	return LoadNormal(normalIndex);
}

#endif