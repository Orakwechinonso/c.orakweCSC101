#ifndef _PC_STYLIZATION_VERTEX_COMMON_H_
#define _PC_STYLIZATION_VERTEX_COMMON_H_

#include "Constants.h"

#if LIDAR_FORMAT_ENABLE
#include "VertexInputLidar.h"
#else
#include "VertexInput.h"
#endif

float3 GetBasePos(VertexInput input)
{
#if VOXEL_CONSOLIDATION
    int2 index = int2(input.lowLeftIdx, 0);
    float3 lowLeft = gLowLeftsTex.Load(index);
#else
    float3 lowLeft = gLowLeft;
#endif 

    return lowLeft;
}

float ComputePointSize()
{                                                                                                                                                  
    return gPointSize == 0? 1.0f:gPointSize;  	
}

// compute tranformed position in world space
float4 ComputeTransformedVec(float4 wPos)
{
    return gUseOriginWorld?
            mul(wPos, gOriginWXf):
            mul(wPos, gWXf);
}






#endif