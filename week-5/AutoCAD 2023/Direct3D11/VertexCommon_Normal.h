#ifndef _PC_STYLIZATION_VERTEX_COMMON_NORMAL_H_
#define _PC_STYLIZATION_VERTEX_COMMON_NORMAL_H_

#include "VertexCommon.h"

// normal map
Texture2D gNormalTex;

float3 LoadNormal(int normalIndex)
{
    int3 index;
	index.y = ((unsigned int)normalIndex)/1014;
	index.x = normalIndex - index.y*1014;
	index.z = 0;
	
    return gNormalTex.Load(index).xyz ;
}

float3 ComputeNormal(float3 normal_value)
{
    float3 normalParam = normalize(normal_value);

    float3 normal_result = gUseOriginWorld?
        mul(normalParam, (float3x3)gOriginNormalMatrix):
        mul(normalParam, (float3x3)gNormalMatrix);
        
    return normalize(normal_result);
}


#endif