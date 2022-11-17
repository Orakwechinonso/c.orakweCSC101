#ifndef _PC_STYLIZATION_NORMAL_DEF_H_
#define _PC_STYLIZATION_NORMAL_DEF_H_


#if LIDAR_FORMAT_ENABLE
        #include "VertexNormalLidar.h"
#else
        #include "VertexNormal.h" 
#endif

float3 ComputeFinalNormal(VertexInput In)
{
    float3 IN_NORMAL = GetNormal(In);
    
    return ComputeNormal(IN_NORMAL);
}

float4 GetNormalColor(VertexInput In)
{
    float4 IN_COLOR = GetColor(In);
    float3 normal_value = ComputeFinalNormal(In);

    return float4(normal_value*0.5f + 0.5f, IN_COLOR.w);
}

#endif