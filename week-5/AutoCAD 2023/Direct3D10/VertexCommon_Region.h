#ifndef _PC_STYLIZATION_VERTEX_COMMON_REGION_H_
#define _PC_STYLIZATION_VERTEX_COMMON_REGION_H_

#include "VertexCommon.h"

// region map
Texture1D gRegionTex;
float gRegionFactor;
int   gGlobalRegionIndex;

int RegionIndex(int index)
{
    return (gGlobalRegionIndex == -1)?index:gGlobalRegionIndex;
}

// internal used method
float4 InternelGetRegion(int regionIndex)
{
    int2 index = int2(regionIndex,0);
    return gRegionTex.Load(index);
}

#endif