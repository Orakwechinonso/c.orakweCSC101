#ifndef _PC_STYLIZATION_VERTEX_REGION_LIDAR_H_
#define _PC_STYLIZATION_VERTEX_REGION_LIDAR_H_

#include "VertexRegionIndexLidar.h"

float4 GetRegion(VertexInput input)
{
    return InternelGetRegion(GetRegionIndex(input));
}

#endif