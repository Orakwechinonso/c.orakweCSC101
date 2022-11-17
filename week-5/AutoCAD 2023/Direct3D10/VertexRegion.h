#ifndef _PC_STYLIZATION_VERTEX_REGION_H_
#define _PC_STYLIZATION_VERTEX_REGION_H_

#include "VertexRegionIndex.h"

float4 GetRegion(VertexInput input)
{
    return InternelGetRegion(GetRegionIndex(input));
}

#endif