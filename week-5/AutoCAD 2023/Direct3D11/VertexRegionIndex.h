#ifndef _PC_STYLIZATION_VERTEX_REGION_INDEX_H_
#define _PC_STYLIZATION_VERTEX_REGION_INDEX_H_


#include "VertexCommon_Region.h"

int GetRegionIndex(VertexInput input)
{
    unsigned int index = (input.voxelInfoX>>24)&0xff;//(input.voxelInfoX>>24)&0xff

    return RegionIndex(index);
}

#endif