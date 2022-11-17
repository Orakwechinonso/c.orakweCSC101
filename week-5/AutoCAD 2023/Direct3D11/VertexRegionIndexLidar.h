#ifndef _PC_STYLIZATION_VERTEX_REGION_INDEX_LIDAR_H_
#define _PC_STYLIZATION_VERTEX_REGION_INDEX_LIDAR_H_

#include "VertexCommon_Region.h"

int GetRegionIndex(VertexInput input)
{
    unsigned int index = (input.voxelMisc>>22)&0xff;//(input.voxelMisc>>22)&0xff

    return RegionIndex(index);
}



#endif