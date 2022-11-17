#ifndef _PC_STYLIZATION_VERTEX_DEF_H_
#define _PC_STYLIZATION_VERTEX_DEF_H_

#if LIDAR_FORMAT_ENABLE
    #include "VertexUtilityLidar.h"
    #include "VertexRegionIndexLidar.h"    
    #include "VertexRegionLidar.h"
#else
    #include "VertexUtility.h"
    #include "VertexRegionIndex.h"
    #include "VertexRegion.h"
#endif

#if LIMITBOX_ENABLE
    #include "VertexLimitBox.h"
#endif 

#endif