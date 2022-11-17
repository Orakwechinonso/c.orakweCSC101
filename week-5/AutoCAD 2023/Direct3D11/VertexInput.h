#ifndef _PC_STYLIZATION_VERTEX_INPUT_H_
#define _PC_STYLIZATION_VERTEX_INPUT_H_

struct VertexInput
{
    // #1
	//  0-11 x    12 bits
	// 12-23 y    12 bits
	// 24-31 layer 8 bits
	// #2
	//  0-11 z    12 bits
	// 12-25 normal 14 bits
	// 26-32 reserved
	// #3
	// 0-31  color
	
	int voxelInfoX: TEXCOORD0;
    int voxelInfoY: TEXCOORD1;
    float4 voxelColor: COLOR;

#if VOXEL_CONSOLIDATION
    int                 lowLeftIdx : TEXCOORD2;
#endif
};

#endif
