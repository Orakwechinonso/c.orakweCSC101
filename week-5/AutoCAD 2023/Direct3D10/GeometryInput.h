#ifndef _PC_STYLIZATION_GEOMETRY_INPUT_H_
#define _PC_STYLIZATION_GEOMETRY_INPUT_H_

struct GeometryInput
{
    float4 HPos       : POSITION;  // all
    float4 Color      : TEXCOORD0;
	float2 Property	  : TEXCOORD1; // x: point size, all
                                   // y: discard, all

};

#endif