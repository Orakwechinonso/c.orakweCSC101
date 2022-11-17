#ifndef _PC_STYLIZATION_PIXEL_INPUT_H_
#define _PC_STYLIZATION_PIXEL_INPUT_H_

struct PixelInput
{
    float4 HPos       	 : SV_Position;	
	float4 Color         : TEXCOORD0;

};

struct PixelInput_NOGS
{
    float4 HPos       	 : SV_Position;	
	float4 Color         : TEXCOORD0;
    float  Discard       : TEXCOORD1;
    
};

#endif 