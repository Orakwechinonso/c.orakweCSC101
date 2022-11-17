//**************************************************************************/
// Copyright (c) 2011 Autodesk, Inc.
// All rights reserved.
// 
// These coded instructions, statements, and computer programs contain
// unpublished proprietary information written by Autodesk, Inc., and are
// protected by Federal copyright law. They may not be disclosed to third
// parties or copied or duplicated in any form, in whole or in part, without
// the prior written consent of Autodesk, Inc.
//**************************************************************************/
// DESCRIPTION: PointCloud simple rendering effect.
// AUTHOR: Danny Chen
// CREATED: April 2012
//**************************************************************************/

float gPointSize = 100.0f; // point size constant
float2 gPixelSize; //Inverse screen size

float4x4 gWVPXf : WorldViewProjection < string UIWidget = "None"; >; // transform matrix
float3 gLowLeft;

struct VS_INPUT
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
};

float4 GetColor(VS_INPUT input)
{
	 return float4(input.voxelColor.a,
                 input.voxelColor.b,
                 input.voxelColor.g,
                 input.voxelColor.r);
	
}



float3 GetPosition(VS_INPUT input)
{
	return float3((input.voxelInfoX&0xfff)*0.001 + gLowLeft.x,
	             ((input.voxelInfoX>>12)&0xfff)*0.001 + gLowLeft.y,
				 (input.voxelInfoY&0xfff)*0.001 + gLowLeft.z);
}

int GetClipped(VS_INPUT input)
{
    return (input.voxelInfoY>>27)&0x01;
}

int GetFiltered(VS_INPUT input)
{
    return (input.voxelInfoY >> 26) & 0x01;
}

struct VS_TO_PS
{
    float4 HPos       : SV_Position;	
    float4 Color      : TEXCOORD0;
};


VS_TO_PS SimplePointCloud_VS(VS_INPUT In)
{
	float3 IN_POS = GetPosition(In);
	float4 IN_COLOR = GetColor(In);   

	VS_TO_PS output;
	float4 wPos = float4(IN_POS,1.0f);
    output.HPos = mul(wPos,gWVPXf); // compute transformed position   
	output.Color = IN_COLOR;
	
	return output;
}


float4 SimplePointCloud_PS(VS_TO_PS In): SV_Target
{
	return float4(In.Color.xyz,1.0f);
}

technique10 RenderPoint
{
    pass P0
    {         
	    SetVertexShader( CompileShader( vs_4_0, SimplePointCloud_VS() ) );
        SetGeometryShader( NULL);
        SetPixelShader( CompileShader( ps_4_0, SimplePointCloud_PS() ) );
    }
}