#ifndef _PC_STYLIZATION_CONSTANTS_H_
#define _PC_STYLIZATION_CONSTANTS_H_

//more project constants in:
// lightingColor.h
// vertexcommon_region.h
// vertexcommon_normal.h
// vertexlimitbox.h
//

// OGS internal constants
float4x4 gNormalMatrix : WorldInverseTranspose < string UIWidget = "None"; >; // the matrix to transform normal
float4x4 gWVPXf : WorldViewProjection < string UIWidget = "None"; >; // transform matrix   
float4x4 gVPXf : ViewProjection < string UIWidget = "None"; >; // view-projection transform matrix   
float4x4 gWXf: World < string UIWidget = "None"; >; // world
float4x4 gVIXf: ViewInverse < string UIWidget = "None"; >;
float3 gViewDirection : ViewDirection < string UIWidget = "None"; >;
float2 gScreenSize: ViewportPixelSize  < string UIWidget = "None"; >;

// point size
int   gMaxPointSize;        //in pixels, used to compute point size                              
int   gMinPointSize;        //used to compute point size  
int   gPointSize;            //default point size, decide if we need to adjust point size 


// the original world/normal transformation if we apply locale 
// in OGS transformations.
bool gUseOriginWorld;
float4x4 gOriginWXf;
float4x4 gOriginNormalMatrix;

// locale info
float3 gLocale;

// point single color.
float4	gPointColor; 

float   gTransAlpha;

#if HIGHLIGHT_SCAN_ENABLE
float4  gScanColor;
bool gScanHighLight;
#endif

// per voxel properties.
#if VOXEL_CONSOLIDATION
    // Base positions texture.
    Texture1D gLowLeftsTex;
#else
    float3 gLowLeft;
#endif






#endif