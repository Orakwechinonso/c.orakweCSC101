#ifndef _PC_STYLIZATION_VERTEX_SHADER_H_
#define _PC_STYLIZATION_VERTEX_SHADER_H_

struct VertexOutput
{
    float4 HPos;
    float2 Property; // x point size, y discard
    float3 WPos;
};

#define PSIZE_PROP   Property.x
#define DISCARD_PROP Property.y

float4 GetModelPosition(VertexInput In)
{
    float3 IN_POS = GetPosition(In);

    return float4(IN_POS,1.0f);
}

float3 GetWorldPosition(float4 modelPos)
{
    return ComputeTransformedVec(modelPos).xyz;
    
}
    
void SetPointSize(out float psize)
{
     psize = ComputePointSize();
}


bool RegionDeleted(int id)
{
    return id == 61;
}


bool RegionInvisible(float alpha)
{
    return alpha < 0.25f;
}

void SetInvisibleRegion(float regionFlag, inout float discardFlag)
{
    // region invisible
    if (RegionInvisible(regionFlag))
        discardFlag = 2.0f;
}


float4 GetTrueColor(VertexInput In)
{
    float4 IN_COLOR = GetColor(In);
    float4 color = float4(IN_COLOR.rgb,1.0f);  

    return color;
}

float4 GetSingleColor(VertexInput In)
{
    float4 IN_COLOR = GetColor(In);
    float4 color = float4(gPointColor.xyz,IN_COLOR.w);

    return color;
}

#if LIMITBOX_ENABLE
float4 GetLimitBoxHighlight(float discardFlag, float4 color)
{
    if ((gHighLightIntersect||gLimitBoxShowOut)&&
        ((discardFlag > 0.5f)&&(discardFlag < 1.2f)))
    {
        return gLimitBoxColor;
    }    
    else 
        return color;
}
#endif

#if HIGHLIGHT_SCAN_ENABLE
float4 GetScanHighlight(float4 color)
{
    if (gScanHighLight)
		color.xyz = lerp(color.xyz,gScanColor.xyz,gScanColor.w);

    return color;
}
#endif

#if HIGHLIGHT_REGION_ENABLE
float4 GetRegionHighlight(float4 color, float4 regionColor)
{    
    if (regionColor.w < 0.75f)
	    color.xyz = lerp(color.xyz,regionColor.xyz,gRegionFactor);

    return color;
}
#endif

#if LIGHTING_ENABLE
float4 GetLightingColor(VertexInput In, float3 worldPos,float4 color)
{
    color.xyz = ComputeLightingColor(ComputeFinalNormal(In), worldPos, color.xyz);

    return color;
}
#endif 

void SetAllPositions(VertexInput In, out float4 HPos,out float3 WPos)
{
    float4 modelPos = GetModelPosition(In);

    HPos = mul(modelPos,gWVPXf); // compute transformed position

    WPos = GetWorldPosition(modelPos);
}



#if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
void SetPosition(VertexInput In, out float4 HPos,out float3 WPos)
{ 
    SetAllPositions(In, HPos, WPos);
}

void SetDeletedRegion(out float discardFlag, float3 WPos, int regionID)
{
    // layer id == 61 means deleted points
#if LIMITBOX_ENABLE
    discardFlag = RegionDeleted(regionID) ? 2.0f:
        ComputeLimitBox(WPos);  //if higher than 1.0 discard vertex    
#else
    discardFlag = RegionDeleted(regionID) ? 2.0f:0.0f;

#endif
}

void SetDiscardFlag(VertexInput In, float3 WPos, inout float discardFlag, out float4 regionColor)
{
    discardFlag = 0.0f;
    int    IN_LAYER = GetRegionIndex(In);

    // set deleted region flag.
    SetDeletedRegion(discardFlag, WPos, IN_LAYER);

    // set region invisible flag.
    regionColor = InternelGetRegion(IN_LAYER);
    SetInvisibleRegion(regionColor.w, discardFlag);

    // set clip flag
    int clipFlag = GetClipped(In);
    if (clipFlag == 1)
        discardFlag = 2.0f;

    int filterFlag = GetFiltered(In);
    if (filterFlag == 1)
        discardFlag = 2.0f;
}

void SetVertexProperties(VertexInput In, out float4 HPos, out float3 WPos, out float discardFlag, out float4 regionColor)
{
     //set position  
    SetPosition(In, HPos, WPos);     

    // set discard flag
    SetDiscardFlag(In, WPos, discardFlag, regionColor);
}
float4 GetVertexColor(VertexInput In, float3 WPos, float discardFlag, float4 color, float4 regionColor)
{
 // highlight limit-box
#if LIMITBOX_ENABLE
    color = GetLimitBoxHighlight(discardFlag, color);   
#endif 
    	   	
    //scan highlight
#if HIGHLIGHT_SCAN_ENABLE
    color = GetScanHighlight(color);
#endif

    // region highlight
#if HIGHLIGHT_REGION_ENABLE
    color = GetRegionHighlight(color, regionColor);
#endif

    // lighting color
#if LIGHTING_ENABLE
    color = GetLightingColor(In, WPos, color);
#endif	

    return color;
}
float4 VertexTrueColor(VertexInput In, out float4 HPos, out float3 WPos, out float discardFlag)
{
    // set position and discard flag.
    float4 regionColor;
    SetVertexProperties(In, HPos, WPos, discardFlag, regionColor);

    // get true color    
    float4 color = GetTrueColor(In);

    // set color
    color = GetVertexColor(In, WPos, discardFlag, color, regionColor);

    return color;
}
float4 VertexSingleColor(VertexInput In, out float4 HPos, out float3 WPos, out float discardFlag)
{
    // set position and discard flag.
    float4 regionColor;
    SetVertexProperties(In, HPos, WPos, discardFlag, regionColor);

    // get true color    
    float4 color = GetSingleColor(In);

    // set color
    color = GetVertexColor(In, WPos, discardFlag, color, regionColor);

    return color;
}
#else
void SetPosition(VertexInput In, out float4 HPos)
{
    float4 modelPos = GetModelPosition(In);

    HPos = mul(modelPos,gWVPXf); // compute transformed position
}
void SetDeletedRegion(out float discardFlag, int regionID)
{
    // layer id == 61 means deleted points
    discardFlag = RegionDeleted(regionID) ? 2.0f: 0.0f;
}
void SetDiscardFlag(VertexInput In,  out float discardFlag, out float4 regionColor)
{
    discardFlag = 0.0f;
    int    IN_LAYER = GetRegionIndex(In);

    // set deleted region flag.
    SetDeletedRegion(discardFlag, IN_LAYER);

    // set region invisible flag.
    regionColor = InternelGetRegion(IN_LAYER);
    SetInvisibleRegion(regionColor.w, discardFlag);

    // set clip flag
    int clipFlag = GetClipped(In);
    if (clipFlag == 1)
        discardFlag = 2.0f;

    int filterFlag = GetFiltered(In);
    if (filterFlag == 1)
        discardFlag = 2.0f;
}

void SetVertexProperties(VertexInput In, out float4 HPos, out float discardFlag, out float4 regionColor)
{
     //set position  
    SetPosition(In, HPos);     

    // set discard flag
    SetDiscardFlag(In, discardFlag, regionColor);
}
float4 GetVertexColor(float4 color, float4 regionColor)
{    	   	
    //scan highlight
#if HIGHLIGHT_SCAN_ENABLE
    color = GetScanHighlight(color);
#endif

    // region highlight
#if HIGHLIGHT_REGION_ENABLE
    color = GetRegionHighlight(color, regionColor);
#endif

    return color;
}
float4 VertexTrueColor(VertexInput In, out float4 HPos, out float discardFlag)
{
    // set position and discard flag.
    float4 regionColor;
    SetVertexProperties(In, HPos, discardFlag, regionColor);

    // get true color    
    float4 color = GetTrueColor(In);

    // set color
    color = GetVertexColor(color, regionColor);

    return color;
}
float4 VertexSingleColor(VertexInput In, out float4 HPos, out float discardFlag)
{
    // set position and discard flag.
    float4 regionColor;
    SetVertexProperties(In, HPos, discardFlag, regionColor);

    // get true color    
    float4 color = GetSingleColor(In);

    // set color
    color = GetVertexColor(color, regionColor);

    return color;
}
#endif



// vertex shaders
GeometryInput PointCloud_VS(VertexInput In)
{
    VertexOutput output;

    // set point size
    SetPointSize(output.PSIZE_PROP);

    // set position, color and discard flag.
 #if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
    float4 color = VertexTrueColor(In, output.HPos, output.WPos, output.DISCARD_PROP);
 #else
    float4 color = VertexTrueColor(In, output.HPos, output.DISCARD_PROP);
 #endif

    // fill output structure
    GeometryInput gs_output;
    
    gs_output.HPos = output.HPos;
    gs_output.Property = output.Property;
    gs_output.Color = float4(color.xyz, 1.0);;
    
    return gs_output;
   
}


PixelInput_NOGS PointCloud_VS_NOGS(VertexInput In)
{
    VertexOutput output;

    // set position, color and discard flag.
 #if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
    float4 color = VertexTrueColor(In, output.HPos, output.WPos, output.DISCARD_PROP);
 #else
    float4 color = VertexTrueColor(In, output.HPos, output.DISCARD_PROP);
 #endif

    // fill output structure.
    PixelInput_NOGS ps_output;
    
    ps_output.HPos = output.HPos;
    ps_output.Color = float4(color.xyz, 1.0);
    ps_output.Discard = output.DISCARD_PROP;

    return ps_output;
}

GeometryInput PointCloudSingleColor_VS(VertexInput In)
{
    VertexOutput output;

    // set point size
    SetPointSize(output.PSIZE_PROP);

    // set position, color and discard flag.
#if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
    float4 color = VertexSingleColor(In, output.HPos, output.WPos, output.DISCARD_PROP);
#else
    float4 color = VertexSingleColor(In, output.HPos, output.DISCARD_PROP);
#endif

    // fill output structure
    GeometryInput gs_output;

    gs_output.HPos = output.HPos;
    gs_output.Property = output.Property;
    gs_output.Color = float4(color.xyz, 1.0);;
    
    
    return gs_output;
   
}

PixelInput_NOGS PointCloudSingleColor_VS_NOGS(VertexInput In)
{
    VertexOutput output;

    // set position, color and discard flag.
#if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
    float4 color = VertexSingleColor(In, output.HPos, output.WPos, output.DISCARD_PROP);
#else
    float4 color = VertexSingleColor(In, output.HPos, output.DISCARD_PROP);
#endif

    // fill output structure.
    PixelInput_NOGS ps_output;
    
    ps_output.HPos = output.HPos;
    ps_output.Color = float4(color.xyz, 1.0);
    ps_output.Discard = output.DISCARD_PROP;

    return ps_output;
}


#endif