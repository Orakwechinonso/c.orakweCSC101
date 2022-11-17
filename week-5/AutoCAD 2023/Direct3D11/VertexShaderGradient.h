#ifndef _PC_STYLIZATION_VERTEX_SHADER_GRADIENT_H_
#define _PC_STYLIZATION_VERTEX_SHADER_GRADIENT_H_

#include "Gradient.h"
#include "VertexShader.h"


float4 ComputeGradientColor(float val, inout float discardFlag,float4 trueColor)
{
    float4 color = GetGradientColorLinear(val, trueColor);

    if (DiscardGradient(color.a))
        discardFlag = 2.0f;
	
    return color;
}

GeometryInput PointCloudHeightRamp_VS(VertexInput In)
{
    VertexOutput output;
    
     // get true color
    float4 trueColor = GetColor(In);

    // set point size
    SetPointSize(output.PSIZE_PROP);
    
    // set position and discard flag.
    float4 regionColor;
    
    //set position  
    SetAllPositions(In, output.HPos, output.WPos);     

#if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
     // set discard flag
    SetDiscardFlag(In, output.WPos, output.DISCARD_PROP, regionColor);
#else
    // set discard flag
    SetDiscardFlag(In, output.DISCARD_PROP, regionColor);
#endif

    // get normal color    
    float4 color = ComputeGradientColor(output.WPos.z + gLocale.z, output.DISCARD_PROP, trueColor);  

    // set color
#if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
    color = GetVertexColor(In, output.WPos, output.DISCARD_PROP, color, regionColor);
#else
    color = GetVertexColor(color, regionColor);
#endif

    // fill output structure
    GeometryInput gs_output;

    gs_output.HPos = output.HPos;
    gs_output.Property = output.Property;
    gs_output.Color = float4(color.xyz, 1.0);;
    
    return gs_output; 
}

PixelInput_NOGS PointCloudHeightRamp_VS_NOGS(VertexInput In)
{
    VertexOutput output;
    
     // get true color
    float4 trueColor = GetColor(In);
    
    // set position and discard flag.
    float4 regionColor;
    
    //set position  
    SetAllPositions(In, output.HPos, output.WPos);     

#if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
     // set discard flag
    SetDiscardFlag(In, output.WPos, output.DISCARD_PROP, regionColor);
#else
    // set discard flag
    SetDiscardFlag(In, output.DISCARD_PROP, regionColor);
#endif

    // get normal color    
    float4 color = ComputeGradientColor(output.WPos.z + gLocale.z, output.DISCARD_PROP, trueColor);  

    // set color
#if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
    color = GetVertexColor(In, output.WPos, output.DISCARD_PROP, color, regionColor);
#else
    color = GetVertexColor(color, regionColor);
#endif

    // fill output structure.
    PixelInput_NOGS ps_output;
    
    ps_output.HPos = output.HPos;
    ps_output.Color = float4(color.xyz, 1.0);
    ps_output.Discard = output.DISCARD_PROP;

    return ps_output;
}

GeometryInput PointCloudIntensityRamp_VS(VertexInput In)
{
    VertexOutput output;
    
    // get true color
    float4 trueColor = GetColor(In);

    // set point size
    SetPointSize(output.PSIZE_PROP);
    
    // set position and discard flag.
    float4 regionColor;
#if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
    SetVertexProperties(In, output.HPos, output.WPos, output.DISCARD_PROP, regionColor);
#else
    SetVertexProperties(In, output.HPos, output.DISCARD_PROP, regionColor);
#endif

    // get gradient color
    float4 color = ComputeGradientColor(trueColor.a, output.DISCARD_PROP, trueColor);  

    // set color
#if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
    color = GetVertexColor(In, output.WPos, output.DISCARD_PROP, color, regionColor);
#else
    color = GetVertexColor(color, regionColor);
#endif

    // fill output structure
    GeometryInput gs_output;

    gs_output.HPos = output.HPos;
    gs_output.Property = output.Property;
    gs_output.Color = float4(color.xyz, 1.0);
    
    return gs_output;
}

PixelInput_NOGS PointCloudIntensityRamp_VS_NOGS(VertexInput In)
{
    VertexOutput output;
    
    // get true color
    float4 trueColor = GetColor(In);
    
    // set position and discard flag.
    float4 regionColor;
#if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
    SetVertexProperties(In, output.HPos, output.WPos, output.DISCARD_PROP, regionColor);
#else
    SetVertexProperties(In, output.HPos, output.DISCARD_PROP, regionColor);
#endif

    // get gradient color
    float4 color = ComputeGradientColor(trueColor.a, output.DISCARD_PROP, trueColor);  

    // set color
#if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
    color = GetVertexColor(In, output.WPos, output.DISCARD_PROP, color, regionColor);
#else
    color = GetVertexColor(color, regionColor);
#endif

    // fill output structure.
    PixelInput_NOGS ps_output;
    
    ps_output.HPos = output.HPos;
    ps_output.Color = float4(color.xyz, 1.0);
    ps_output.Discard = output.DISCARD_PROP;

    return ps_output;
}

GeometryInput PointCloudClassificationRamp_VS(VertexInput In)
{
    VertexOutput output;

    // get true color
    float4 trueColor = GetColor(In);

    // set point size
    SetPointSize(output.PSIZE_PROP);

    // set position and discard flag.
    float4 regionColor;
#if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
    SetVertexProperties(In, output.HPos, output.WPos, output.DISCARD_PROP, regionColor);
#else
    SetVertexProperties(In, output.HPos, output.DISCARD_PROP, regionColor);
#endif

    // get gradient color
    int    IN_CLASSID = 0;

#if LIDAR_FORMAT_ENABLE
    IN_CLASSID = GetClassID(In);
#endif

    float4 color = ComputeGradientColor((float)IN_CLASSID+0.5f, output.DISCARD_PROP, trueColor);  
    
    // set color
#if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
    color = GetVertexColor(In, output.WPos, output.DISCARD_PROP, color, regionColor);
#else
    color = GetVertexColor(color, regionColor);
#endif

    // fill output structure
    GeometryInput gs_output;

    gs_output.HPos = output.HPos;
    gs_output.Property = output.Property;
    gs_output.Color = float4(color.xyz, 1.0);;
    
    return gs_output;
}

PixelInput_NOGS PointCloudClassificationRamp_VS_NOGS(VertexInput In)
{
    VertexOutput output;

    // get true color
    float4 trueColor = GetColor(In);

    // set position and discard flag.
    float4 regionColor;
#if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
    SetVertexProperties(In, output.HPos, output.WPos, output.DISCARD_PROP, regionColor);
#else
    SetVertexProperties(In, output.HPos, output.DISCARD_PROP, regionColor);
#endif

    // get gradient color
    int    IN_CLASSID = 0;

#if LIDAR_FORMAT_ENABLE
    IN_CLASSID = GetClassID(In);
#endif

    float4 color = ComputeGradientColor((float)IN_CLASSID+0.5f, output.DISCARD_PROP, trueColor);  
    
    // set color
#if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
    color = GetVertexColor(In, output.WPos, output.DISCARD_PROP, color, regionColor);
#else
    color = GetVertexColor(color, regionColor);
#endif

    // fill output structure.
    PixelInput_NOGS ps_output;
   
    ps_output.HPos = output.HPos;
    ps_output.Color = float4(color.xyz, 1.0);
    ps_output.Discard = output.DISCARD_PROP;

    return ps_output;
}



#endif
