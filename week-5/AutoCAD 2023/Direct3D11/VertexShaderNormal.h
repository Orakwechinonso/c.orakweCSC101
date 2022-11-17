#ifndef _PC_STYLIZATION_VERTEX_SHADER_NORMAL_H_
#define _PC_STYLIZATION_VERTEX_SHADER_NORMAL_H_

#include "NormalDef.h"
#include "VertexShader.h"

GeometryInput PointCloudNormalRamp_VS(VertexInput In)
{
    VertexOutput output;

    // set point size
    SetPointSize(output.PSIZE_PROP);
    
    // set position and discard flag.
    float4 regionColor;
#if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
    SetVertexProperties(In, output.HPos, output.WPos, output.DISCARD_PROP, regionColor);
#else
    SetVertexProperties(In, output.HPos, output.DISCARD_PROP, regionColor);
#endif

    // get normal color    
    float4 color = GetNormalColor(In);

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

PixelInput_NOGS PointCloudNormalRamp_VS_NOGS(VertexInput In)
{
    VertexOutput output;

    // set position and discard flag.
    float4 regionColor;
#if (LIMITBOX_ENABLE || LIGHTING_ENABLE)
    SetVertexProperties(In, output.HPos, output.WPos, output.DISCARD_PROP, regionColor);
#else
    SetVertexProperties(In, output.HPos, output.DISCARD_PROP, regionColor);
#endif

    // get normal color    
    float4 color = GetNormalColor(In);

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