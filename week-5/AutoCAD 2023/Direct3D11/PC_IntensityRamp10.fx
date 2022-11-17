// test macros
#include "Macros.h" // debug only, disable it in formal submission.

// includes.
#include "includes.h"
#include "VertexShaderGradient.h"

// techinique
technique10 RenderIntensityRamp
{
    pass P0
    {         
		SetVertexShader( CompileShader( vs_4_0, PointCloudIntensityRamp_VS() ) );
        SetGeometryShader( CompileShader( gs_4_0, PointCloud_GS() ) );
        SetPixelShader( CompileShader( ps_4_0, PointCloud_PS() ) );
    }
}