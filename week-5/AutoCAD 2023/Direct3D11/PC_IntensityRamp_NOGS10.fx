// test macros
#include "Macros.h" // debug only, disable it in formal submission.

// includes.
#include "includes.h"
#include "VertexShaderGradient.h"

// techinique
technique10 RenderIntensityRampNoGS
{
    pass P0
    {         
		SetVertexShader( CompileShader( vs_4_0, PointCloudIntensityRamp_VS_NOGS() ) );
        SetGeometryShader( NULL );
        SetPixelShader( CompileShader( ps_4_0, PointCloud_PS_NOGS() ) );
    }
}