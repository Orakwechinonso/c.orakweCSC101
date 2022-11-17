// test macros
#include "Macros.h" // debug only, disable it in formal submission.

// includes.
#include "includes.h"

// techinique
technique10 RenderSingleColorNoGS
{
    pass P0
    {         
		SetVertexShader( CompileShader( vs_4_0, PointCloudSingleColor_VS_NOGS() ) );
        SetGeometryShader( NULL );
        SetPixelShader( CompileShader( ps_4_0, PointCloud_PS_NOGS() ) );
    }
}