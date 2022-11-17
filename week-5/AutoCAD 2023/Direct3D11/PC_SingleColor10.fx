// test macros
#include "Macros.h" // debug only, disable it in formal submission.

// includes.
#include "includes.h"

// techinique
technique10 RenderSingleColor
{
    pass P0
    {         
		SetVertexShader( CompileShader( vs_4_0, PointCloudSingleColor_VS() ) );
        SetGeometryShader( CompileShader( gs_4_0, PointCloud_GS() ) );
        SetPixelShader( CompileShader( ps_4_0, PointCloud_PS() ) );
    }
}