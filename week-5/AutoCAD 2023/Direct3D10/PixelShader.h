#ifndef _PC_STYLIZATION_PIXEL_SHADER_H_
#define _PC_STYLIZATION_PIXEL_SHADER_H_

int gAlphaTestFunction : AlphaTestFunction                                                  
<                                                                                          
int UIMin = 1;                                                                         
int UIMax = 8;                                                                         
int UIStep = 1;                                                                        
> = 8;                                                                                        
float gAlphaTestReference : AlphaTestReference                                            
<                                                                                         
float UIMin = 0.0f;                                                                     
float UIMax = 1.0f;                                                                     
float UIStep = 0.1f;                                                                   
> = 1.0f;                                                                                   
bool alphaTest(float alpha)                                                               
{                                                                                         
    bool fail = false;                                                                    
    switch (gAlphaTestFunction)                                                            
    {                                                                                      
    case 1:        fail = true;                                     break;           
    case 2:        fail = alpha >= gAlphaTestReference - 1e-5f;     break;              
    case 4:        fail = alpha >  gAlphaTestReference + 1e-5f;     break;             
    case 5:        fail = alpha <= gAlphaTestReference + 1e-5f;     break;            
    case 7:        fail = alpha <  gAlphaTestReference - 1e-5f;     break;             
    case 3:        fail = abs(alpha - gAlphaTestReference) > 1e-5f; break;             
    case 6:        fail = abs(alpha - gAlphaTestReference) < 1e-5f; break;             
    }                                                                                      
    return fail;                                                                           
}

float4 PointCloud_PS(PixelInput In): SV_Target
{
    if (alphaTest(gTransAlpha)) 
        discard;

    return float4(In.Color.rgb, gTransAlpha);
}
void DiscardPixel(float flag)
{
    if (flag > 1.25f)
        discard;
}

float4 PointCloud_PS_NOGS(PixelInput_NOGS In): SV_Target
{
    DiscardPixel(In.Discard);

    if (alphaTest(gTransAlpha)) 
        discard;

    return float4(In.Color.rgb, gTransAlpha);
}

#endif
