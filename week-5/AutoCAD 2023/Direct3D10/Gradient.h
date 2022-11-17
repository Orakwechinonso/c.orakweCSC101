#ifndef _PC_STYLIZATION_GRADIENT_H_
#define _PC_STYLIZATION_GRADIENT_H_


// parameters for gradient texture
float gGradientMaxValue;
float gGradientMinValue;
float gGradientScale;
float2 gThreshold;
bool  gFilterGradient;
int   gGradientOption;
float4 gMaxGradientColor;
float4 gMinGradientColor;
int   gGradientSize;

// gradient texture and samplers
Texture1D gGradientTex;

float ComputeGradProp(float val)
{
	return (val-gGradientMinValue)/(gGradientMaxValue - gGradientMinValue)*gGradientScale;
}

bool4 GradientOnBorder(float val)
{
    bool4 border;
    border.x  = (val >= 1.0f); // upBorder
    border.y  = (val <= 0.0f); //downBorder
    border.z = border.x||border.y; // outborder
    border.w  = ((gGradientOption == 1)&&(border.z)); //discard

    return border;
    
}

float4 GradientColorLinear(float val)
{ 
    float new_val = val*gGradientSize - 0.5f;
    int index = (int)(new_val);
    float frac_val = new_val - index;
    
    float4 color1 = gGradientTex.Load(int2(index,0));
    float4 color2 = gGradientTex.Load(int2(index+1,0));
    
    
    return lerp(color1,color2,frac_val);
}

float4 GradientColorPoint(float val)
{
    int index = (int)(val*gGradientSize - 0.5f);
    
    return gGradientTex.Load(int2(index,0));
}

float4 GradientColor(float4 color, float4 trueColor,bool4 border)
{
    return (gGradientOption == 2)?
                (border.x?
                     gMaxGradientColor:
                 border.y?
                     gMinGradientColor: color):
                (border.z? 
                     float4(trueColor.rgb,1.0f):color);
}


bool DiscardGradient(float alpha)
{
    return ((gFilterGradient)&&(alpha<0.5f));
}

float4 GetGradientColorLinear(float value,  float4 trueColor)
{
    float val = ComputeGradProp(value);  
    bool4 border = GradientOnBorder(val);

    float4 color = GradientColorLinear(val);

    color = GradientColor(color, trueColor,border);

    if (border.w) 
        color.a = 0.0f;

   return color;
}

float4 GetGradientColorPoint(float value, float4 trueColor)
{
    float val = ComputeGradProp(value);  
    bool4 border = GradientOnBorder(val);

    float4 color = GradientColorPoint(val);

    color = GradientColor(color, trueColor,border);

    if (border.w) 
        color.a = 0.0f;

   return color;
}



#endif