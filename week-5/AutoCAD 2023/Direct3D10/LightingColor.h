#ifndef _PC_STYLIZATION_LIGHTING_COLOR_H_
#define _PC_STYLIZATION_LIGHTING_COLOR_H_

// lighting parameter
#define LIGHT_COUNT 3

struct Light
{
    // light color
    float3 Color;

    // light ambient color
    float3 AmbColor;

    // light specular color
    float3 SpecColor;

    // light direction
    float3 Dir;

    // these for are reserved for future extension
    float3 Pos;
    float4 Atten;
    float2 Cone;
};

// light set
Light gLightList[LIGHT_COUNT]: LightArray;

// light count
int gNumDirectionalLights : DirLightCount;

// specular power
float gGlossiness: SpecularPower;
bool gSpecularEnable;
float4 gEdgeColor;

// ReCap lighting params.
bool gReCapLightingEnable;
float3 gCameraPosition;

float3 ComputeDirectionalLight(Light light, float3 V, float3 N, float3 inColor)
{
    // Material reflectance.
    const float Ka = 0.4f;  // ambient
    const float Kd = 1.0f;  // diffuse
    const float Ks = 0.8f;  // specular

    // Get light direction (in world coords).
    float3 L = normalize(mul(light.Dir, (float3x3)gWXf));
    float3 R = reflect(L, N);

    // Compute ambient reflectance.
    float3 Ia = Ka * inColor;

    // Compute diffuse reflectance.
    float3 Id = Kd * inColor  * saturate(dot(N, L));

    // Compute specular reflectance.
    float3 Is = gSpecularEnable ? Ks * pow(saturate(dot(R, V)), gGlossiness) 
                                : 0.0f;

    // Apply lighting.
    return   Ia * light.AmbColor
           + Id * light.Color
           + Is * light.SpecColor;
}

// Compute AutoCAD-style lighting (with lambert/phong switch)
float3 ComputeAutoCADLighting(float3 normal,float3 viewDir, float3 WPos, float3 inColor)
{
    // Global ambient reflectance.
    float  Kga = 0.2f;

    // Initial surface color output (ambient only).
    float3 outColor = inColor * Kga;

    // Compute viewer direction.
    float3 V = viewDir;

    // Get surface normal.
    float3 N = normal;
#if TWOSIDEDLIGHTING_ENABLE
    N = faceforward(N, V, N);
#endif

    // Accumulate lighting.
    if (gNumDirectionalLights > 1)
        outColor += ComputeDirectionalLight(gLightList[1], V, N, inColor);
    
    if (gNumDirectionalLights > 2)
        outColor += ComputeDirectionalLight(gLightList[2], V, N, inColor);

    return outColor;
}

// Shader for Recap-style lighting (lambert/phong).
float3 ComputeReCapLighting(float3 normal, float3 viewDir, float3 WPos,float3 color)
{
    // Material reflectance.
    float Ka = 0.3f;
    float Kd = 0.8f;
    float Ks = 0.8f;
    float Kgloss = 15.0f;

    // Compute viewer direction.
    float3 V = viewDir;

    // Get surface normal.
    float3 N = normal;
#if TWOSIDEDLIGHTING_ENABLE
    N = faceforward(N, V, N);
#endif
    float3 L = -V;
    float3 R = reflect(L, N);

    // Compute ambient reflectance.
    float3 Ia = Ka * color;

    // Compute diffuse reflectance.
    float3 Id = Kd * color * saturate(dot(N, L));

    // Compute specular reflectance.
    float3 Is = gSpecularEnable ? Ks * pow(saturate(dot(R, V)), Kgloss) 
                                : 0.0f;

    // Combine lighting.
    return Ia + Id + Is;
}

float3 ComputeLightingColor(float3 normal, float3 WPos,float3 color)
{
    // Compute viewer direction.
    float3 V = normalize(gCameraPosition - WPos);

    // Get surface normal.
    float3 N = normalize(normal);
    
    
#if LIGHTING_ENABLE
    if (gReCapLightingEnable)
        color =  ComputeReCapLighting(N, V, WPos, color);
    else
        color = ComputeAutoCADLighting(N, V, WPos, color);
#endif

#if HIGHLIGHT_EDGE_ENABLE
    float NdotV = abs(dot(N,V));
    if (NdotV <0.2f)
    {
        color.xyz = color.xyz + gEdgeColor.xyz*gEdgeColor.a;
    }
#endif

    return color;

}

#endif
