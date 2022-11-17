#ifndef _PC_STYLIZATION_Geometry_SHADER_H_
#define _PC_STYLIZATION_Geometry_SHADER_H_

[maxvertexcount(4)]
void PointCloud_GS(point GeometryInput input[1], inout TriangleStream<PixelInput> outputStream)
{
    PixelInput output;

    // Initialize vertex attributes.
    output.Color = input[0].Color;
	
    // Region filtering.
    if (input[0].DISCARD_PROP > 1.2f)
	    return;
		
    // Compute point size offsets in screen space.
    float2 size = float2(
        input[0].PSIZE_PROP / gScreenSize.x,
        input[0].PSIZE_PROP / gScreenSize.y);

    output.HPos = input[0].HPos;
    float w = output.HPos.w;

    // Perspective divide.
    output.HPos /= w;

    float x = output.HPos.x;
    float y = output.HPos.y;

    // Output tristrip quad.
    output.HPos.x = x - size.x;
    output.HPos.y = y + size.y;
    outputStream.Append(output);

    //output.HPos.x = x - size.x;
    output.HPos.y = y - size.y;
    outputStream.Append(output);

    output.HPos.x = x + size.x;
    output.HPos.y = y + size.y;
    outputStream.Append(output);

    //output.HPos.x = x + size.x;
    output.HPos.y = y - size.y;
    outputStream.Append(output);

    outputStream.RestartStrip();
}

#endif