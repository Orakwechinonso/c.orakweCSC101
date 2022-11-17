#ifndef _PC_STYLIZATION_VERTEX_LIMIT_BOX_H_
#define _PC_STYLIZATION_VERTEX_LIMIT_BOX_H_


//properties for the limit box                                                                                   
float4                                  gLimitBoxPlanes[LIMITBOX_PLANES];  //a box with 6 planes.  
float                                   gLimitBoxBorders[LIMITBOX_PLANES];
bool                                    gHighLightIntersect; // highlight intersect point
bool                                    gInvertLimitBox;
bool                                    gLimitBoxShowOut;
float4                                  gLimitBoxColor;
int                                     gLimitPlaneCount;

//calculates distance to plane                                       
float planeDistance(float4 planeEq, float3 pos )    
{                                                                                        
       float dotProd = dot( planeEq.xyz, pos.xyz );        
       return ( dotProd + planeEq.w );                               
} 


//check if point inside limit box
int pointInsideLimitBox( float3 pnt )                                        
{                     
    bool onEdge = false;                                                                                        
    for( int i = 0; i < gLimitPlaneCount; i++ )                                                      
    {    
        float dist = planeDistance( gLimitBoxPlanes[i],  pnt );                                                                                                  
        if(dist  <  0.0 )       
            return 2;          //outside       
        else if (dist < gLimitBoxBorders[i])
            onEdge = true;                                        
    }                                                                                                      
    return onEdge?1:0;                                                                                     
}  
float ComputeLimitBox(float3 pos)
{
    int val = pointInsideLimitBox( pos );

    val = gInvertLimitBox?2-val:val;   
    
    if (gLimitBoxShowOut)
    {
        val = val>=1?1:val;
    }                
    return (float)val;
}

#endif