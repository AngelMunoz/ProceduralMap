// Trivial shader to test cross-platform compilation
matrix World;
matrix View;
matrix Projection;

struct VertexShaderInput
{
    float4 Position : POSITION0;
};

struct VertexShaderOutput
{
    float4 Position : SV_Position;
    float3 TexCoord : TEXCOORD0;
};

VertexShaderOutput MainVS(in VertexShaderInput input)
{
    VertexShaderOutput output = (VertexShaderOutput)0;
    float4 worldPosition = mul(input.Position, World);
    float4 viewPosition = mul(worldPosition, View);
    output.Position = mul(viewPosition, Projection);
    output.TexCoord = input.Position.xyz;
    return output;
}

float4 MainPS(VertexShaderOutput input) : COLOR0
{
    float3 dir = normalize(input.TexCoord);
    float3 topColor = float3(0.0, 0.05, 0.1);
    float3 bottomColor = float3(0.0, 0.0, 0.01);
    float3 color = lerp(bottomColor, topColor, max(0.0, dir.y));
    return float4(color, 1.0);
}

technique ProceduralNightSky
{
    pass P0
    {
        VertexShader = compile vs_3_0 MainVS();
        PixelShader = compile ps_3_0 MainPS();
    }
};
