Shader "Unlit/SGSSS_Test"
{
    Properties
    {
        _MainTex ("Texture", 2D) = "white" {}
        _ScatterAmt("ScatterAmt", Vector) = (1, 1, 1, 1)
    }
    SubShader
    {
        Tags { "RenderType"="Opaque" }
        LOD 100

        Pass
        {
            HLSLPROGRAM
            #pragma vertex vert
            #pragma fragment frag
            // make fog work
            #pragma multi_compile_fog

            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Shadows.hlsl"

            struct appdata
            {
                float4 posOS : POSITION;
                float3 normalOS : NORMAL;
                float2 uv : TEXCOORD0;
            };

            struct v2f
            {
                float2 uv : TEXCOORD0;
                float3 normalWS : TEXCOORD1;
                float3 posWS : TEXCOORD2;
                float4 posCS : SV_POSITION;
            };

            TEXTURE2D(_MainTex);
            SAMPLER(sampler_MainTex);
            float4 _MainTex_ST;
            float3 _ScatterAmt;
            v2f vert (appdata v)
            {
                v2f o;
                o.posWS = TransformObjectToWorld(v.posOS);
                o.posCS = TransformObjectToHClip(v.posOS);
                o.normalWS = TransformObjectToWorldNormal(v.normalOS);
                o.uv = TRANSFORM_TEX(v.uv, _MainTex);
                return o;
            }
            
            struct SphericalGaussian
            {
                float3 Amplitude;  // float3或者float皆可，按需求设定
                float3 Axis;
                float Sharpness;
            };

            float3 EvaluateSG(in SphericalGaussian sg, in float3 dir)
            {
                float cosAngle = dot(dir, sg.Axis);
                return sg.Amplitude * exp(sg.Sharpness * (cosAngle - 1.0f));
            }

            // SG Kernel
            SphericalGaussian MakeNormalizedSG(float3 LightDir, half Sharpness)
            {
                // 归一化的SG
                SphericalGaussian  SG;
                SG.Axis = LightDir;
                SG.Sharpness = Sharpness; // (1 / ScatterAmt.element)
                SG.Amplitude = SG.Sharpness / ((2 * PI) - (2 * PI) * exp(-2 * SG.Sharpness)); // 归一化处理
                
                return SG;
            }

            float SGIrradianceFitted(in SphericalGaussian lightingLobe, in float3 normal)
            {
                const float muDotN = dot(lightingLobe.Axis, normal);
                const float lambda = lightingLobe.Sharpness;
             
                const float c0 = 0.36f;
                const float c1 = 1.0f / (4.0f * c0);
             
                float eml  = exp(-lambda);
                float em2l = eml * eml;
                float rl   = rcp(lambda);
             
                float scale = 1.0f + 2.0f * em2l - rl;
                float bias  = (eml - em2l) * rl - em2l;
             
                float x  = sqrt(1.0f - scale);
                float x0 = c0 * muDotN;
                float x1 = c1 * x;
             
                float n = x0 + x1;
             
                float y = saturate(muDotN);
                if(abs(x0) <= x1)
                    y = n * n / x;
             
                float result = scale * y + bias;
             
                return result;
            }

            float3 SGDiffuseLighting(float3 N, float3 L, float3 ScatterAmt)
            {
                SphericalGaussian RedKernel = MakeNormalizedSG(L, 1/max(ScatterAmt.x, 0.0001f));
                SphericalGaussian GreenKernel = MakeNormalizedSG(L, 1/max(ScatterAmt.y, 0.0001f));
                SphericalGaussian BlueKernel = MakeNormalizedSG(L, 1/max(ScatterAmt.z, 0.0001f));
                float3 Diffuse = float3(SGIrradianceFitted(RedKernel, N), SGIrradianceFitted(GreenKernel, N), SGIrradianceFitted(BlueKernel, N));

                //filmic tone mapping
                float3 x = max(0, Diffuse - 0.004f);
                Diffuse = (x*(6.2f*x + 0.5f))/(x*(6.2f*x + 1.7f) + 0.06f);
                return Diffuse;
            }

            float curvature(float3 VetexNormal, float3 WorldPositon)
            {
                float DeltaN = length(abs(ddx(VetexNormal))+abs(ddy(VetexNormal)));
                float DeltaP = length(abs(ddx(WorldPositon))+abs(ddy(WorldPositon)));
                float Curvature = DeltaN / DeltaP;
                return Curvature;
            }
            
            float4 frag (v2f i) : SV_Target
            {
                Light light = GetMainLight();
                float3 lightColor = light.color;
                float3 L = normalize(light.direction);
                float3 N = normalize(i.normalWS);
                float NdotL = saturate(dot(N, L));
                float viewDir = normalize(i.posWS - _WorldSpaceCameraPos);
                float halfVector = normalize(viewDir + L);
                float Curvature = curvature(i.normalWS, i.posWS);
                float NdotH = saturate(dot(N, halfVector));
                float4 albedo = SAMPLE_TEXTURE2D(_MainTex, sampler_MainTex, i.uv);
                float Diffuse = NdotL*albedo;
                float3 SGDiffuse = SGDiffuseLighting(N, L, _ScatterAmt*Curvature)*albedo*lightColor;
                return float4(SGDiffuse,1);
            }
            ENDHLSL
        }
    }
}
