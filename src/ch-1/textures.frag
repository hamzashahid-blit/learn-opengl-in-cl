#version 330 core
out vec4 FragColor;
in vec3 outColor;
in vec2 TexCoord;

uniform vec4 myColor;
uniform float texMix;
uniform sampler2D ourTexture;
uniform sampler2D ourTexture2;

void main()
{
	FragColor = vec4((outColor.x + myColor.x),
	 		  		 (outColor.y + myColor.y),
					 (outColor.z + myColor.z), 1.0) *
				mix(texture(ourTexture,  TexCoord),
				    texture(ourTexture2, vec2(1.0 - TexCoord.x, TexCoord.y)), texMix);
}
