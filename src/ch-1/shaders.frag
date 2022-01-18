#version 330 core
out vec4 FragColor;
in vec3 outColor;

uniform vec4 myColor; // Not using this for now

void main()
{
	FragColor = vec4(outColor.x + myColor.x,
	 		  		 outColor.y + myColor.y,
					 outColor.z + myColor.z, 1.0);
}
