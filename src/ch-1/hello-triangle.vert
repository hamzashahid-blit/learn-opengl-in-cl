#version 330 core
layout (location = 0) in vec3 aPos;
out vec4 myColor; 

void main()
{
	gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
	myColor = vec4(aPos + 0.5, 1.0);
}
