#version 330 core
layout (location = 0) in vec3 aPos;   // the position variable has attribute position 0
layout (location = 1) in vec3 aColor; // the color variable has attribute position 1
layout (location = 2) in vec3 aNormal; // Normal vector of the current face

uniform mat4 pvMatrix;
uniform mat4 modelMatrix;

out vec3 ourColor;
out vec3 Normal;
out vec3 FragPos;

void main()
{
   gl_Position = pvMatrix * modelMatrix * vec4(aPos.x, aPos.y, aPos.z, 1.0);
   ourColor = aColor;
   Normal = aNormal;
   FragPos = vec3(modelMatrix * vec4(aPos, 1.0));
}