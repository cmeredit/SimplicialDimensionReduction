#version 330 core
out vec4 FragColor;

in vec3 ourColor;
in vec3 Normal;
in vec3 FragPos;

uniform vec4 uniformColor;

void main()
{
    vec3 lightColor = vec3(1.0, 1.0, 1.0);
    float ambientStrength = 0.1;
    vec3 ambient = ambientStrength * lightColor;

    vec3 lightPos = vec3(2.0, 2.0, 2.0);
    vec3 lightDir = normalize(lightPos - FragPos);

    float diff = max(dot(Normal, lightDir), 0.0);
    vec3 diffuse = diff * lightColor;

    vec3 result = (ambient + diffuse) * ourColor;

    FragColor = vec4(result, 1.0);
}