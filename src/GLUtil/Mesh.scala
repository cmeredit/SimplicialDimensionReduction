package GLUtil

import org.joml.Matrix4f
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.{GL_TRIANGLES, GL_UNSIGNED_INT}
import org.lwjgl.opengl.GL20.{glGetUniformLocation, glUniform4f, glUniformMatrix4fv}
import org.lwjgl.opengl.{GL11, GL15, GL20, GL30}
import org.lwjgl.system.MemoryUtil

import java.nio.{FloatBuffer, IntBuffer}
import scala.io.{BufferedSource, Source}

case class Attribute(shaderLocation: Int, size: Int, stride: Int, offset: Int)

class Mesh(data: Array[Float], attributes: Vector[Attribute], indices: Array[Int],
           vertexShaderSource: String, fragmentShaderSource: String) {

  // Helper for passing our data to openGL
  private val dataBuffer: FloatBuffer = BufferUtils
    .createFloatBuffer(data.length)
    .put(data)
    .flip()

  // Useful for the glDrawElements function
  val numVertices: Int = data.length

  // Helper for passing our indices to openGL
  private val indexBuffer: IntBuffer = BufferUtils
    .createIntBuffer(indices.length)
    .put(indices)
    .flip()

  // Make and bind a new VAO for this mesh
  val vao: Int = GL30.glGenVertexArrays()
  GL30.glBindVertexArray(vao)

  // Make and bind a new VBO to store our attributes
  val vbo: Int = GL15.glGenBuffers()
  GL15.glBindBuffer(GL15.GL_ARRAY_BUFFER, vbo)

  // Send our data to openGL
  GL15.glBufferData(GL15.GL_ARRAY_BUFFER, dataBuffer, GL15.GL_STATIC_DRAW)

  // Set up our attributes for openGL
  attributes.foreach(attribute => {
    GL20.glVertexAttribPointer(
      attribute.shaderLocation, // Layout location of the attribute in shader
      attribute.size, // Number of floats in the attribute
      GL11.GL_FLOAT, // Only use float attributes
      false, // Never normalize our data
      attribute.stride, // Number of bytes between entries for this attribute in data array
      attribute.offset // Number of bytes to first entry for this attribute in data array
    )
    GL20.glEnableVertexAttribArray(attribute.shaderLocation)
  })

  // Unbind the vbo
  GL15.glBindBuffer(GL15.GL_ARRAY_BUFFER, 0)

  // Make and bind a new VBO for our indices (ebo: Element Buffer Object)
  val ebo: Int = GL15.glGenBuffers()
  GL15.glBindBuffer(GL15.GL_ELEMENT_ARRAY_BUFFER, ebo)

  // Send our indices to openGL
  GL15.glBufferData(GL15.GL_ELEMENT_ARRAY_BUFFER, indexBuffer, GL15.GL_STATIC_DRAW)

  // Do NOT unbind the ebo: The vao will store this unbind call.

  // Unbind the vao
  GL30.glBindVertexArray(0)

  // Load the shader strings
  private val bufferedVertexSource: BufferedSource = Source.fromFile(vertexShaderSource)
  private val vertexShaderString: String = bufferedVertexSource.mkString("")
  bufferedVertexSource.close()
  private val bufferedFragmentSource: BufferedSource = Source.fromFile(fragmentShaderSource)
  private val fragmentShaderString: String = bufferedFragmentSource.mkString("")
  bufferedFragmentSource.close()

  // Compile the vertex shader
  private val vertexShader: Int = GL20.glCreateShader(GL20.GL_VERTEX_SHADER)
  GL20.glShaderSource(vertexShader, vertexShaderString)
  GL20.glCompileShader(vertexShader)

  // Compile the fragment shader
  private val fragmentShader: Int = GL20.glCreateShader(GL20.GL_FRAGMENT_SHADER)
  GL20.glShaderSource(fragmentShader, fragmentShaderString)
  GL20.glCompileShader(fragmentShader)

  // Set up the actual shader program
  val shaderProgram: Int = GL20.glCreateProgram
  GL20.glAttachShader(shaderProgram, vertexShader)
  GL20.glAttachShader(shaderProgram, fragmentShader)
  GL20.glLinkProgram(shaderProgram)
  GL20.glValidateProgram(shaderProgram)

//  println("Log")
//  println(GL20.glGetProgramInfoLog(shaderProgram))
//  println(GL20.glGetShaderi(vertexShader, GL20.GL_COMPILE_STATUS))
//  println(GL20.glGetShaderi(fragmentShader, GL20.GL_COMPILE_STATUS))

  // Dispose of unneeded shaders (we've compiled and linked them in shaderProgram and no longer need them
  // in isolation)
  GL20.glDeleteShader(vertexShader)
  GL20.glDeleteShader(fragmentShader)

  private val modelMatrix: Matrix4f = (new Matrix4f()).identity()
  private val modelFloatBuffer: FloatBuffer = MemoryUtil.memAllocFloat(16)

  def rotate(radians: Float, axisX: Float, axisY: Float, axisZ: Float): Unit = {
    modelMatrix.rotate(radians, axisX, axisY, axisZ)
  }

  def translate(x: Float, y: Float, z: Float): Unit = {
    modelMatrix.translate(x, y, z)
  }

  def setUniform4f(uniformName: String, v0: Float, v1: Float, v2: Float, v3: Float): Unit = {
    GL20.glUseProgram(shaderProgram)
    glUniform4f(glGetUniformLocation(shaderProgram, uniformName), v0, v1, v2, v3)
  }

  def setUniformMatrix4fv(uniformName: String, mat: Matrix4f, fb: FloatBuffer): Unit = {
    GL20.glUseProgram(shaderProgram)
    glUniformMatrix4fv(glGetUniformLocation(shaderProgram, uniformName), false, mat.get(fb))
  }

  def draw(): Unit = {
    GL20.glUseProgram(shaderProgram)
    setUniformMatrix4fv("modelMatrix", modelMatrix, modelFloatBuffer)

    GL30.glBindVertexArray(vao)
    GL11.glDrawElements(GL_TRIANGLES, numVertices, GL_UNSIGNED_INT, 0)
    GL30.glBindVertexArray(0)
  }
}