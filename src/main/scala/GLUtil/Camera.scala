package GLUtil

import scala.math.{cos, sin, toRadians}

import org.joml.{Matrix4f, Vector3f, Vector3fc}
//import org.lwjgl.system.MemoryUtil

//import java.nio.FloatBuffer

sealed trait CameraMovement
case object Forward extends CameraMovement
case object Backward extends CameraMovement
case object Right extends CameraMovement
case object Left extends CameraMovement

// NOTE: JOML uses an update-in-place-and-return-result-reference paradigm

// Matrix4f, Vector3f, etc. are mutable, while Matrix4fc, Vector3fc, etc. are immutable. Compare to Scala's
// mutable vs. immutable collections. Don't be fooled - a Matrix4f val is a constant reference to a mutable container.
// The purpose of the val here is to guarantee that at runtime, we don't allocate matrices, vectors, etc. all over
// the place

class Camera(
              private val position: Vector3f = new Vector3f(0.0f, 0.0f, 0.0f),

              private val up: Vector3f = new Vector3f(0.0f, 1.0f, 0.0f),
              private val front: Vector3f = new Vector3f(0.0f, 0.0f, 1.0f),

              private val movementSpeed: Float = 2.5f,
              private val mouseSensitivity: Float = 0.1f,

              private var yaw: Float = 0.0f,
              private var pitch: Float = 0.0f,

              private var zoom: Float = 45.0f
            ) {
  private val matrix: Matrix4f = new Matrix4f()
//  private val cameraFloatBuffer: FloatBuffer = MemoryUtil.memAllocFloat(16)

  private val worldUp: Vector3fc = new Vector3f(0.0f, 1.0f, 0.0f)
  private val right: Vector3f = new Vector3f()
  private val lookingAt: Vector3f = new Vector3f()

  updateCameraVectors()

  private def updateCameraVectors(): Unit = {
    // Update front
    // Front is (0, 0, 1) with a pitch rotation, followed by a yaw rotation
    front.x = (sin(toRadians(yaw)) * cos(toRadians(pitch))).toFloat
    front.y = sin(toRadians(pitch)).toFloat
    front.z = (cos(toRadians(yaw)) * cos(toRadians(pitch))).toFloat
    //front.normalize()

    // Update right
    front.cross(worldUp, right)
    right.normalize()

    // Update up
    right.cross(front, up)
  }

  def getViewMatrix: Matrix4f = {

    position.add(front, lookingAt)

    matrix
      .identity()
      .lookAt(
        position.x, position.y, position.z,
        lookingAt.x, lookingAt.y, lookingAt.z,
        up.x, up.y, up.z
      )
  }

//  private def getViewMatrixFloatBuffer: FloatBuffer = {
//    getViewMatrix.get(cameraFloatBuffer)
//  }

  def processKeyboard(direction: CameraMovement, deltaTime: Float): Unit = {
    val speed: Float = movementSpeed * deltaTime
    direction match {
      case Forward => front.mulAdd(speed, position, position)
      case Backward => front.mulAdd(-speed, position, position)
      case Right => right.mulAdd(speed, position, position)
      case Left => right.mulAdd(-speed, position, position)
    }
  }

  def processMouse(xOffset: Float, yOffset: Float, constrainPitch: Boolean = true): Unit = {
    yaw = yaw - xOffset * mouseSensitivity
    pitch = pitch + yOffset * mouseSensitivity

    // (lower max x min upper) == clamp(lower, x, upper)
    if (constrainPitch) pitch = -89.0f max pitch min 89.0f

    updateCameraVectors()
  }

  def processScroll(yOffset: Float): Unit = {
    zoom = zoom - yOffset

    // (lower max x min upper) == clamp(lower, x, upper)
    zoom = 1.0f max zoom min 45.0f
  }
}
