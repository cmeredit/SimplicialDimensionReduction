package GLUtil

import org.joml.Matrix4f

import scala.math.{cos, sin, toRadians}



class HighDimViewer(val position: (Int, Int), val size: (Int, Int),
                    points: Vector[Vector[Float]], colors: Vector[Vector[Float]]) {




  object Positioning {
    var positionVector: Vector[Double] = (0 until pointDimension).map(_ => 0.0).toVector

    var frame: Vector[Vector[Double]] = (0 until pointDimension).map((i: Int) => {
      (0 until pointDimension).map((j: Int) => if (j == i) 1.0 else 0.0).toVector
    }).toVector



    def rotateFrame(i: Int, j: Int, theta: Double): Unit = {
      val transformation: Vector[Double] => Vector[Double] = VectorUtil.getRotationFunction(frame(i), frame(j), theta)
      frame = frame.map(transformation)
    }

    def moveByFrameVec(i: Int, distance: Double): Unit = {
      positionVector = positionVector.zip(frame(i).map(_ * distance)).map(p => p._1 + p._2)
    }
  }




  private val FLOAT_SIZE: Int = 4

  private val pointDimension: Int =
    if (points.isEmpty || points(0).isEmpty)
      0
    else
      points(0).length

  private val helperMat4 = new Matrix4f()
  private val mouseSensitivity: Float = 0.1f
  private val movementSpeed: Float = 2.5f

  val meshes: Map[(Int, Int, Int), Mesh] = getUnorderedIndexTriples.map({case (i1, i2, i3) =>
    val (data, indices): (Array[Float], Array[Int]) = assembleDataArray(i1, i2, i3)

//    println(data.mkString(", "))
//    println(indices.mkString(", "))

    val position: Attribute = Attribute(0, 3, 9 * FLOAT_SIZE, 0)
    val color: Attribute = Attribute(1, 3, 9 * FLOAT_SIZE, 3 * FLOAT_SIZE)
    val normal: Attribute = Attribute(2, 3, 9 * FLOAT_SIZE, 6 * FLOAT_SIZE)

    ((i1, i2, i3), new Mesh(data, Vector(position, color, normal), indices,
      "Shaders/SH1/test.vs", "Shaders/SH1/test.fs"))
  }).toMap

  private val rotationAngles: scala.collection.mutable.Map[(Int, Int), Double] = scala.collection.mutable.Map() ++
    {
      for (i <- 0 until pointDimension;
           j <- 0 until pointDimension if i != j
           )
        yield ((i, j), if (i < j) -45.0 else 45.0)
    }.toMap

  private val trueCameraPosition: scala.collection.mutable.Map[Int, Float] = scala.collection.mutable.Map() ++
    (0 until pointDimension).map(x => (x, -6.0f)).toMap

//  private val frame: scala.collection.mutable.Map[Int, Vector[Double]] = scala.collection.mutable.Map() ++
//    (0 until pointDimension).map(n => (n, (0 until pointDimension).map(i => if (i == n) 1.0 else 0.0).toVector)).toMap

  def getViewMatrix(i: Int, j: Int, k: Int): Matrix4f = {

    val roll: Double = rotationAngles(i, j)
    val pitch: Double = rotationAngles(k, j)
    val yaw: Double = rotationAngles(k, i)

    val frontX: Float = (sin(toRadians(yaw)) * cos(toRadians(pitch))).toFloat
    val frontY: Float = sin(toRadians(pitch)).toFloat
    val frontZ: Float = (cos(toRadians(yaw)) * cos(toRadians(pitch))).toFloat

    // Up:
    val upx: Float = (cos(yaw)*sin(roll) - sin(yaw) * sin(pitch) * cos(roll)).toFloat
    val upy: Float = (cos(pitch) * cos(roll)).toFloat
    val upz: Float = (-sin(yaw)*sin(roll) - cos(yaw) * sin(pitch) * cos(roll)).toFloat

    helperMat4
      .identity()
      .lookAt(
        trueCameraPosition(i), trueCameraPosition(j), trueCameraPosition(k),
        trueCameraPosition(i) + frontX, trueCameraPosition(j) + frontY, trueCameraPosition(k) + frontZ,
        0.0f, 1.0f, 0.0f
      )
//      .rotate(toRadians(roll).toFloat, frontX, frontY, frontZ)
  }

  def processMouse(i: Int, j: Int, k: Int, xOffset: Float, yOffset: Float): Unit = {
    rotationAngles((k, i)) = rotationAngles((k, i)) - xOffset * mouseSensitivity
    rotationAngles((i, k)) = rotationAngles((i, k)) + xOffset * mouseSensitivity

    rotationAngles((k, j)) = -89.0f max rotationAngles((k, j)).toFloat + yOffset * mouseSensitivity min 89.0f
    rotationAngles((j, k)) = -89.0f max rotationAngles((j, k)).toFloat + yOffset * mouseSensitivity min 89.0f
  }

  def processKeyboard(i: Int, j: Int, k: Int, direction: CameraMovement, deltaTime: Float): Unit = {
    val speed: Float = movementSpeed * deltaTime

    val frontX: Float = (sin(toRadians(rotationAngles(k, i))) * cos(toRadians(rotationAngles(k, j)))).toFloat
    val frontY: Float = sin(toRadians(rotationAngles(k, j))).toFloat
    val frontZ: Float = (cos(toRadians(rotationAngles(k, i))) * cos(toRadians(rotationAngles(k, j)))).toFloat

    val rightX: Float = cos(toRadians(rotationAngles(k, i))).toFloat
    val rightY: Float = 0.0f
    val rightZ: Float = -sin(toRadians(rotationAngles(k, i))).toFloat

    direction match {
      case Forward =>
        trueCameraPosition(i) = trueCameraPosition(i) + frontX * speed
        trueCameraPosition(j) = trueCameraPosition(j) + frontY * speed
        trueCameraPosition(k) = trueCameraPosition(k) + frontZ * speed
      case Backward =>
        trueCameraPosition(i) = trueCameraPosition(i) - frontX * speed
        trueCameraPosition(j) = trueCameraPosition(j) - frontY * speed
        trueCameraPosition(k) = trueCameraPosition(k) - frontZ * speed
      case Right =>
        trueCameraPosition(i) = trueCameraPosition(i) - rightX * speed
        trueCameraPosition(j) = trueCameraPosition(j) - rightY * speed
        trueCameraPosition(k) = trueCameraPosition(k) - rightZ * speed
      case Left =>
        trueCameraPosition(i) = trueCameraPosition(i) + rightX * speed
        trueCameraPosition(j) = trueCameraPosition(j) + rightY * speed
        trueCameraPosition(k) = trueCameraPosition(k) + rightZ * speed
    }
  }

  private def assembleDataArray(index1: Int, index2: Int, index3: Int): (Array[Float], Array[Int]) = {
    val projections: Vector[Array[Float]] = points.map(v => Array(v(index1), v(index2), v(index3)))

    // Contains cubes as a long array of floats and true indices...
    val cubeRadius = 0.005f
    val cubes: Vector[(Array[Float], Array[Int])] = projections.zipWithIndex.map({case (center, index) =>
      // Use p for a positive shift, n for a negative shift, in dimension given by position
      val nnn: Array[Float] = Array(center(0) - cubeRadius, center(1) - cubeRadius, center(2) - cubeRadius)
      val nnp: Array[Float] = Array(center(0) - cubeRadius, center(1) - cubeRadius, center(2) + cubeRadius)
      val npn: Array[Float] = Array(center(0) - cubeRadius, center(1) + cubeRadius, center(2) - cubeRadius)
      val npp: Array[Float] = Array(center(0) - cubeRadius, center(1) + cubeRadius, center(2) + cubeRadius)
      val pnn: Array[Float] = Array(center(0) + cubeRadius, center(1) - cubeRadius, center(2) - cubeRadius)
      val pnp: Array[Float] = Array(center(0) + cubeRadius, center(1) - cubeRadius, center(2) + cubeRadius)
      val ppn: Array[Float] = Array(center(0) + cubeRadius, center(1) + cubeRadius, center(2) - cubeRadius)
      val ppp: Array[Float] = Array(center(0) + cubeRadius, center(1) + cubeRadius, center(2) + cubeRadius)

      val RECIP_ROOT_THREE: Float = 0.57735026919f

      val normalConversion: Map[Array[Float], Array[Float]] = Vector(
        (nnn, Array(-RECIP_ROOT_THREE, -RECIP_ROOT_THREE, -RECIP_ROOT_THREE)),
        (nnp, Array(-RECIP_ROOT_THREE, -RECIP_ROOT_THREE,  RECIP_ROOT_THREE)),
        (npn, Array(-RECIP_ROOT_THREE,  RECIP_ROOT_THREE, -RECIP_ROOT_THREE)),
        (npp, Array(-RECIP_ROOT_THREE,  RECIP_ROOT_THREE,  RECIP_ROOT_THREE)),
        (pnn, Array( RECIP_ROOT_THREE, -RECIP_ROOT_THREE, -RECIP_ROOT_THREE)),
        (pnp, Array( RECIP_ROOT_THREE, -RECIP_ROOT_THREE,  RECIP_ROOT_THREE)),
        (ppn, Array( RECIP_ROOT_THREE,  RECIP_ROOT_THREE, -RECIP_ROOT_THREE)),
        (ppp, Array( RECIP_ROOT_THREE,  RECIP_ROOT_THREE,  RECIP_ROOT_THREE))
      ).toMap

      val cubeCoordinatesWithColors: Array[Float] = Seq(nnn, nnp, npn, npp, pnn, pnp, ppn, ppp).map((vertexArray: Array[Float]) => vertexArray ++ colors(index) ++ normalConversion(vertexArray)).reduce(_ ++ _)
      // Indices for the six sides
      val frontIndices: Array[Int] = Array(4, 5, 6, 5, 6, 7)
      val backIndices: Array[Int] = Array(0, 1, 2, 1, 2, 3)
      val leftIndices: Array[Int] = Array(0, 1, 4, 1, 4, 5)
      val rightIndices: Array[Int] = Array(2, 3, 6, 3, 6, 7)
      val topIndices: Array[Int] = Array(1, 3, 5, 3, 5, 7)
      val bottomIndices: Array[Int] = Array(0, 2, 4, 2, 4, 6)
      val indices = (frontIndices ++ backIndices ++ leftIndices ++ rightIndices ++ topIndices ++ bottomIndices)
        .map(_ + index * 8) // Get the true indices, not just the local indices!!!

      (cubeCoordinatesWithColors, indices)
    })
    val (coordinates, indices): (Array[Array[Float]], Array[Array[Int]]) = cubes.toArray.unzip

    (coordinates.flatten, indices.flatten)
  }

  def getUnorderedIndexTriples: Vector[(Int, Int, Int)] = {
    val allNontrivialTriples = for (
      i <- 0 until pointDimension;
      j <- 0 until pointDimension;
      k <- 0 until pointDimension
      if i < j && j < k && i < k
    ) yield (i, j, k)

    // DistinctBy _.toSet
    allNontrivialTriples.toVector //.groupBy(t => Set(t._1, t._2, t._3)).map(_._2.head).toVector

  }

}