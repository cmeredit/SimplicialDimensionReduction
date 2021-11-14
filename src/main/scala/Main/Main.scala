package Main

//import ScalaComp.Main
import DataLoaders.IrisLoader
import DimensionReduction.Delaunay.{LinearUtil, QuickHullUtil, Simplex}
import DimensionReduction.Point
import org.joml.Matrix4f
import org.lwjgl._
import org.lwjgl.glfw.Callbacks._
import org.lwjgl.glfw.GLFW._
import org.lwjgl.glfw._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl._
import org.lwjgl.system.MemoryStack._
import org.lwjgl.system.MemoryUtil._
import org.lwjgl.system._

import java.nio.DoubleBuffer
import _root_.GLUtil._

import scala.math.random

object Main extends App {

  // The window handle
  private var window = 0L

  private lazy val primaryMode: GLFWVidMode = glfwGetVideoMode(glfwGetPrimaryMonitor())

  // Matrix for the projection-view-model transform and its buffer
  private lazy val pvmFloatBuffer = MemoryUtil.memAllocFloat(16)
  private lazy val pvMatrix = new Matrix4f()

  // Time between frames
  private var deltaTime: Float = 0.0f
  private var lastFrame: Float = 0.0f

  private var lastX: Double = 0.0f
  private var lastY: Double = 0.0f
  private var firstMouse: Boolean = true


  private lazy val iris: (Vector[Vector[Float]], Vector[Vector[Float]]) = IrisLoader.getIrisData



  private sealed trait WhatToLoad
  private case object ShouldLoadIris extends WhatToLoad
  private case object ShouldLoadRandom3dHull extends WhatToLoad
  private case object ShouldLoadRandom3dSphere extends WhatToLoad
  private case object ShouldLoadRandomNormals extends WhatToLoad
  private val whatToLoad: WhatToLoad = ShouldLoadRandom3dSphere

  private val num3dPointsToHull: Int = 500

  private lazy val hdv: HighDimViewer = {

    println("AHHHHH FUCK HERE")

    println(whatToLoad)
    whatToLoad match {
      case ShouldLoadIris => new HighDimViewer((0,0), (10,10), iris._1, iris._2)
      case ShouldLoadRandom3dHull =>
        val points: Vector[Point] = (1 to num3dPointsToHull).map(_ => Vector(random(), random(), random())).toVector.map(Point)

        var currentPoints: Vector[Point] = points
        var previousNumPoints: Int = currentPoints.length
        var currentHull: Vector[Simplex] = QuickHullUtil.getConvexHull(currentPoints)
        var currentNumPoints: Int = currentPoints.length

        do {
          previousNumPoints = currentNumPoints

          currentPoints = currentHull.flatMap(_.vertices).distinct

          currentHull = QuickHullUtil.getConvexHull(currentPoints)

          currentNumPoints = currentHull.flatMap(_.vertices).distinct.length
        } while (currentNumPoints != previousNumPoints)


        val convexHull: Vector[Simplex] = currentHull

        val convexHullVertices: Vector[Point] = convexHull.flatMap(_.vertices)
        val interiorVertices: Vector[Vector[Float]] = points.diff(convexHullVertices).map(_.map(_.toFloat))

        val floatVertexInfo: Vector[Vector[Float]] = convexHullVertices.map(_.map(_.toFloat)) ++ interiorVertices
        val floatColorInfo: Vector[Vector[Float]] = convexHullVertices.map(_ => Vector(0.0f, 0.0f, 1.0f)) ++ interiorVertices.map(_ => Vector(1.0f, 0.0f, 0.0f))


        new HighDimViewer((0,0), (10, 10), floatVertexInfo, floatColorInfo)
      case ShouldLoadRandom3dSphere =>
        val points: Vector[Point] = (1 to num3dPointsToHull).map(_ => {

          val theta: Double = random() * 2.0 * scala.math.Pi
          val phi: Double = random() * scala.math.Pi

          Vector(scala.math.sin(phi) * scala.math.cos(theta), scala.math.sin(phi) * scala.math.sin(theta), scala.math.cos(phi))


          val r: Double = (random() * 2.0 - 1.0) * 4.0

          val x: Double = r * scala.math.cos(theta) //random() * 2.0 - 1.0
          val y: Double = r * scala.math.sin(theta) //random() * 2.0 - 1.0
          Vector(x, x*x + y*y, y)

        }).toVector.map(Point)

        val convexHull: Vector[Simplex] = QuickHullUtil.getConvexHull(points)

        val convexHullVertices: Vector[Point] = convexHull.flatMap(_.vertices)
        val interiorVertices: Vector[Vector[Float]] = points.diff(convexHullVertices).map(_.map(_.toFloat))

        val numPerNormal: Int = 40
        val normalLength: Double = 0.1


        val numConnectingVertices: Int = 60

        val floatVertexInfo: Vector[Vector[Float]] = convexHullVertices.map(_.map(_.toFloat)) ++
          interiorVertices ++
          convexHull.flatMap((s: Simplex) => {
            val centroid: Point = Point(s.vertices.map(_.coordinates).transpose.map(_.sum / 3.0))
            centroid.zip(s.normalVector).map({case (a, b) => (a+b).toFloat})
            centroid.map(_.toFloat)

            (0 until numPerNormal).map((n: Int) => centroid.zip(s.normalVector).map({case (a, b) => (a + b * (n.toDouble / numPerNormal.toDouble) * normalLength).toFloat})) ++ s.vertices.combinations(2).flatMap(combo => {
              val x0 = combo(0)(0)
              val y0 = combo(0)(1)
              val z0 = combo(0)(2)
              val x1 = combo(1)(0)
              val y1 = combo(1)(1)
              val z1 = combo(1)(2)

              val dx = x1-x0
              val dy = y1-y0
              val dz = z1-z0


              (0 until numConnectingVertices).map(n => {
                val t = n.toDouble / numConnectingVertices.toDouble
                Vector(x0 + t*dx, y0 + t*dy, z0 + t*dz)
              }).toVector.map(_.map(_.toFloat))
            })

          })



        val floatColorInfo: Vector[Vector[Float]] = convexHullVertices.map(_ => Vector(0.0f, 0.0f, 1.0f)) ++
          interiorVertices.map(_ => Vector(1.0f, 0.0f, 0.0f)) ++
          convexHull.flatMap(_ => (0 until numPerNormal - 1).map(_ => Vector(0.0f, 1.0f, 0.0f)).toVector ++ Vector(Vector(1.0f, 1.0f, 1.0f)) ++ Vector.fill(3 * numConnectingVertices)(Vector(0.0f, 0.0f, 0.0f)))



        //      points.foreach((v: Point) => {
        //        println("Minimum distance to a simplex in the convex hull " + convexHull.map(_.signedDistance(v)).max)
        //      })



        new HighDimViewer((0,0), (10, 10), floatVertexInfo, floatColorInfo)

      case ShouldLoadRandomNormals =>

        val pointAngles: Vector[(Double, Double)] = (1 to num3dPointsToHull).map(_ => {

          val theta: Double = random() * 2.0 * scala.math.Pi
          val phi: Double = random() * scala.math.Pi

          (phi, theta)
        }).toVector

        val numPerNormal: Int = 20
        val normalLength: Double = 0.2

        val floatVertexInfo: Vector[Vector[Float]] = pointAngles.map({case (phi, theta) => Vector(scala.math.sin(phi) * scala.math.cos(theta), scala.math.sin(phi) * scala.math.sin(theta), scala.math.cos(phi)).map(_.toFloat)}) ++
          pointAngles.flatMap({ case (phi, theta) =>

            def sphereCoord: (Double, Double) => Point = {case (p, t) =>
              Point(Vector(scala.math.sin(p) * scala.math.cos(t), scala.math.sin(p) * scala.math.sin(t), scala.math.cos(p)))
            }

            val basePoint: Point = sphereCoord(phi, theta)

            val simplexVertices = Vector(sphereCoord(phi + 0.025, theta), sphereCoord(phi - 0.025, theta - 0.05), sphereCoord(phi - 0.025, theta + 0.05))

            val (dist, normalGuess): (Point => Double, Point) = LinearUtil.getSignedDistAndNormalToHyperplane(simplexVertices)

            val normal: Point = if (dist(Point(Vector(0.0, 0.0, 0.0))) <= 0.0) normalGuess else normalGuess * -1.0

            val dp: Double = normal.zip(basePoint).map({case (a, b) => a * b}).sum

            if (dp < 0.9) {
              println("")
              println("Uh oh! Our normal vector is fucked!")
              println("Dot product: " + dp)
              println("Calculated normal vector: (" + normal.map(_.toString).reduce(_ + ", " + _) + ")")
              println("Points on the plane: " + simplexVertices.map((p: Point) => "(" + p.map(_.toString).reduce(_ + ", " + _) + ")").reduce(_ + " | " + _))
            }


            (0 until numPerNormal).map((n: Int) => basePoint.zip(normal).map({case (a, b) => (a + b * (n.toDouble / numPerNormal.toDouble) * normalLength).toFloat})) ++ simplexVertices.map(_.map(_.toFloat))

          })
        val floatColorInfo: Vector[Vector[Float]] = pointAngles.map(_ => Vector(0.0f, 0.0f, 1.0f)) ++
          pointAngles.flatMap(_ => (0 until numPerNormal - 1).map(_ => Vector(0.0f, 1.0f, 0.0f)).toVector ++ Vector(Vector(1.0f, 1.0f, 1.0f)) ++ Vector(Vector(1.0f, 0.0f, 0.0f), Vector(1.0f, 0.0f, 0.0f), Vector(1.0f, 0.0f, 0.0f)))

        new HighDimViewer((0,0), (10, 10), floatVertexInfo, floatColorInfo)


      case _ => new HighDimViewer((0,0), (10,10), iris._1, iris._2)
    }
  }







  private var i: Int = 0
  private var j: Int = 1
  private var k: Int = 2
  private lazy val viewPortSize: Int = primaryMode.height() / numToFitPerDim
  private lazy val baseX: Int = (primaryMode.width() - primaryMode.height()) / 2
  private lazy val baseY: Int = primaryMode.height()
  private lazy val mod: Int = (viewPortSize * 0.9).toInt
  private lazy val indexLists: Vector[(Int, Int, Int)] = hdv.getUnorderedIndexTriples
  private lazy val numToFit: Double = indexLists.length.toDouble
  private lazy val numToFitPerDim: Int = math.sqrt(numToFit).ceil.toInt

  def run(): Unit = {
    init()
    loop()
    cleanupGLFW()
  }

  private def init(): Unit = {

    initializeGLFWAndWindow()

    initializeCallbacks()

    def initializeGLFWAndWindow(): Unit = {
      // Setup an error callback. The default implementation
      // will print the error message in System.err.
      GLFWErrorCallback.createPrint(System.err).set

      // Initialize GLFW. Most GLFW functions will not work before doing this.
      if (!glfwInit) throw new IllegalStateException("Unable to initialize GLFW")

      // Configure GLFW
      glfwDefaultWindowHints() // optional, the current window hints are already the default
      glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE) // the window will stay hidden after creation
      glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE) // the window will be resizable
      glfwWindowHint(GLFW_SAMPLES, 4) // 4 anti-aliasing samples


//      primaryMode = glfwGetVideoMode(glfwGetPrimaryMonitor())
      glfwWindowHint(GLFW_RED_BITS, primaryMode.redBits())
      glfwWindowHint(GLFW_GREEN_BITS, primaryMode.greenBits())
      glfwWindowHint(GLFW_BLUE_BITS, primaryMode.blueBits())
      glfwWindowHint(GLFW_REFRESH_RATE, primaryMode.refreshRate())

      // Create the window
      window = glfwCreateWindow(primaryMode.width(), primaryMode.height(), "Simp", glfwGetPrimaryMonitor(), NULL)
      if (window == NULL) throw new RuntimeException("Failed to create the GLFW window")


      // Get the thread stack and push a new frame
      {
        val stack = stackPush
        try {
          val pWidth = stack.mallocInt(1) // int*
          val pHeight = stack.mallocInt(1)
          // Get the window size passed to glfwCreateWindow
          glfwGetWindowSize(window, pWidth, pHeight)
          // Get the resolution of the primary monitor
          val videoMode = glfwGetVideoMode(glfwGetPrimaryMonitor)
          // Center the window
          glfwSetWindowPos(window, 1920 + (videoMode.width - pWidth.get(0)) / 2, (videoMode.height - pHeight.get(0)) / 2)
//          System.out.println(videoMode.width)
//          System.out.println(pWidth.get(0))
        } finally if (stack != null) stack.close()
      } // the stack frame is popped automatically

      // Make the OpenGL context current
      glfwMakeContextCurrent(window)
      // This line is critical for LWJGL's interoperation with GLFW's
      // OpenGL context, or any context that is managed externally.
      // LWJGL detects the context that is current in the current thread,
      // creates the GLCapabilities instance and makes the OpenGL
      // bindings available for use.
      GL.createCapabilities
      glEnable(GL13.GL_MULTISAMPLE)
      // Enable v-sync
      glfwSwapInterval(1)
      // Make the window visible
      glfwShowWindow(window)
    }

    def initializeCallbacks(): Unit = {

      def captureMouse(): Unit = {
        glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED)
      }

      def releaseMouse(): Unit = {
        lastX = 0.0
        lastY = 0.0
        firstMouse = true
        glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
      }

      // Close the window if ESC is pressed and cursor is enabled. If the cursor is disabled, then enable it.
      glfwSetKeyCallback(window, (window: Long, key: Int, _: Int, action: Int, _: Int) => {
        (key, action) match {
          case (GLFW_KEY_ESCAPE, GLFW_RELEASE) => glfwGetInputMode(window, GLFW_CURSOR) match {
            case GLFW_CURSOR_DISABLED => releaseMouse()
            case _ => glfwSetWindowShouldClose(window, true)
          }
          case _ => // Unhandled
        }
      })

      glfwSetMouseButtonCallback(window, (_, button, action, _) => (button, action) match {
        case (GLFW_MOUSE_BUTTON_LEFT, GLFW_PRESS) => glfwGetInputMode(window, GLFW_CURSOR) match {
            case GLFW_CURSOR_NORMAL =>
              val xPosition: DoubleBuffer = BufferUtils.createDoubleBuffer(1)
              val yPosition: DoubleBuffer = BufferUtils.createDoubleBuffer(1)
              glfwGetCursorPos(window, xPosition, yPosition)
              val viewportXIndex: Int = ((xPosition.get() - baseX.toDouble) / viewPortSize.toDouble).toInt
              val viewportYIndex: Int = numToFitPerDim - 1 - ((baseY.toDouble - yPosition.get()) / viewPortSize.toDouble).toInt

//              println(viewportXIndex)
//              println(viewportYIndex)
              val flatIndex = numToFitPerDim * viewportYIndex + viewportXIndex
              val newInd: (Int, Int, Int) = indexLists(flatIndex)
              i = newInd._1
              j = newInd._2
              k = newInd._3

              captureMouse()
            case GLFW_CURSOR_DISABLED => releaseMouse()
            case _ => // Ignore...
          }
        case _ => // Ignore...
      })


    }

  }

  private def loop(): Unit = {


    val vertices = Array(
      // Positions        // Colors
       0.5f,  0.5f, 0.0f, 1.0f, 0.0f, 0.0f, // top right
       0.5f, -0.5f, 0.0f, 1.0f, 1.0f, 1.0f, // bottom right
      -0.5f, -0.5f, 0.0f, 0.0f, 1.0f, 0.0f, // bottom left
      -0.5f,  0.5f, 0.0f, 0.0f, 0.0f, 1.0f // top left
    )
    val indices = Array(
      0, 1, 3, // first triangle
      1, 2, 3 // second triangle)
    )

//    val iris = ScalaComp.Main.getIrisData
//
////    hdv = new HighDimViewer((0,0), (10,10),
////      Vector(Vector(0.0f, 0.0f, 3.0f, -1.0f), Vector(0.0f, 0.0f, 2.5f, 0.0f), Vector(1.0f, 1.0f, 1.0f, 1.0f)),
////
////      Vector(Vector(1.0f,0.0f,0.0f), Vector(0.0f,0.0f,1.0f), Vector(0.0f,1.0f,0.0f)))
//    hdv = new HighDimViewer((0,0), (10,10), iris._1, iris._2)

    glfwSetCursorPosCallback(window, (_, xPos, yPos) => {

      if (glfwGetInputMode(window, GLFW_CURSOR) == GLFW_CURSOR_DISABLED) {
        if (firstMouse) {
          lastX = xPos
          lastY = yPos
          firstMouse = false
        }

        val xOffset: Double = xPos - lastX
        val yOffset: Double = -(yPos - lastY)

        lastX = xPos
        lastY = yPos

        hdv.processMouse(i, j, k, xOffset.toFloat, yOffset.toFloat)
      }
    })

//    val center: Array[Float] = Array(0.0f, 0.0f, 0.0f)
//    val cubeRadius: Float = 0.5f
//    // Use p for a positive shift, n for a negative shift, in dimension given by position
//    val nnn: Array[Float] = Array(center(0) - cubeRadius, center(1) - cubeRadius, center(2) - cubeRadius)
//    val nnp: Array[Float] = Array(center(0) - cubeRadius, center(1) - cubeRadius, center(2) + cubeRadius)
//    val npn: Array[Float] = Array(center(0) - cubeRadius, center(1) + cubeRadius, center(2) - cubeRadius)
//    val npp: Array[Float] = Array(center(0) - cubeRadius, center(1) + cubeRadius, center(2) + cubeRadius)
//    val pnn: Array[Float] = Array(center(0) + cubeRadius, center(1) - cubeRadius, center(2) - cubeRadius)
//    val pnp: Array[Float] = Array(center(0) + cubeRadius, center(1) - cubeRadius, center(2) + cubeRadius)
//    val ppn: Array[Float] = Array(center(0) + cubeRadius, center(1) + cubeRadius, center(2) - cubeRadius)
//    val ppp: Array[Float] = Array(center(0) + cubeRadius, center(1) + cubeRadius, center(2) + cubeRadius)
//
//    val vertices: Array[Float] = Seq(nnn, nnp, npn, npp, pnn, pnp, ppn, ppp).map(_ ++ Array(1.0f, 0.0f, 0.0f)).reduce(_ ++ _)
//    // Indices for the six sides
//    val frontIndices: Array[Int] = Array(4, 5, 6, 5, 6, 7)
//    val backIndices: Array[Int] = Array(0, 1, 2, 1, 2, 3)
//    val leftIndices: Array[Int] = Array(0, 1, 4, 1, 4, 5)
//    val rightIndices: Array[Int] = Array(2, 3, 6, 3, 6, 7)
//    val topIndices: Array[Int] = Array(1, 3, 5, 3, 5, 7)
//    val bottomIndices: Array[Int] = Array(0, 2, 4, 2, 4, 6)
//    val indices = (frontIndices ++ backIndices ++ leftIndices ++ rightIndices ++ topIndices ++ bottomIndices)


    val FLOAT_SIZE: Int = 4
    val m: Mesh = new Mesh(vertices, Vector(GLAttribute(0, 3, 6 * FLOAT_SIZE, 0), GLAttribute(1, 3, 6 * FLOAT_SIZE, 3*FLOAT_SIZE)), indices,
      "Shaders/SH1/test.vs", "Shaders/SH1/test.fs")
    m.rotate(Math.toRadians(45.0).toFloat, 0.0f, 0.0f, 1.0f)



    glEnable(GL_SCISSOR_TEST)
    glEnable(GL_DEPTH_TEST)


    def processInput(): Unit = {
      // Keyboard Movement
      val keyBindings: Seq[(Int, CameraMovement)] = Seq(
        (GLFW_KEY_W, Forward),
        (GLFW_KEY_S, Backward),
        (GLFW_KEY_D, Right),
        (GLFW_KEY_A, Left),
      )

      keyBindings.foreach(binding => {
        val keyState = glfwGetKey(window, binding._1)
        if (keyState == GLFW_PRESS) {
          hdv.processKeyboard(i, j, k, binding._2, deltaTime)
        }
      })

    }
    hdv.getUnorderedIndexTriples foreach println

    // Run the rendering loop until the user has attempted to close
    // the window or has pressed the ESCAPE key.
    while (!glfwWindowShouldClose(window)) {

      val currentFrame: Float = glfwGetTime().toFloat
      deltaTime = currentFrame - lastFrame
      lastFrame = currentFrame

      // "Restrict" to entire screen
      glViewport(0, 0, primaryMode.width(), primaryMode.height())
      glScissor(0, 0, primaryMode.width(), primaryMode.height())
      glClearColor(0.1f, 0.1f, 0.15f, 1.0f)
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

//      // Restrict to LEFT viewport
//      val w = primaryMode.height() * 2 / 3
//      glViewport(0, (primaryMode.height() - w) / 2, w, w)
//      glScissor(0, (primaryMode.height() - w) / 2, w, w)
//      glClearColor(0.3f, 0.3f, 0.3f, 1.0f)
//      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
//
//      // Calculate a time dependent green value
//      val timeValue: Double = glfwGetTime
//      val greenValue: Float = (sin(timeValue).toFloat / 2.0f) + 0.5f
//
//      // Calculate the perspective-view-model matrix
//      pvMatrix.identity
//      pvMatrix.perspective(Math.toRadians(45.0f).toFloat, 1.0f, 0.1f, 100.0f)
//      pvMatrix.mul(hdv.getViewMatrix(i, j, k))
//
//      m.rotate(deltaTime, 0.0f, sin(timeValue).toFloat, 1.0f)
//
//      m.setUniform4f("uniformColor", 0.0f, greenValue, 0.0f, 1.0f)
//      m.setUniformMatrix4fv("pvMatrix", pvMatrix, pvmFloatBuffer)
//
//      m.draw()
//
//      hdv.meshes(i, j, k).setUniform4f("uniformColor", 0.0f, greenValue, 0.0f, 1.0f)
//      hdv.meshes(i, j, k).setUniformMatrix4fv("pvMatrix", pvMatrix, pvmFloatBuffer)
//      hdv.meshes(i, j, k).draw()



//      indexLists = hdv.getUnorderedIndexTriples
//      numToFit = indexLists.length.toDouble
//      numToFitPerDim = math.sqrt(numToFit).ceil.toInt

//      viewPortSize = (primaryMode.height()) / numToFitPerDim
//      baseX = (primaryMode.width() - primaryMode.height()) / 2
//      baseY = primaryMode.height()
//      mod = (viewPortSize * 0.9).toInt

      indexLists.zipWithIndex.foreach {case ((xi, yi, zi), index) =>


        val viewportXIndex: Int = index % numToFitPerDim
        val viewportYIndex: Int = index / numToFitPerDim + 1

        val viewPortX: Int = baseX + viewportXIndex * viewPortSize
        val viewPortY: Int = baseY - viewportYIndex * viewPortSize



        // Restrict to RIGHT viewport
        glViewport(viewPortX, viewPortY, mod, mod)
        glScissor(viewPortX, viewPortY, mod, mod)
        glClearColor(0.3f, 0.3f, 0.3f, 1.0f)
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

        // Calculate the perspective-view-model matrix
        pvMatrix.identity
        pvMatrix.perspective(Math.toRadians(45.0f).toFloat, 1.0f, 0.1f, 100.0f)
        pvMatrix.mul(hdv.getViewMatrix(xi, yi, zi))

        hdv.meshes(xi, yi, zi).setUniform4f("uniformColor", 0.0f, 0.0f, 0.0f, 1.0f)
        hdv.meshes(xi, yi, zi).setUniformMatrix4fv("pvMatrix", pvMatrix, pvmFloatBuffer)
        hdv.meshes(xi, yi, zi).draw()
      }

      glfwSwapBuffers(window) // swap the color buffers

      // Poll for window events. The key callback above will only be
      // invoked during this call.
      glfwPollEvents()

      if (glfwGetInputMode(window, GLFW_CURSOR) == GLFW_CURSOR_DISABLED) processInput()
    }
  }

  private def cleanupGLFW(): Unit = {
    // Free the window callbacks and destroy the window
    glfwFreeCallbacks(window)
    glfwDestroyWindow(window)
    // Terminate GLFW and free the error callback
    glfwTerminate()
    glfwSetErrorCallback(null).free()
  }


  run()
}
