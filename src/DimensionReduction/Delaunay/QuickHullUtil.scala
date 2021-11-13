package DimensionReduction.Delaunay

import DimensionReduction.Delaunay.QuickHullUtil.DebugPrinter

import scala.math.{cos, random, sin}
import scala.util.Random

/** Provides functions that implement the QuickHull algorithm using a randomized incremental approach. */
object QuickHullUtil {

  /** Handles debug printing. */
  object DebugPrinter {

    // By default, don't print anything
    var shouldPrint: Boolean = false

    def print(s: Any): Unit = if (shouldPrint) println(s)
  }

  /** Computes the convex hull of the supplied points using a randomized incremental approach.
   *
   *  @see https://www.youtube.com/watch?v=tBE7PfKupZo
   *  @param points The collection of points whose convex hull is to be computed.
   *  @return The convex hull represented as a collection of simplices.
   */
  def getConvexHull(points: Vector[Vector[Double]]): Vector[Simplex] = {

    // Note: This function is referentially transparent despite the use of random initialization and imperative components
    // Emphasize: This code does not have side effects

    // Make sure there are actually enough points to get a nondegenerate hull
    assert(points.nonEmpty)
    // Make sure the points all have the same dimension
    assert(points.map(_.length).distinct.length == 1)

    // We'll refer to the dimension of our points as "d" in the following comments
    val dimension = points.head.length

    // Todo: Make this nonrandom
    // startingVertices will contain a collection of points that define a hyperplane of codimension 1
    val startingVertices: Vector[Vector[Double]] = {
      var vertexCandidates: Vector[Vector[Double]] = Vector()
      do {
        vertexCandidates = Random.shuffle(points).take(dimension)
      } while (!LinearUtil.doPointsDefineAHyperplaneOfCodimensionOne(vertexCandidates))
      vertexCandidates
    }

    // Store the distance function for that hyperplane
    val distToStartingVertices: Vector[Double] => Double = LinearUtil.getSignedDistAndNormalToHyperplane(startingVertices)._1

    // Get a the farthest (absolute) point from that hyperplane
    val vertexOfMaximalAbsoluteDistance: Vector[Double] = points.maxBy((v: Vector[Double]) => scala.math.abs(distToStartingVertices(v)))

//    DebugPrinter.print("Starting vertices:")
//    DebugPrinter.print(startingVertices)
//    DebugPrinter.print("Vertices and their distances to the starting vertices:")
//    points.foreach((v: Vector[Double]) => DebugPrinter.print(v.toString() + " has dist " + scala.math.abs(distToStartingVertices(v))))

    // Our starting vertices and this far extra vertex will define the d+1 simplices of our initial convex hull.
    val initialHullVertices: Vector[Vector[Double]] = startingVertices ++ Vector(vertexOfMaximalAbsoluteDistance)


    // Todo: Functionalize

    // Stores the simplices that need to be processed. I.e., simplices that might have points above them.
    var simplicesToProcess: Vector[Simplex] = Vector()

    // Initialize the d+1 simplices that make up our initial convex hull guess
    for (pointToExclude <- initialHullVertices) {
      val pointsOfThisSimplex: Vector[Vector[Double]] = initialHullVertices.filter(_ != pointToExclude)


//      DebugPrinter.print("Point to exclude:")
//      DebugPrinter.print(pointToExclude)
//      DebugPrinter.print("Points of this simplex:")
//      DebugPrinter.print(pointsOfThisSimplex)

      val (distGuess, normal): (Vector[Double] => Double, Vector[Double]) = LinearUtil.getSignedDistAndNormalToHyperplane(pointsOfThisSimplex)

      // We want to ensure that the points "above" this simplex are on the opposite side of the simplex from the excluded point.
      // We also want to recognize points "above" this simplex as points with positive signed distance from the simplex.
      // Hence, the excluded point should have negative signed distance. If it has positive signed distance, then we need to switch.
      val shouldNegateDistanceFunc: Boolean = distGuess(pointToExclude) > 0

      val thisSimplex: Simplex = Simplex(
        pointsOfThisSimplex,
        if (shouldNegateDistanceFunc)
          (v: Vector[Double]) => -distGuess(v)
        else
          distGuess,
        if (shouldNegateDistanceFunc)
          normal.map(_ * -1.0)
        else
          normal
      )

      simplicesToProcess = simplicesToProcess.appended(thisSimplex)
    }

    val verticesToProcess: Vector[Vector[Double]] = Random.shuffle(points.diff(simplicesToProcess.flatMap(_.vertices).distinct))

    object ConflictGraph {
//      private var vertices: Vector[Vector[Double]] = verticesToProcess
      private var simplices: Vector[Simplex] = simplicesToProcess
      private var edges: Map[Vector[Double], Vector[Simplex]] = (for (
        v <- verticesToProcess;
        s <- simplices if s.signedDistance(v) >= 0.0
      ) yield (v, s)).groupBy(_._1).map({case (v, p) => (v, p.map(_._2))})

      def pointIsInConvexHull(p: Vector[Double]): Boolean = {
        (!edges.contains(p)) || edges(p).isEmpty
      }

      def getConflictingSimplices(p: Vector[Double]): Vector[Simplex] = {
        if (!edges.contains(p))
          Vector()
        else
          edges(p)
      }

      def getConflictingVertices(s: Simplex): Vector[Vector[Double]] = {
//        edges.filter(_._2.contains(s)).keys.toVector
        edges.keys.filter(s.signedDistance(_) >= 0.0).toVector
      }

      def generateConflictingSimplices(v: Vector[Double]): Vector[Simplex] = {
        simplices.filter((s: Simplex) => s.signedDistance(v) >= 0.0)
      }

      def generateConflictingVertices(s: Simplex): Vector[Vector[Double]] = {
        edges.keys.filter((v: Vector[Double]) => s.signedDistance(v) >= 0.0).toVector
      }

//      def removeVertex(v: Vector[Double]): Unit = {
//        vertices = vertices.filter(_ != v)
//      }

//      def deleteConflicts(p: Vector[Double]): Unit = {
//        if (edges.contains(p)) {
//          simplices = simplices.diff(edges(p))
//          edges = edges.updated(p, Vector())
//        }
//      }

      def deleteSimplices(simplicesToRemove: Vector[Simplex]): Unit = {
        simplices = simplices.diff(simplicesToRemove)
        edges = edges.map({case (v, simps) => (v, simps.diff(simplicesToRemove))}).filter(_._2.nonEmpty)
      }

      def addConflict(p: Vector[Double], s: Simplex): Unit = {
        if (edges.contains(p))
          edges = edges.updated(p, edges(p) ++ Vector(s))
        else
          edges = edges.updated(p, Vector(s))
      }

      def getRidges(visibleSimplices: Vector[Simplex]): Vector[(Simplex, Simplex)] = {
        (for (
          visibleSimplex <- visibleSimplices;
          invisibleSimplex <- simplices.diff(visibleSimplices) if {
//            DebugPrinter.print("Intersection size: " + visibleSimplex.vertices.intersect(invisibleSimplex.vertices).length)
            visibleSimplex.vertices.intersect(invisibleSimplex.vertices).length == dimension - 1
          }
        ) yield (visibleSimplex, invisibleSimplex)).distinctBy(p => p._1.vertices.intersect(p._2.vertices).toSet)
      }

      def createFacet(face: Vector[Vector[Double]], p: Vector[Double]): Simplex = {
        val (distGuess, normal): (Vector[Double] => Double, Vector[Double]) = LinearUtil.getSignedDistAndNormalToHyperplane(face ++ Vector(p))

//        val shouldNegateDistance: Boolean = distGuess(pointBelow) >= 0.0

        // The points in the convex hull should all be below.
        val extremalDistance: Double = simplices.flatMap(_.vertices).map(distGuess).maxBy(scala.math.abs)
        val shouldNegateDistance: Boolean = extremalDistance >= 0.0

        val trueDistance: Vector[Double] => Double = if (shouldNegateDistance) (x: Vector[Double]) => -distGuess(x) else distGuess
        val trueNormal: Vector[Double] = if (shouldNegateDistance) normal.map(_ * -1.0) else normal

        val newFacet: Simplex = Simplex(face ++ Vector(p), trueDistance, trueNormal)

        newFacet
      }

      def processPoint(pointToProcess: Vector[Double]): Unit = {if (edges.contains(pointToProcess)) {

//        println("Convex hull currently contains " + simplices.length + " simplices")
//        println("There are still " + edges.keys.toVector.length + " vertices outside of the hull.")

        val conflicts: Vector[Simplex] = generateConflictingSimplices(pointToProcess)//edges(pointToProcess)
        val ridges: Vector[(Simplex, Simplex)] = getRidges(conflicts)

        ridges.foreach {case (visibleSimplex, invisibleSimplex) =>
          val intersectionPoints: Vector[Vector[Double]] = visibleSimplex.vertices.intersect(invisibleSimplex.vertices)

          val newFacet: Simplex = ConflictGraph.createFacet(intersectionPoints, pointToProcess)

          val candidateConflicts: Vector[Vector[Double]] = getConflictingVertices(visibleSimplex) ++ getConflictingVertices(invisibleSimplex)

          simplices = simplices.appended(newFacet)

          candidateConflicts.foreach((candidate: Vector[Double]) => {
            if (newFacet.signedDistance(candidate) >= 0.0)
              edges = edges.updated(candidate, edges(candidate) ++ Vector(newFacet))
          })

        }

        edges = edges.removed(pointToProcess)
        edges = edges.map({case (v, c) => (v, c.diff(conflicts))}).filter(_._2.nonEmpty)
        simplices = simplices.diff(conflicts)

      }}

      def getSimplices: Vector[Simplex] = simplices

      def print(): Unit = {

        def makeReadable(v: Vector[Double]): String = v match {
          case Vector(0.0, 0.0, 1.0) => "A"
          case Vector(0.0, 0.0, 0.0) => "B"
          case Vector(-1.0, -1.0, -1.0) => "C"
          case Vector(-1.0, 1.0, -1.0) => "D"
          case Vector(1.0, -1.0, -1.0) => "E"
          case Vector(1.0, 1.0, -1.0) => "F"
          case any: Vector[Double] => any.toString()
        }

        DebugPrinter.print("")
        DebugPrinter.print("Printing conflict graph ============================================================================================================================================================")
        DebugPrinter.print("")
        DebugPrinter.print("Vertices:")
        edges.keys foreach {v => DebugPrinter.print(makeReadable(v))}
        DebugPrinter.print("")
        DebugPrinter.print("Simplices:")
        simplices foreach {(simplex: Simplex) => DebugPrinter.print(simplex.vertices.map(makeReadable))}
        DebugPrinter.print("")
        DebugPrinter.print("Conflict graph:")
        edges foreach {case (v, conflicts) => DebugPrinter.print(makeReadable(v) + " : " + {if (conflicts.nonEmpty) conflicts.map(s => s.vertices.map(makeReadable).reduce(_ + ", " + _)).reduce(_ + " ~~~~ " + _) else ""})}
        DebugPrinter.print("")
        DebugPrinter.print("====================================================================================================================================================================================")
        DebugPrinter.print("")
      }

    }

    verticesToProcess.zipWithIndex.foreach({case (currentVertex: Vector[Double], index: Int) =>

      //      println("Working on vertex " + index + " out of " + verticesToProcess.length)

      ConflictGraph.processPoint(currentVertex)

    })

    DebugPrinter.print("Done processing vertices")

    ConflictGraph.print()

    ConflictGraph.getSimplices

  }

}

object Test extends App {

  val points: Vector[Vector[Double]] = Vector(
    Vector(0.0, 0.0, 0.0),
    Vector(0.0, 0.0, 1.0),
    Vector(-1.0, -1.0, -1.0),
    Vector(1.0, -1.0, -1.0),
    Vector(-1.0, 1.0, -1.0),
    Vector(1.0, 1.0, -1.0)
  )


  QuickHullUtil.DebugPrinter.shouldPrint = false


//  (0 until 1000) foreach {_ => {
//    val convexHull: Vector[Simplex] = QuickHullUtil.getConvexHull(points)
//
//    val convexHullPoints: Vector[Vector[Double]] = convexHull.flatMap(_.vertices).distinct
//    val innerPoints: Vector[Vector[Double]] = points.filter((p: Vector[Double]) => !convexHullPoints.contains(p))
//
////    println("Convex hull:")
////    convexHullPoints foreach {println}
////
////    println("\n\nInner points:")
////    innerPoints foreach {println}
////
////
////    println("\n\n\n")
//
//    assert(innerPoints == Vector(Vector(0.0, 0.0, 0.0)))
//  }}

  {
    val randomCirclePoints: Vector[Vector[Double]] = (0 until 3141).map(_ => {
      val angle: Double = random() * 2.0 * scala.math.Pi
      val radius: Double = {
        val v = random() + random()

        if (v >= 1.0) 2.0-v else v
      }

      Vector(radius * cos(angle), radius * sin(angle))
    }).toVector


    val convexHull: Vector[Simplex] = QuickHullUtil.getConvexHull(randomCirclePoints)

    val convexHullPoints2: Vector[Vector[Double]] = convexHull.flatMap(_.vertices).distinct
    val innerPoints2: Vector[Vector[Double]] = randomCirclePoints.filter((p: Vector[Double]) => !convexHullPoints2.contains(p))


    println("Convex hull points =================================================")
    convexHullPoints2 foreach {p => println(p.map(_.toString).reduce(_ + "," + _))}
    println("")
    println("")
    println("Interior points ====================================================")
    innerPoints2 foreach {p => println(p.map(_.toString).reduce(_ + "," + _))}
  }

  println("Done! :)")
}

object QuickHullTest extends App {

  DebugPrinter.shouldPrint = false

  val oneDimPoints: Vector[Vector[Double]] = (0 until 10).map(_ => Vector(random() * 2.0 - 1.0)).toVector
  val parabolicPoints = oneDimPoints.map(p => p ++ Vector(p.head * p.head))

  val hullSimplices: Vector[Simplex] = QuickHullUtil.getConvexHull(parabolicPoints)

  parabolicPoints.foreach(p =>
    println("(" + p.map(_.toString).reduce(_ + ", " + _) + ")")
  )

  hullSimplices.foreach((s: Simplex) => {
    val startPoint: Vector[Double] = s.vertices(0)
    val endPoint: Vector[Double] = s.vertices(1)

    val x0: Double = startPoint(0)
    val y0: Double = startPoint(1)
    val x1: Double = endPoint(0)
    val y1: Double = endPoint(1)
    val dx: Double = x1 - x0
    val dy: Double = y1 - y0

    val niceToString: Double => String = (d: Double) => f"$d%1.4f"

    println("(" + niceToString(x0) + " + (" + niceToString(dx) + " * t)" + ", " + niceToString(y0) + " + (" + niceToString(dy) + " * t" + "))")


    // Also print the normal vector....
    val nx0 = (x0 + x1) / 2.0
    val ny0 = (y0 + y1) / 2.0

    val displayLength: Double = 0.5
    val ndx = s.normalVector(0) * displayLength
    val ndy = s.normalVector(1) * displayLength

    println("(" + niceToString(nx0) + " + (" + niceToString(ndx) + " * t)" + ", " + niceToString(ny0) + " + (" + niceToString(ndy) + " * t" + "))")


  })

}



object QuickHullTest2 extends App {
  val points: Vector[Vector[Double]] = (1 to 100).map(_ => Vector(random(), random(), random())).toVector

  val convexHull: Vector[Simplex] = QuickHullUtil.getConvexHull(points)

  val convexHullVertices: Vector[Vector[Double]] = convexHull.flatMap(_.vertices).distinct
  val interiorVertices: Vector[Vector[Double]] = points.diff(convexHullVertices)


  val convexHullOfConvexHull: Vector[Simplex] = QuickHullUtil.getConvexHull(convexHullVertices)
  val ccVertices: Vector[Vector[Double]] = convexHullOfConvexHull.flatMap(_.vertices).distinct
  val cccVertices: Vector[Vector[Double]] = QuickHullUtil.getConvexHull(ccVertices).flatMap(_.vertices).distinct
  val ccccVertices: Vector[Vector[Double]] = QuickHullUtil.getConvexHull(cccVertices).flatMap(_.vertices).distinct
  val cccccVertices: Vector[Vector[Double]] = QuickHullUtil.getConvexHull(ccccVertices).flatMap(_.vertices).distinct

  println("Number of points in convex hull: " + convexHullVertices.length)
  println("Number of points in convex hull of convex hull: " + ccVertices.length)
  println("Number of CCC vertices..." + cccVertices.length)
  println("Number of CCCC vertices..." + ccccVertices.length)
  println("Number of CCCCC vertices..." + cccccVertices.length)


}

