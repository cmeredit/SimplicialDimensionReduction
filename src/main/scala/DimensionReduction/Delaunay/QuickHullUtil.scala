package DimensionReduction.Delaunay

import DimensionReduction.Delaunay.QuickHullUtil.DebugPrinter
import DimensionReduction.{LinearUtil, Point}

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
   *  @param degeneracyTolerance The tolerance with which we consider distances to be zero.
   *  @return The convex hull represented as a collection of simplices. If the convex hull is degenerate, return None. Otherwise, return Some(hull).
   */
  def getConvexHull(points: Vector[Point], degeneracyTolerance: Double = 0.000001): Option[Vector[PointedAffineSpace]] = {

    // Note: This function is referentially transparent despite the use of random initialization and imperative components
    // Emphasize: This code does not have side effects

    // Make sure there are actually enough points to get a nondegenerate hull
    assert(points.nonEmpty)
    // Make sure the points all have the same dimension
    assert(points.map(_.dimension).distinct.length == 1)

    // We'll refer to the dimension of our points as "d" in the following comments
    val dimension = points.head.dimension

    // Todo: Make this nonrandom
    // startingVertices will contain a collection of points that define a hyperplane of codimension 1
    val startingVertices: Vector[Point] = {
      var vertexCandidates: Vector[Point] = Vector()
      do {
        vertexCandidates = Random.shuffle(points).take(dimension)
      } while (!LinearUtil.doPointsDefineAHyperplaneOfCodimensionOne(vertexCandidates))
      vertexCandidates
    }

    // Store the distance function for that hyperplane
    val distToStartingVertices: Point => Double = LinearUtil.getSignedDistAndNormalToHyperplane(startingVertices)._1

    // Get a the farthest (absolute) point from that hyperplane
    val vertexOfMaximalAbsoluteDistance: Point = points.diff(startingVertices).maxBy((v: Point) => scala.math.abs(distToStartingVertices(v)))


//    println("Vertex of maximal absolute distance: " + vertexOfMaximalAbsoluteDistance.toString)
//    println("Distance: " + scala.math.abs(distToStartingVertices(vertexOfMaximalAbsoluteDistance)))

    if (scala.math.abs(distToStartingVertices(vertexOfMaximalAbsoluteDistance)) < degeneracyTolerance)
      None
    else {

      //    DebugPrinter.print("Starting vertices:")
      //    DebugPrinter.print(startingVertices)
      //    DebugPrinter.print("Vertices and their distances to the starting vertices:")
      //    points.foreach((v: Point) => DebugPrinter.print(v.toString() + " has dist " + scala.math.abs(distToStartingVertices(v))))

      // Our starting vertices and this far extra vertex will define the d+1 simplices of our initial convex hull.
      val initialHullVertices: Vector[Point] = startingVertices ++ Vector(vertexOfMaximalAbsoluteDistance)

//      println("Initial hull vertices")
//      println(initialHullVertices)

      // Todo: Functionalize

      // Stores the simplices that need to be processed. I.e., simplices that might have points above them.
      var simplicesToProcess: Vector[PointedAffineSpace] = Vector()

      // Initialize the d+1 simplices that make up our initial convex hull guess
      for (pointToExclude <- initialHullVertices) {
        val pointsOfThisSimplex: Vector[Point] = initialHullVertices.filter(_ != pointToExclude)


        //      DebugPrinter.print("Point to exclude:")
        //      DebugPrinter.print(pointToExclude)
        //      DebugPrinter.print("Points of this simplex:")
        //      DebugPrinter.print(pointsOfThisSimplex)

        val (distGuess, normal): (Point => Double, Point) = LinearUtil.getSignedDistAndNormalToHyperplane(pointsOfThisSimplex)

        // We want to ensure that the points "above" this simplex are on the opposite side of the simplex from the excluded point.
        // We also want to recognize points "above" this simplex as points with positive signed distance from the simplex.
        // Hence, the excluded point should have negative signed distance. If it has positive signed distance, then we need to switch.
        val shouldNegateDistanceFunc: Boolean = distGuess(pointToExclude) > 0

        val thisSimplex: PointedAffineSpace = PointedAffineSpace(
          pointsOfThisSimplex,
          if (shouldNegateDistanceFunc)
            (v: Point) => -distGuess(v)
          else
            distGuess,
          if (shouldNegateDistanceFunc)
            normal * -1.0
          else
            normal
        )

        simplicesToProcess = simplicesToProcess.appended(thisSimplex)
      }

//      println("Simplices to process")
//      simplicesToProcess foreach println

      val verticesToProcess: Vector[Point] = Random.shuffle(points.diff(simplicesToProcess.flatMap(_.vertices).distinct))
//      println("Vertices to process")

      object ConflictGraph {
        //      private var vertices: Vector[Point] = verticesToProcess
        private var simplices: Vector[PointedAffineSpace] = simplicesToProcess
        private var edges: Map[Point, Vector[PointedAffineSpace]] = (for (
          v <- verticesToProcess;
          s <- simplices if s.signedDistance(v) >= 0.0
        ) yield (v, s)).groupBy(_._1).map({ case (v, p) => (v, p.map(_._2)) })

        def pointIsInConvexHull(p: Point): Boolean = {
          (!edges.contains(p)) || edges(p).isEmpty
        }

        def getConflictingSimplices(p: Point): Vector[PointedAffineSpace] = {
          if (!edges.contains(p))
            Vector()
          else
            edges(p)
        }

        def getConflictingVertices(s: PointedAffineSpace): Vector[Point] = {
          //        edges.filter(_._2.contains(s)).keys.toVector
          edges.keys.filter(s.signedDistance(_) >= 0.0).toVector
        }

        def generateConflictingSimplices(v: Point): Vector[PointedAffineSpace] = {
          simplices.filter((s: PointedAffineSpace) => s.signedDistance(v) >= 0.0)
        }

        def generateConflictingVertices(s: PointedAffineSpace): Vector[Point] = {
          edges.keys.filter((v: Point) => s.signedDistance(v) >= 0.0).toVector
        }

        //      def removeVertex(v: Point): Unit = {
        //        vertices = vertices.filter(_ != v)
        //      }

        //      def deleteConflicts(p: Point): Unit = {
        //        if (edges.contains(p)) {
        //          simplices = simplices.diff(edges(p))
        //          edges = edges.updated(p, Vector())
        //        }
        //      }

        def deleteSimplices(simplicesToRemove: Vector[PointedAffineSpace]): Unit = {
          simplices = simplices.diff(simplicesToRemove)
          edges = edges.map({ case (v, simps) => (v, simps.diff(simplicesToRemove)) }).filter(_._2.nonEmpty)
        }

        def addConflict(p: Point, s: PointedAffineSpace): Unit = {
          if (edges.contains(p))
            edges = edges.updated(p, edges(p) ++ Vector(s))
          else
            edges = edges.updated(p, Vector(s))
        }

        def getRidges(visibleSimplices: Vector[PointedAffineSpace]): Vector[(PointedAffineSpace, PointedAffineSpace)] = {
          (for (
            visibleSimplex <- visibleSimplices;
            invisibleSimplex <- simplices.diff(visibleSimplices) if {
              //            DebugPrinter.print("Intersection size: " + visibleSimplex.vertices.intersect(invisibleSimplex.vertices).length)
              visibleSimplex.vertices.intersect(invisibleSimplex.vertices).length == dimension - 1
            }
          ) yield (visibleSimplex, invisibleSimplex)).distinctBy(p => p._1.vertices.intersect(p._2.vertices).toSet)
        }

        def createFacet(face: Vector[Point], p: Point): PointedAffineSpace = {
          val (distGuess, normal): (Point => Double, Point) = LinearUtil.getSignedDistAndNormalToHyperplane(face ++ Vector(p))

          //        val shouldNegateDistance: Boolean = distGuess(pointBelow) >= 0.0

          // The points in the convex hull should all be below.
          val extremalDistance: Double = simplices.flatMap(_.vertices).map(distGuess).maxBy(scala.math.abs)
          val shouldNegateDistance: Boolean = extremalDistance >= 0.0

          val trueDistance: Point => Double = if (shouldNegateDistance) (x: Point) => -distGuess(x) else distGuess
          val trueNormal: Point = if (shouldNegateDistance) normal * -1.0 else normal

          val newFacet: PointedAffineSpace = PointedAffineSpace(face ++ Vector(p), trueDistance, trueNormal)

          newFacet
        }

        def processPoint(pointToProcess: Point): Unit = {
          if (edges.contains(pointToProcess)) {

            //        println("Convex hull currently contains " + simplices.length + " simplices")
            //        println("There are still " + edges.keys.toVector.length + " vertices outside of the hull.")

            val conflicts: Vector[PointedAffineSpace] = generateConflictingSimplices(pointToProcess) //edges(pointToProcess)
            val ridges: Vector[(PointedAffineSpace, PointedAffineSpace)] = getRidges(conflicts)

            ridges.foreach { case (visibleSimplex, invisibleSimplex) =>
              val intersectionPoints: Vector[Point] = visibleSimplex.vertices.intersect(invisibleSimplex.vertices)

              val newFacet: PointedAffineSpace = ConflictGraph.createFacet(intersectionPoints, pointToProcess)

              val candidateConflicts: Vector[Point] = getConflictingVertices(visibleSimplex) ++ getConflictingVertices(invisibleSimplex)

              simplices = simplices.appended(newFacet)

              candidateConflicts.foreach((candidate: Point) => {
                if (newFacet.signedDistance(candidate) >= 0.0)
                  edges = edges.updated(candidate, edges(candidate) ++ Vector(newFacet))
              })

            }

            edges = edges.removed(pointToProcess)
            edges = edges.map({ case (v, c) => (v, c.diff(conflicts)) }).filter(_._2.nonEmpty)
            simplices = simplices.diff(conflicts)

          }
        }

        def getSimplices: Vector[PointedAffineSpace] = simplices

        def print(): Unit = {

          def makeReadable(v: Point): String = v match {
            case Point(Vector(0.0, 0.0, 1.0)) => "A"
            case Point(Vector(0.0, 0.0, 0.0)) => "B"
            case Point(Vector(-1.0, -1.0, -1.0)) => "C"
            case Point(Vector(-1.0, 1.0, -1.0)) => "D"
            case Point(Vector(1.0, -1.0, -1.0)) => "E"
            case Point(Vector(1.0, 1.0, -1.0)) => "F"
            case any: Point => any.toString()
          }

          DebugPrinter.print("")
          DebugPrinter.print("Printing conflict graph ============================================================================================================================================================")
          DebugPrinter.print("")
          DebugPrinter.print("Vertices:")
          edges.keys foreach { v => DebugPrinter.print(makeReadable(v)) }
          DebugPrinter.print("")
          DebugPrinter.print("Simplices:")
          simplices foreach { (simplex: PointedAffineSpace) => DebugPrinter.print(simplex.vertices.map(makeReadable)) }
          DebugPrinter.print("")
          DebugPrinter.print("Conflict graph:")
          edges foreach { case (v, conflicts) => DebugPrinter.print(makeReadable(v) + " : " + {
            if (conflicts.nonEmpty) conflicts.map(s => s.vertices.map(makeReadable).reduce(_ + ", " + _)).reduce(_ + " ~~~~ " + _) else ""
          })
          }
          DebugPrinter.print("")
          DebugPrinter.print("====================================================================================================================================================================================")
          DebugPrinter.print("")
        }

      }

      verticesToProcess.zipWithIndex.foreach({ case (currentVertex: Point, index: Int) =>

        println("Working on vertex " + index + " out of " + verticesToProcess.length)
        println("Working on " + currentVertex)

        ConflictGraph.processPoint(currentVertex)

      })

      DebugPrinter.print("Done processing vertices")

      ConflictGraph.print()

      Some(ConflictGraph.getSimplices)
    }
  }

}

object Test extends App {

  val points: Vector[Point] = Vector(
    Point(Vector( 0.0,  0.0,  0.0)),
    Point(Vector( 0.0,  0.0,  1.0)),
    Point(Vector(-1.0, -1.0, -1.0)),
    Point(Vector( 1.0, -1.0, -1.0)),
    Point(Vector(-1.0,  1.0, -1.0)),
    Point(Vector( 1.0,  1.0, -1.0))
  )


  QuickHullUtil.DebugPrinter.shouldPrint = false


//  (0 until 1000) foreach {_ => {
//    val convexHull: Vector[Simplex] = QuickHullUtil.getConvexHull(points)
//
//    val convexHullPoints: Vector[Point] = convexHull.flatMap(_.vertices).distinct
//    val innerPoints: Vector[Point] = points.filter((p: Point) => !convexHullPoints.contains(p))
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
    val randomCirclePoints: Vector[Point] = (0 until 3141).map(_ => {
      val angle: Double = random() * 2.0 * scala.math.Pi
      val radius: Double = {
        val v = random() + random()

        if (v >= 1.0) 2.0-v else v
      }

      Point(Vector(radius * cos(angle), radius * sin(angle)))
    }).toVector


    val convexHull: Vector[PointedAffineSpace] = QuickHullUtil.getConvexHull(randomCirclePoints) match {
      case Some(h) => h
      case None => Vector()
    }

    val convexHullPoints2: Vector[Point] = convexHull.flatMap(_.vertices).distinct
    val innerPoints2: Vector[Point] = randomCirclePoints.filter((p: Point) => !convexHullPoints2.contains(p))


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

  val oneDimPoints: Vector[Point] = (0 until 10).map(_ => Vector(random() * 2.0 - 1.0)).toVector.map(Point)
  val parabolicPoints = oneDimPoints.map(p => Point(p.coordinates ++ Vector(p.head * p.head)))

  val hullSimplices: Vector[PointedAffineSpace] = QuickHullUtil.getConvexHull(parabolicPoints)match {
    case Some(h) => h
    case None => Vector()
  }

  parabolicPoints.foreach(p =>
    println("(" + p.map(_.toString).reduce(_ + ", " + _) + ")")
  )

  hullSimplices.foreach((s: PointedAffineSpace) => {
    val startPoint: Point = s.vertices(0)
    val endPoint: Point = s.vertices(1)

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

