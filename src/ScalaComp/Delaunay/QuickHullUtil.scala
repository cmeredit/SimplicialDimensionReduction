package ScalaComp.Delaunay

import ScalaComp.Delaunay.QuickHullUtil.DebugPrinter
import ScalaComp.LinearUtil

import scala.math.{cos, random, sin}
import scala.util.Random

object QuickHullUtil {


  object DebugPrinter {

    // By default, don't print anything
    var shouldPrint: Boolean = false

    def print(s: Any): Unit = if (shouldPrint) println(s)
  }

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
    val distToStartingVertices: Vector[Double] => Double = LinearUtil.getSignedDistanceFunctionToHyperplane(startingVertices)._1

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

      val (distGuess, normal): (Vector[Double] => Double, Vector[Double]) = LinearUtil.getSignedDistanceFunctionToHyperplane(pointsOfThisSimplex)

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
        val (distGuess, normal): (Vector[Double] => Double, Vector[Double]) = LinearUtil.getSignedDistanceFunctionToHyperplane(face ++ Vector(p))

//        val shouldNegateDistance: Boolean = distGuess(pointBelow) >= 0.0

        // The points in the convex hull should all be below.
        val extremalDistance: Double = simplices.flatMap(_.vertices).map(distGuess).maxBy(scala.math.abs)
        val shouldNegateDistance: Boolean = extremalDistance >= 0.0

        val trueDistance: Vector[Double] => Double = if (shouldNegateDistance) (x: Vector[Double]) => -distGuess(x) else distGuess
        val trueNormal: Vector[Double] = if (shouldNegateDistance) normal.map(_ * -1.0) else normal

        val newFacet: Simplex = Simplex(face ++ Vector(p), trueDistance, trueNormal)

        newFacet
      }

//      def addFacet(facet: Simplex, ridge: (Simplex, Simplex)): Unit = {
//
//        simplices = simplices ++ Vector(facet)
//
//        val candidateConflicts: Vector[Vector[Double]] = ConflictGraph.getConflictingVertices(ridge._1) ++ ConflictGraph.getConflictingVertices(ridge._2)
//
//        DebugPrinter.print("Adding facet: " + facet.vertices)
//        DebugPrinter.print("Based on ridge: " + ridge._1.vertices + " ^v^v^v^ " + ridge._2.vertices)
//
//        candidateConflicts.foreach((candidate: Vector[Double]) => {
//
//          DebugPrinter.print("Candidate conflict: " + candidate)
//          DebugPrinter.print("Candidate distance: " + facet.signedDistance(candidate))
//
//          // test if this candidate actually conflicts with the new facet
//          if (facet.signedDistance(candidate) >= 0.0) {
//            DebugPrinter.print("Candidate truly is a conflict")
//            ConflictGraph.addConflict(candidate, facet)
//          } else {
//            DebugPrinter.print("Rejected candidate")
//          }
//
//        })
//      }





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
    {

//      println("Working on vertex " + index + " out of " + verticesToProcess.length)

      ConflictGraph.processPoint(currentVertex)

//      DebugPrinter.print("Checking if we need to process this point.")
//      val needToProcess = !ConflictGraph.pointIsInConvexHull(currentVertex)
//
//      if (needToProcess) DebugPrinter.print("We do need to process this point.") else DebugPrinter.print("Skipping this point")
//
//      if (needToProcess) {
//
//
//        DebugPrinter.print("Going to process " + currentVertex.toString())
//        DebugPrinter.print("Conflict graph at beginning of stage")
//        ConflictGraph.print()
//
//        DebugPrinter.print("")
//        DebugPrinter.print("Finding conflicts")
//        val conflicts: Vector[Simplex] = ConflictGraph.getConflictingSimplices(currentVertex)
////        ConflictGraph.deleteConflicts(currentVertex)
//        ConflictGraph.deleteSimplices(conflicts)
//
//        DebugPrinter.print("Finding ridges")
//        val ridges: Vector[(Simplex, Simplex)] = ConflictGraph.getRidges(conflicts)
//
//        DebugPrinter.print("Number of ridges:")
//        DebugPrinter.print(ridges.length)
//        DebugPrinter.print("Found " + ridges.length + " ridges")
//
////        DebugPrinter.print("Finding horizon points")
////        val allHorizonPoints: Vector[Vector[Double]] = ridges.flatMap({case (s1, s2) => s1.vertices.intersect(s2.vertices)})
//
//        DebugPrinter.print("Adding new facets based on ridges")
//        ridges.foreach {case (visibleSimplex, invisibleSimplex) =>
//          val intersectionPoints: Vector[Vector[Double]] = visibleSimplex.vertices.intersect(invisibleSimplex.vertices)
//
//          val newFacet: Simplex = ConflictGraph.createFacet(intersectionPoints, currentVertex)
//
//          ConflictGraph.addFacet(newFacet, (visibleSimplex, invisibleSimplex))
//        }
//
//        DebugPrinter.print("Conflict graph at end of stage:")
//        ConflictGraph.print()
//
//      }
//
////      ConflictGraph.removeVertex(currentVertex)
    }


    })

    DebugPrinter.print("Done processing vertices")

    ConflictGraph.print()

//    // If a point is below all of our hull simplices, then it can't be a vertex of the hull. So only keep the points that are above one of the simplices
//    // we still need to process.
//    // Below, we'll be a bit more smart about this update step. However, we know we need to compare against *all* simplices to process since we've just
//    // now initialized them. In the future, we only need to check against new simplices.
//    undeterminedPoints = points.filter((point: Vector[Double]) => simplicesToProcess.exists((simplex: Simplex) => simplex.signedDistance(point) > 0.0))
//
//
//    while (simplicesToProcess.nonEmpty && undeterminedPoints.nonEmpty) {
//
//      DebugPrinter.print("NEXT STAGE ========================================================================================================================================")
//      DebugPrinter.print("Current hull:")
//      hullSimplices foreach {simplex => DebugPrinter.print(simplex.vertices)}
//
//      DebugPrinter.print("Undetermined points")
//      undeterminedPoints foreach DebugPrinter.print
//
//      DebugPrinter.print("Simplices to process:")
//      simplicesToProcess foreach {simplex => DebugPrinter.print(simplex.vertices)}
//
//
//      // Pop off a simplex
//      val nextSimplex: Simplex = simplicesToProcess.head
//      simplicesToProcess = simplicesToProcess.tail
//
//      val allSimplices: Vector[Simplex] = simplicesToProcess ++ hullSimplices ++ Vector(nextSimplex)
//
//
////      DebugPrinter.print("Vertices of this simplex:")
////      DebugPrinter.print(nextSimplex.vertices)
//
////      DebugPrinter.print("Points that are still undetermined:")
////      DebugPrinter.print(undeterminedPoints)
//
//      val farthestPoint: Vector[Double] = undeterminedPoints.maxBy(nextSimplex.signedDistance)
//      val distanceToFarthestPoint: Double = nextSimplex.signedDistance(farthestPoint)
//
//      DebugPrinter.print("Farthest point:")
//      DebugPrinter.print(farthestPoint)
//
//      DebugPrinter.print("Distance to farthest point:")
//      DebugPrinter.print(distanceToFarthestPoint)
//
//
//
//      // If there are no points above this simplex, then it must belong to the convex hull. Otherwise, we need to split it into more simplices
//      // in a similar manner to how we initialized simplicesToProcess. The only differences are...
//      // ---- We must include the farthestPoint to avoid re-adding the original simplex
//      // ---- We will update undeterminedPoints only based on these new simplices
//
//      // HUGE complication: A new point might be visible to more than one simplex...
//      if (distanceToFarthestPoint <= 0) {
//        hullSimplices = hullSimplices.appended(nextSimplex)
//      } else {
//
//        val visibleSimplices: Vector[Simplex] = allSimplices.filter((simplex: Simplex) => simplex.signedDistance(farthestPoint) > 0.0)
//
//
//
//        DebugPrinter.print("Visible simplices")
//        visibleSimplices foreach {simplex => DebugPrinter.print(simplex.vertices)}
//
//        if (visibleSimplices.length == 1) {
//          val newSimplices: Vector[Simplex] = for (pointToExclude <- nextSimplex.vertices) yield {
//            val pointsOfThisSimplex: Vector[Vector[Double]] = nextSimplex.vertices.filter(_ != pointToExclude) ++ Vector(farthestPoint)
//            val distGuess: Vector[Double] => Double = LinearUtil.getSignedDistanceFunctionToHyperplane(pointsOfThisSimplex)
//
//            // We want to ensure that the points "above" this simplex are on the opposite side of the simplex from the excluded point.
//            // We also want to recognize points "above" this simplex as points with positive signed distance from the simplex.
//            // Hence, the excluded point should have negative signed distance. If it has positive signed distance, then we need to switch.
//            val shouldNegateDistanceFunc: Boolean = distGuess(pointToExclude) > 0
//
//            Simplex(
//              pointsOfThisSimplex,
//              if (shouldNegateDistanceFunc)
//                (v: Vector[Double]) => -distGuess(v)
//              else
//                distGuess
//            )
//          }
//
//          simplicesToProcess = simplicesToProcess ++ newSimplices
//          undeterminedPoints = undeterminedPoints.filter((point: Vector[Double]) => newSimplices.exists((simplex: Simplex) => simplex.signedDistance(point) > 0.0))
//        } else {
//
//          val simplicesAreAdjacent: (Simplex, Simplex) => Boolean = {case (s1, s2) =>
//            s1.vertices.intersect(s2.vertices).length == s1.vertices.length - 1
//          }
//
//
//          // A ridge is a pair of adjacent simplices where the first element of the pair is visible from a point, but the second element is not
//          val ridges: Vector[(Simplex, Simplex)] = for (
//            s1 <- visibleSimplices;
//            s2 <- allSimplices if (s2.signedDistance(farthestPoint) <= 0.0) && simplicesAreAdjacent(s1, s2)
//          ) yield (s1, s2)
//
////          DebugPrinter.print("ridges:")
////          ridges foreach {case (s1, s2) =>
////            DebugPrinter.print(s1.vertices.toString() + "  |  " + s2.vertices.toString())
////          }
//
//          val ridgeIntersections: Vector[Vector[Vector[Double]]] = ridges.map({case (s1, s2) => s1.vertices.intersect(s2.vertices)}).distinct
//
//          val horizonPoints = ridgeIntersections.flatten.distinct
//
//          DebugPrinter.print("Horizon points:")
//          DebugPrinter.print(horizonPoints)
//
//          val newSimplices: Vector[Simplex] = ridgeIntersections.map((ridgeIntersectionVertices: Vector[Vector[Double]]) => {
//
//            val pointsOfThisSimplex: Vector[Vector[Double]] = ridgeIntersectionVertices ++ Vector(farthestPoint)
//
////            DebugPrinter.print("Intersection size...")
////            DebugPrinter.print(ridgeIntersectionVertices.length)
//
//            val distGuess: Vector[Double] => Double = LinearUtil.getSignedDistanceFunctionToHyperplane(pointsOfThisSimplex)
//
//            val excludedPoint: Vector[Double] = horizonPoints.filter(p => !pointsOfThisSimplex.contains(p)).head
//
//            // We want to ensure that the points "above" this simplex are on the opposite side of the simplex from the excluded point.
//            // We also want to recognize points "above" this simplex as points with positive signed distance from the simplex.
//            // Hence, the excluded point should have negative signed distance. If it has positive signed distance, then we need to switch.
//            val shouldNegateDistanceFunc: Boolean = distGuess(excludedPoint) > 0
//
//            Simplex(
//              pointsOfThisSimplex,
//              if (shouldNegateDistanceFunc)
//                (v: Vector[Double]) => -distGuess(v)
//              else
//                distGuess
//            )
//          })
//
//          simplicesToProcess = simplicesToProcess.diff(visibleSimplices) ++ newSimplices
//          undeterminedPoints = undeterminedPoints.filter((point: Vector[Double]) => newSimplices.exists((simplex: Simplex) => simplex.signedDistance(point) > 0.0))
//
//        }
//
//
//      }
//    }
//
//    hullSimplices ++ simplicesToProcess



    // Todo: Figure out why the heck we do this culling step
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

