package DataLoaders

import DimensionReduction.Delaunay.{DelaunayUtil, PointedAffineSpace}
import DimensionReduction.{GeometricSimplicialComplex, Point, DimensionStatUtil, Simplex}

import scala.io.{BufferedSource, Source}

class IrisDatum(override val coordinates: Vector[Double], val name: String) extends Point(coordinates)

/** Utility object for loading the iris dataset. */
object IrisLoader {

  /** Returns the Iris dataset formatted as a Vector of points and a Vector of colors
   *
   *  @return The Iris dataset
   */
  def getIrisData(normalizeToHypercube: Boolean = false): (Vector[Vector[Float]], Vector[Vector[Float]]) = {
    val bufferedIrisSource: BufferedSource = Source.fromFile("Datasets/iris.data")
    val irisData: Vector[IrisDatum] = bufferedIrisSource.getLines().map(line => {
      val entries: Array[String] = line.split(",")
      val coordinates: Vector[Double] = entries.reverse.tail.reverse.map(_.toDouble).toVector
      val name: String = entries.reverse.head
      new IrisDatum(coordinates, name)
    }).toVector.distinctBy(_.coordinates)
    bufferedIrisSource.close()

    val nameToColor: String => Vector[Float] = {
      case "Iris-setosa" => Vector(1.0f, 0.0f, 0.0f)
      case "Iris-versicolor" => Vector(0.0f, 1.0f, 0.0f)
      case "Iris-virginica" => Vector(0.0f, 0.0f, 1.0f)
      case _ => Vector(0.0f, 0.0f, 0.0f)
    }

    val minCorner: Point = Point(irisData.map(p => p.coordinates).transpose.map(_.min))
    val maxCorner: Point = Point(irisData.map(p => p.coordinates).transpose.map(_.max))

    def mapToHypercube(v: Vector[Double]): Vector[Double] = {
      ((Point(v) - minCorner).get / (maxCorner - minCorner).get).get.coordinates
    }



    irisData.map(p => (if (normalizeToHypercube) mapToHypercube(p.coordinates).map(_.toFloat) else p.coordinates.map(_.toFloat), nameToColor(p.name))).unzip
  }

  def getIrisCoords(normalizeToHypercube: Boolean = false): (Vector[Vector[Double]], Vector[Vector[Float]]) = {
    val bufferedIrisSource: BufferedSource = Source.fromFile("Datasets/iris.data")
    val irisData: Vector[IrisDatum] = bufferedIrisSource.getLines().map(line => {
      val entries: Array[String] = line.split(",")
      val coordinates: Vector[Double] = entries.reverse.tail.reverse.map(_.toDouble).toVector
      val name: String = entries.reverse.head
      new IrisDatum(coordinates, name)
    }).toVector.distinctBy(_.coordinates)
    bufferedIrisSource.close()

    val nameToColor: String => Vector[Float] = {
      case "Iris-setosa" => Vector(1.0f, 0.0f, 0.0f)
      case "Iris-versicolor" => Vector(0.0f, 1.0f, 0.0f)
      case "Iris-virginica" => Vector(0.0f, 0.0f, 1.0f)
      case _ => Vector(0.0f, 0.0f, 0.0f)
    }

    val minCorner: Point = Point(irisData.map(p => p.coordinates).transpose.map(_.min))
    val maxCorner: Point = Point(irisData.map(p => p.coordinates).transpose.map(_.max))

    def mapToHypercube(v: Vector[Double]): Vector[Double] = {
      ((Point(v) - minCorner).get / (maxCorner - minCorner).get).get.coordinates
    }

    irisData.map(p => (if (normalizeToHypercube) mapToHypercube(p.coordinates) else p.coordinates, nameToColor(p.name))).unzip
  }

}

object TestSimpIris extends App {
  val (irisData, _): (Vector[Vector[Double]], Vector[Vector[Float]]) = IrisLoader.getIrisCoords(true)

  irisData foreach println


  val allDists = {for (
    x <- irisData;
    y <- irisData
  ) yield Point(x).dist(Point(y))}.flatten
  val averageDistance: Double = allDists.sum / allDists.length.toDouble

  println("Average distance between any two points: " + averageDistance)

  val minimumDists = irisData.map(x => irisData.flatMap(y => Point(x).dist(Point(y))).filter(_ > 0.0).min)
  val averageMinimumDistance: Double = minimumDists.sum / minimumDists.length.toDouble

  println("Average minimum distance between points: " + averageMinimumDistance)

  println("Loaded iris data")

  val delaunay: Vector[PointedAffineSpace] = DelaunayUtil.getDelaunaySimplicialization(irisData.map(v => Point(v)))

  delaunay foreach {point => println(point.vertices)}

  println("Got Delaunay simplicialization")

  val simp = new GeometricSimplicialComplex(delaunay)

  println("Got simplicial complex.")

  simp.exportToFile("Datasets/UnculledNormalizedIrisComplex.txt")

//  simp.nSimplices foreach { case (n, nSimplices) =>
//    println(n + "-Simplices:")
//    nSimplices.foreach((simplex: Simplex) =>
//      println(if (simplex.points.nonEmpty) {"[" + simplex.points.map(_.toString).reduce(_ + ", " + _) + "]"} else "[]")
//    )
//  }

  simp.nSimplices foreach {case (n, nSimps) => println("There are " + nSimps.length + " " + n + "-simplices.")}

  val vertexMembership: Vector[(Simplex, Vector[Simplex])] = simp.nSimplices(0).map(vertex => (vertex, simp.ancestorMap(vertex).filter(_.points.size == 2)))

  vertexMembership foreach println

//  println("Vertex membership:")
//  vertexMembership.foreach({case (v, ancestors) =>
//    println(v + " belongs to " + ancestors.map(_.toString).reduce(_ + ", and " + _))
//  })

  println("Vertex degrees")
  vertexMembership.map({case (_, ancestors) => ancestors.length}).groupBy(x => x).map(x => (x._1, x._2.length)).foreach(println(_))

  val edgeLengths = simp.nSimplices(1).flatMap(simplex => {
    val p0: Point = simplex.points.toVector(0)
    val p1: Point = simplex.points.toVector(1)

    p1.dist(p0)
  })

  val sortedEdgeLengths = edgeLengths.sorted

  val medianEdgeLength: Double = sortedEdgeLengths(sortedEdgeLengths.length / 2)

  val averageEdgeLength = edgeLengths.sum / edgeLengths.length.toDouble

  println("Number of edges: " + simp.nSimplices(1).length)
  println("Average edge length: " + averageEdgeLength)
  println("Median edge length: " + medianEdgeLength)


//  edgeLengths foreach println

//  assert(false)

  val averageMinDistanceInHypercube: Double = DimensionStatUtil.getAverageMinDistanceInHypercube(4, 147, 100)

  val edgesToExclude = simp.nSimplices(1).filter(edge => {
    val p0: Point = edge.points.toVector(0)
    val p1: Point = edge.points.toVector(1)

    val edgeLength: Double = p0.dist(p1).get


    edgeLength > averageMinDistanceInHypercube
  })

  println("Number of edges to exclude: " + edgesToExclude.length)

  val culledSimplicialComplex: GeometricSimplicialComplex = simp.getComplexWithout(edgesToExclude)


  culledSimplicialComplex.exportToFile("Datasets/CulledNormalizedIrisComplex.txt")

}