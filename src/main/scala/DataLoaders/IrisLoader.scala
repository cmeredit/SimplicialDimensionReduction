package DataLoaders

import DimensionReduction.Delaunay.{DelaunayUtil, PointedAffineSpace}
import DimensionReduction.{GeometricSimplicialComplex, Point, RandomDistStats, Simplex}

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
