package ScalaComp

import scala.io.{BufferedSource, Source}

class IrisDatum(override val coordinates: List[Double], val name: String) extends Point(coordinates)

object Main {

  def run(): Unit = {
    val points: List[Point] = List(
      List(0.0),
      List(1.0),
      List(1.5),
      List(2.0)
    ).map(new Point(_))
    DBCluster.cluster(1.0, 2)(points) foreach println
  }

  def getIrisData: (Vector[Vector[Float]], Vector[Vector[Float]]) = {
    val bufferedIrisSource: BufferedSource = Source.fromFile("Datasets/iris.data")
    val irisData: Vector[IrisDatum] = bufferedIrisSource.getLines().map(line => {
      val entries: Array[String] = line.split(",")
      val coordinates: List[Double] = entries.reverse.tail.reverse.map(_.toDouble).toList
      val name: String = entries.reverse.head
      new IrisDatum(coordinates, name)
    }).toVector.distinctBy(_.coordinates)
    bufferedIrisSource.close()

    irisData.map(p => (p.coordinates.toVector.map(_.toFloat), p.name match {
      case "Iris-setosa" => Vector(1.0f, 0.0f, 0.0f)
      case "Iris-versicolor" => Vector(0.0f, 1.0f, 0.0f)
      case "Iris-virginica" => Vector(0.0f, 0.0f, 1.0f)
      case _ => Vector(0.0f, 0.0f, 0.0f)
    })).unzip
  }

}
