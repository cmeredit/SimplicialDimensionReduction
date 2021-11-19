package Examples

import DimensionReduction.{GeometricSimplicialComplex, Point}

object LoadTest extends App {

  println("Loading...")

  val loadedGeomComp: Option[GeometricSimplicialComplex] = GeometricSimplicialComplex.loadFromFile("Datasets/UnculledNormalizedIrisComplex.txt")

  println("Loaded!")

  println(loadedGeomComp)


  loadedGeomComp match {
    case Some(complex) =>
      complex.printStatistics(30)
    case None =>
      println("Red alert! Failed to load simplicial complex from file!")
  }


  val culledGeomComp: Option[GeometricSimplicialComplex] = GeometricSimplicialComplex.loadFromFile("Datasets/CulledNormalizedIrisComplex.txt")

  culledGeomComp match {
    case Some(complex) =>
      complex.printStatistics(8)
      println("Brute force projection...")
      println(complex.project(Point(Vector(0.0, 0.0, 0.0, 0.0))))
      println(complex.project(Point(Vector(1.0, 1.0, 1.0, 1.0))))
      println(complex.project(Point(Vector(0.9444444444444444, 0.7499999999999998, 0.9661016949152542, 0.8750000000000001))))

      // Choose a single vertex from the complex...
      println(complex.project(complex.simplices.head.points.head))
    case None =>
      println("Red alert! Failed to load simplicial complex from file!")
  }

}
