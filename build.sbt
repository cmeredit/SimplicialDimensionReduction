name := "SimplicialDimensionReduction"

version := "0.1"

scalaVersion := "2.13.7"

libraryDependencies += "org.typelevel" %% "spire" % "0.18.0-M1"

libraryDependencies ++= {
  val version = "3.1.6"
  val os = "linux" // TODO: Change to "linux" or "macos" if necessary

  Seq(
    "lwjgl",
    "lwjgl-glfw",
    "lwjgl-opengl"
    // TODO: Add more modules here
  ).flatMap {
    module => {
      Seq(
        "org.lwjgl" % module % version,
        "org.lwjgl" % module % version classifier s"natives-$os"
      )
    }
  }
}
