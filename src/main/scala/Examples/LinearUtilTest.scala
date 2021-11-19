package Examples

import DimensionReduction.{LinearUtil, Point}
import spire.math.Rational

object LinearUtilTest extends App {
  val a: Point = Point(Vector(0.9051310475138654, 0.08442008172760171, -0.416666577286393))
  val b: Point = Point(Vector(0.9278894781448365, 0.03992321446481126, -0.3707118197399426))
  val c: Point = Point(Vector(0.9192682247682198, 0.13235814155761652, -0.3707118197399426))

  println((b - a).get.toString)
  println((c - a).get.toString)


  println(LinearUtil.getRREF(Vector((b - a).get.map(Rational(_)), (c - a).get.map(Rational(_)))))

}
