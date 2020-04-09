import scala.math

/**
 * Точка считается принадлежащей отрезку, если она находится внутри него или на границе.
 * Для каждой точки в порядке появления во вводе выведите, скольким отрезкам она принадлежит.
 *
 * точка НЕ лежит на данном отрезке только в двух случаях:
 * 1) Если координата конца отрезка меньше координаты точки;
 * 2) Если координата начала отрезка больше координаты точки;
 * В любом другом случае точка лежит на данном отрезке
 */
object Main {
  def solve(segmentCount: Int, pointCount: Int, segments: Seq[(Int,Int)], points: Array[Int]): Unit = {
    val sortedLeft = segments.sortBy(_._1)
    val sortedRight = segments.sortBy(_._2)

    def leftCount(point: Int) = sortedLeft.lastIndexWhere{case (left,right) => left <= point}
    def rightCount(point: Int) = sortedRight.lastIndexWhere{case (left,right) => right < point}

    def resultCount(point: Int) = leftCount(point) - rightCount(point)

    println(points.map(resultCount).mkString(" "))
  }
  def main(args: Array[String]): Unit = {
    val (segmentCount,pointCount)  = {
      val s = scala.io.StdIn.readLine().split(" ")
      s.head.toInt -> s.last.toInt
    }
    val segments = (1 to segmentCount).map(_ => {
      val s = scala.io.StdIn.readLine().split(" ")
      s.head.toInt -> s.last.toInt
    })
    val points = scala.io.StdIn.readLine().split(" ").map(_.toInt)

    solve(segmentCount, pointCount, segments, points)
  }
  def test(): Unit = {
    solve(2,3,Seq((0,5),(7,10)),Array(1,6,11))
  }
}
Main.test()
