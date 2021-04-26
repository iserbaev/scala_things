/**
 * Задание:
 * Даны отрезки на прямой.
 * Найти такие точки, которые лежат на всех (!!) заданных отрезках.
 * Найденное множество должно быть минимальным по размеру.
 */
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.StdIn
object Main {
  type Point   = Long
  type Segment = (Point, Point)
  type Input   = (Int, Seq[Segment])
  type Output  = (Int, Seq[Point])

  def consist(l: Segment, r: Segment): Seq[Segment] =
    (l, r) match {
      case ((_, lr), (rl, rr)) if lr >= rl && lr <= rr => Seq(l)
      case ((_, lr), (rl, rr)) if lr > rl && lr > rr   => Seq(r)
      case _                                           => Seq(l, r)
    }

  def pointsCover: Input => Output =
    input =>
      input._1 match {
        case 0 | 1 =>
          0 -> Seq()
        case _ =>
          val sortedL = input._2.sortBy(_._2)
          val segments = sortedL.foldLeft(ListBuffer.empty[Segment]) {
            case (acc, segment) =>
              if (acc.isEmpty) acc.append(segment)
              else {
                val last = consist(acc.last, segment)
                acc.append(last: _*)
              }
              acc
          }
          val points = segments.foldLeft(ListBuffer.empty[Point]) {
            case (acc, segment) =>
              if (acc.isEmpty) acc.append(segment._2)
              else {
                (acc.last, segment) match {
                  case (point, (rl, _)) if point >= rl => acc
                  case (point, (rl, rr)) if point < rl || point > rr =>
                    acc.append(rr)
                }
              }
              acc
          }
          points.size -> points.toSeq
      }
  def main(args: Array[String]): Unit = {
    val n = scala.io.StdIn.readInt()
    val lrs = (1 to n).map { _ =>
      val arr = StdIn.readLine().split(" ").map(_.toLong)
      arr.head -> arr.last
    }
    val out = pointsCover(n, lrs)
    println(out._1)
    println(out._2.mkString("\n"))
  }
}


import Main._

val tests: Seq[(Input, Output)] = List(
  (3 -> Seq(1L -> 3L, 2L -> 5L, 3L -> 6L), 1 -> Seq(3L)),
  (4 -> Seq(4L -> 7L, 1L -> 3L, 2L -> 5L, 5L -> 6L), 2 -> Seq(3L,6L)),
  (5 -> Seq(5L -> 6L, 4L -> 7L, 3L -> 8L, 2L -> 9L, 1L -> 10L), 1 -> Seq(6L)),
  (5 -> Seq(1L -> 2L, 2L -> 3L, 3L -> 4L, 4L -> 5L, 5L -> 6L), 3 -> Seq(2L,4L,6L)),
  (5 -> Seq(1L -> 2L, 3L -> 4L, 5L -> 6L,7L -> 8L,9L -> 10L), 5 -> Seq(2L,4L,6L,8L,10L))
)

def check(tests: Seq[(Input, Output)], f: Input => Output): Unit =
  tests.foreach{ case (in,out) =>
    assert(f(in) == out, s"f(in) != out. \n in = $in, \n out = $out \n f(in) = ${f(in)}")
  }

check(tests, (in: Input) => tests.find(_._1 == in).get._2)

tests.last._1._2.sortBy(_._1)

pointsCover(tests.last._1._1,tests.last._1._2)

check(tests, pointsCover)