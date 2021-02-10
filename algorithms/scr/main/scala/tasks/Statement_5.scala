package tasks

import scala.collection.mutable.ArrayBuffer

object Statement_5 {

  def maxInWindow(n: Int, a: Seq[Int], m: Int): Seq[Int] = {
    def slice(max: Int, i: Int, element: Int): (Int, Int) =
      if (i < m) {
        (max.max(element), i + 1)
      } else {
        (element, 1)
      }
    val acc =
      a.tail
        .foldLeft(
          ((a.head, m), (a.head, 1), ArrayBuffer(a.head))
        ) {
          case (((max0, i0), (max1, i1), acc), e) =>
            if (i0 < m) {
              val max = slice(max0, i0, e)
              acc.append(max._1)
              (max, slice(max1, i1, e), acc)
            } else {
              val max = slice(max1, i1, e)
              acc.append(max._1)
              (max, (e, 1), acc)
            }
        }
        ._3

    acc.drop(m - 1)
  }

}
object TestApp5 extends App {
  def test(a: String, m: Int, expected: String): Unit = {
    val aa = a.split(" ").map(_.toInt)
    val result =
      Statement_5.maxInWindow(aa.length, aa, m).mkString(" ")
    assert(result == expected, s"($result) != ($expected)")
  }

  test("2 1 5", 1, "2 1 5")
  test("2 7 3 1 5 2 6 2", 4, "7 7 5 6 6")
  test(
    "73 65 24 14 44 20 65 97 27 6 42 1 6 41 16",
    7,
    "73 97 97 97 97 97 97 97 42"
  )
  test("28 7 64 40 68 86 80 93 4 53 32 56 68 18 59", 12, "93 93 93 93")
}
