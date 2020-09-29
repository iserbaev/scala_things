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
    if (result != expected) println(s"($result) != ($expected)")
  }

  test("2 1 5", 1, "2 1 5")
  test("2 7 3 1 5 2 6 2", 4, "7 7 5 6 6")
  test(
    "73 65 24 14 44 20 65 97 27 6 42 1 6 41 16",
    7,
    "73 97 97 97 97 97 97 97 42"
  )
  test("28 7 64 40 68 86 80 93 4 53 32 56 68 18 59", 12, "93 93 93 93")
  test("16 79 20 19 43 72 78 33 40 52 70 79 66 43 60 ", 12, "79 79 79 79")

  test(
    "34 51 61 90 26 84 2 25 7 8 25 78 21 47 25",
    3,
    "61 90 90 90 84 84 25 25 25 78 78 78 47"
  )

  test(
    "27 83 29 77 6 3 48 2 16 72 46 38 55 2 58",
    5,
    "83 83 77 77 48 72 72 72 72 72 58"
  )

  test("1 4 5 6 1 1 1 1", 4, "6 6 6 6 1")
}
