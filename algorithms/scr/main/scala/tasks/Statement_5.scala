package tasks

import scala.collection.mutable.ArrayBuffer

object Statement_5 {

  case class MaxWindow(windowSize: Int) {
    type ElElIdxMaxMaxIdx = (Int, Int, Int, Int)

    private val buf = ArrayBuffer[ElElIdxMaxMaxIdx]()

    def max: Int =
      buf.last._3

    private def lastIdx = buf.length - 1

    def add(e: Int): Unit =
      if (buf.nonEmpty) {
        val (_, _, prevMax, prevMaxIdx) = buf.last
        val newIdx                      = lastIdx + 1
        if (prevMax <= e) {
          buf.append((e, newIdx, e, newIdx))
        } else {
          if (prevMaxIdx > (newIdx - windowSize))
            buf.append((e, newIdx, prevMax, prevMaxIdx))
          else {
            val (newMax, newMaxIdx) = {
              val from =
                if (lastIdx + 1 < windowSize) 0
                else newIdx - windowSize + 1
              val to    = lastIdx + 1
              val slice = buf.slice(from, to)
              val (ee, eeIdx, _, _) =
                if (slice.nonEmpty) slice.maxBy(_._1)
                else (e, newIdx, e, newIdx)

              if (e >= ee) (e, newIdx)
              else (ee, eeIdx)
            }
            buf.append((e, newIdx, newMax, newMaxIdx))
          }
        }
      } else {
        buf.append((e, 0, e, 0))
      }

  }

  def maxInWindow(n: Int, a: Array[Int], m: Int): Seq[Int] = {
    val result =
      if (a.length <= m) Seq(a.max)
      else {
        val maxWindow = MaxWindow(m)
        a.foldLeft(ArrayBuffer.empty[Int]) {
          case (acc, e) =>
            maxWindow.add(e)
            acc.append(maxWindow.max)
            acc
        }
      }

    result.slice(m - 1, n)
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
