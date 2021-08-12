package tasks

import tasks.Stairs.stairs

object Stairs {
  def stairs(arr: Array[Int]): Int = {
    def process(sum: Int, prev: Int, element: Int) =
      if (prev > element) (sum + element, element)
      else (sum + prev, element)

    val res = arr.tail
      .foldLeft((0, arr.head)) {
        case ((sum, prev), element) =>
          process(sum, prev, element)
      }

    res._1
  }

  def main(args: Array[String]): Unit = {
    scala.io.StdIn.readLine().toInt
    val s2 = scala.io.StdIn.readLine().split(" ").map(_.toInt)

    println(stairs(s2))
  }
}

object StairsTest extends App {
  def test() = {
    assert(-63 == stairs(Array(-2, -16, -13, -9, -48)), 1)
    assert(2 == stairs(Array(1, 1, -2, -4, -6, 2, 2)), 2)
    assert(-73 == stairs(Array(-64, -16, -13, -9, -48)), 3)
    assert(5 == stairs(Array(0, 0, 0, 4, 6, -5)), 4)
    assert(-9 == stairs(Array(-6, 4, -16, -13, -9, 0)), 5)
    assert(-18 == stairs(Array(-6, 4, -16, -13, -9)), 6)
    assert(21 == stairs(Array(3, 4, 10, 10, 0, -6, -10, 0)), 7)
    assert(3 == stairs(Array(1, 2)), 8)
    assert(1 == stairs(Array(2, -1)), 9)
    assert(3 == stairs(Array(-1, 2, 1)), 10)
    assert(2 == stairs(Array(2)), 11)
    assert(-2 == stairs(Array(-2)), 12)
  }

  test()
}
