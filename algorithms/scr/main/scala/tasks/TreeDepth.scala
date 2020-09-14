package tasks

object TreeDepth {

  def byParents(n: Int, parents: Array[Int]): Int =
    if (n != 0) {
      @scala.annotation.tailrec
      def recur(index: Int, count: Int): Int = parents(index) match {
        case -1   => count + 1
        case next => recur(next, count + 1)
      }

      parents.foldLeft(0) {
        case (maxDepth, parentIndex) =>
          if (parentIndex < 0) maxDepth
          else {
            val current = recur(parentIndex, 1)
            math.max(current, maxDepth)
          }
      }
    } else {
      1
    }

}
object TreeDepthTest extends App {
  def test(n: String, parents: String, expected: String): Unit = {
    val result =
      TreeDepth.byParents(n.toInt, parents.split(" ").map(_.toInt)).toString
    println(s"TreeDepth of ($parents) = $result, expected = $expected")
    assert(result == expected, s"result != expected, $result != $expected")
  }

  test("10", "9 7 5 5 2 9 9 9 2 -1", "4")
  test("10", "2 2 3 5 5 7 7 9 9 -1", "6")
  test("0", "-1", "1")
}
