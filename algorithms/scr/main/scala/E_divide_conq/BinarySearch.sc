object Main {
  type Element = Int
  type Index   = Int
  def binarySearch[A](
    num:               A,
    array:             Array[A],
    l:                 Index,
    r:                 Index,
    increased:         Boolean = true
  )(implicit ordering: Ordering[A]): Index = {
    val index = (l + r) / 2
    if (l > r || r < l) {
      -1
    } else {
      val am = array(index)
      if (ordering.equiv(am, num)) {
        index
      } else if (if (increased) ordering.gt(am, num) else ordering.lt(am, num)) {
        binarySearch(num, array, l, index - 1)
      } else {
        binarySearch(num, array, index + 1, r)
      }
    }
  }
  def main(args: Array[String]): Unit = {
    val first  = scala.io.StdIn.readLine().split(" ").tail.map(_.toInt)
    val second = scala.io.StdIn.readLine().split(" ").tail.map(_.toInt)

    val result = second
      .map(s => binarySearch(s, first, 0, first.length - 1))
      .map(i => if (i != -1) i + 1 else i)

    println(result.mkString(" "))
  }
  def test(): Unit = {
    val first  = Array(1, 5, 8, 12, 13)
    val second = Array(8, 1, 23, 1, 11)

    assert(
      second
        .map(s => binarySearch[Int](s, first, 0, first.length - 1))
        .sameElements(Array(2, 0, -1, 0, -1))
    )

    assert(binarySearch(5, first,0, first.length - 1) == 1)
  }
}
Main.test()
