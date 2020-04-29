object Main {
  type Element = Int
  type Index   = Int
  def binarySearch[A](
    num:               A,
    array:             Array[A],
    l:                 Index,
    r:                 Index,
    growing:           Boolean = true
  )(implicit ordering: Ordering[A]): Index = {
    val index = (l + r) / 2
    if (l > r || r < l) {
      -1
    } else {
      val am = array(index)
      if (ordering.equiv(am, num)) {
        index
      } else if (if (growing) ordering.gt(am, num) else ordering.lt(am, num)) {
        binarySearch(num, array, l, index - 1, growing)
      } else {
        binarySearch(num, array, index + 1, r, growing)
      }
    }
  }
  def upperBound[A](
    num:               A,
    array:             Array[A],
    l:                 Index,
    r:                 Index,
    firstOccurrence:   Boolean = true,
    growing:           Boolean = true,
    resultIndex:       Option[Index] = None
  )(implicit ordering: Ordering[A]): Index = {
    val index = (l + r) / 2
    if (l > r || r < l || (firstOccurrence && resultIndex.isDefined)) {
      resultIndex.getOrElse(-1)
    } else {
      val am      = array(index)
      val updated = if (ordering.gt(am, num)) Some(index) else resultIndex

      if (growing) {
        upperBound(num, array, index + 1, r, firstOccurrence, growing, updated)
      } else {
        upperBound(num, array, l, index - 1, firstOccurrence, growing, updated)
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

    assert(binarySearch(5, first, 0, first.length - 1) == 1)

    assert(upperBound(5, first,0,first.length - 1, firstOccurrence = true,growing = true) == 2)
    assert(upperBound(5, first,0,first.length - 1, firstOccurrence = false,growing = true) == 4)
    assert(upperBound(5, first.reverse,0,first.length - 1, firstOccurrence = true,growing = false) == 2)
    assert(upperBound(5, first.reverse,0,first.length - 1, firstOccurrence = false,growing = false) == 0)
    assert(upperBound(13, first,0,first.length - 1) == -1)
    assert(upperBound(12, first,0,first.length - 1) == 4)
  }
}
Main.test()
