object Main {
  type Element = Int
  type Index = Int
  @scala.annotation.tailrec def binarySearchWithPredicate(
      num: Element,
      predicate: Element => Boolean,
      lastOccurence: Boolean = false,
      prevIndex: Option[Index] = None,
      array: Array[Element],
      l: Index,
      r: Index
  ): Index = {
    val index = (l + r) / 2
    val am = array(index)
    val checkResult = predicate(am)
    val checkedIndex = if (checkResult) {Option(index)} else {prevIndex}
    if (l > r) {
      checkedIndex.getOrElse(-1)
    } else {
      if ((am == num && checkResult && lastOccurence) || am < num) {
        binarySearchWithPredicate(num, predicate, lastOccurence, checkedIndex, array, index + 1, r)
      } else {
        binarySearchWithPredicate(num, predicate, lastOccurence, checkedIndex, array, l, index - 1)
      }
    }
  }
  def binarySearch[A](num: A, array: Array[A], l: Int, r: Int, increased: Boolean = true)(implicit ordering: Ordering[A]): Int = {
    val index = (l + r) / 2
    if (l > r || r < l) {
      -1
    } else {
      val am = array(index)
      if (ordering.equiv(am,num)) {
        index
      } else if (if (increased) ordering.gt(am,num) else ordering.lt(am,num)) {
        binarySearch(num,array,l, index - 1)
      } else {
        binarySearch(num,array,index + 1, r)
      }
    }
  }
  def main(args: Array[String]): Unit = {
    val first  = scala.io.StdIn.readLine().split(" ").tail.map(_.toInt)
    val second = scala.io.StdIn.readLine().split(" ").tail.map(_.toInt)

    val result = second.map(s => binarySearch(s, first,0,first.length - 1)).map(i => if (i != -1) i + 1 else i)

    println(result.mkString(" "))
  }
  def test(): Unit = {
    val first  = Array(1,5,8,12,13)
    val second = Array(8,1,23,1,11)

    val result = second.map(s => binarySearch(s, first,0,first.length - 1)).map(i => if (i != -1) i + 1 else i)
    println(result.mkString(" "))
  }
  def testPredicateSerach(): Unit = {
    val arr = Array(1,2,2,4,5,6,77,77,88)
    println(binarySearchWithPredicate(2, (i: Int) => i <= 2, lastOccurence = true,None, arr,0, arr.length - 1))
    println(binarySearchWithPredicate(2, (i: Int) => i <= 2, lastOccurence = false,None, arr,0, arr.length - 1))
    println(binarySearchWithPredicate(77, (i: Int) => i <= 77, lastOccurence = true,None, arr,0, arr.length - 1))
    println(binarySearchWithPredicate(77, (i: Int) => i <= 77, lastOccurence = false,None, arr,0, arr.length - 1))
  }
}
Main.test()
Main.testPredicateSerach()
