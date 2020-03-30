
object Main {
  def sortWithInversion(array: Array[Int]): Int = {
    def splitSortAndMerge(l: List[Int], acc: Int): (List[Int],Int) = l match {
      case Nil =>
        l -> acc
      case List(one) =>
        l -> acc
      case _ =>
        val (left,right) = l.splitAt(l.size / 2)
        val (ls,lacc) = splitSortAndMerge(left,acc)
        val (rs,racc) = splitSortAndMerge(right,acc)
        sortAndMerge(ls,rs,lacc + racc)
    }
    def sortAndMerge(l: List[Int], r: List[Int], acc: Int): (List[Int],Int) = (l,r) match {
      case (Nil,_) =>
        r -> acc
      case (_,Nil) =>
        l -> acc
      case (lh :: lt, rh :: rt) =>
        if (lh <= rh) {
          val (res,resAcc) = sortAndMerge(lt, r, acc)
          (lh :: res, resAcc)
        } else {
          val (res,resAcc) = sortAndMerge(l, rt, acc + l.size)
          (rh :: res, resAcc)
        }
    }

    val l = array.toList
    splitSortAndMerge(l, 0)._2
  }

  def main(args: Array[String]): Unit = {
    val n  = scala.io.StdIn.readInt()
    val array = scala.io.StdIn.readLine().split(" ").map(_.toInt)

    def result = sortWithInversion(array)

    println(if (n < 2) 0 else result)
  }
  def test(): Unit = {
    assert(sortWithInversion(Array(2,3,9,2,9)) == 2)
    assert(sortWithInversion(Array(7,6,5,4,3,2,1)) == 21)
    assert(sortWithInversion(Array(1,2,3,5,4)) == 1)
    assert(sortWithInversion(Array(10, 8, 6, 2, 4, 5)) == 12)
    assert(sortWithInversion(Array(1, 9, 8, 1, 4, 1)) == 8)
    assert(sortWithInversion(Array(6, 5, 8, 6, 0, 4)) == 10)
    assert(sortWithInversion(Array(6, 2, 3, 7, 5, 8)) == 4)
    assert(sortWithInversion(Array(6, 4, 5, 0, 0, 2)) == 11)
    assert(sortWithInversion(Array(8, 9, 10, 7, 4, 0)) == 12)
    assert(sortWithInversion(Array(10, 9, 3, 8, 3, 10)) == 8)
    assert(sortWithInversion(Array(9, 10, 9, 5, 7, 7)) == 10)
    assert(sortWithInversion(Array(9, 5, 8, 9, 4, 10)) == 6)
    assert(sortWithInversion(Array(5, 7, 0, 2, 2, 0)) == 10)
  }

  def testTime(): Unit = {
    val arr = scala.util.Random.shuffle((1 to 100000).toList).toArray
    val before = System.currentTimeMillis()
    sortWithInversion(arr)
    val after = System.currentTimeMillis()
    val diff = after - before
    println(diff)
    assert(diff < 3000)
  }
}
Main.test()
Main.testTime()
