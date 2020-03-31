import java.util.concurrent.atomic.AtomicLong

import scala.collection.mutable.ListBuffer

object Main {
  def sortWithInversion(array: Array[Int]): Long = {
    val counter = new AtomicLong(0L)
    def splitSortAndMerge(l: Array[Int]): Array[Int] = l match {
      case nil if nil.length < 2 =>
        l
      case _ =>
        val (left,right) = l.splitAt(l.length / 2)
        sortAndMerge(splitSortAndMerge(left),splitSortAndMerge(right)).toArray
    }
    def sortAndMerge(l: Array[Int], r: Array[Int], acc: ListBuffer[Int] = ListBuffer.empty): ListBuffer[Int] = (l,r) match {
      case (nl,nr) if (nl.isEmpty || nr.isEmpty)=>
        acc ++ r ++ l
      case (_, _) =>
        if (l(0) <= r(0)) {
          val (h,t) = l.splitAt(0 + 1)
          sortAndMerge(t, r, acc += h(0))
        } else {
          val old = counter.get()
          counter.set(old + l.length)
          val (h,t) = r.splitAt(0 + 1)
          sortAndMerge(l, t, acc += h(0))
        }
    }

    val l = array
    splitSortAndMerge(l)
    counter.get()
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

  def testTime(n: Int = 100000): Long = {
    val arr = scala.util.Random.shuffle((1 to n).toList).toArray
    val before = System.currentTimeMillis()
    sortWithInversion(arr)
    val after = System.currentTimeMillis()
    val diff = after - before
    println(diff)
    assert(diff < 3000, s"$diff > 3000")
    diff
  }
}
Main.test()
Main.testTime(1000)
Main.testTime(10000)
Main.testTime(20000)
Main.testTime(40000)
Main.testTime(100000)
