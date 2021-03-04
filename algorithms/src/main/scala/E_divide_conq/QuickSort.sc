object QuickSort {
  type Index = Int
  private implicit class ArrayOps(val a: Array[Int]) extends AnyVal {
    def fromIndex(i: Index): Int = a(i - 1)
  }
  private def swap(a: Array[Int], i:Index,j: Index): Array[Int] = {
    val ai = a.fromIndex(i)
    val aj = a.fromIndex(j)
    a.update(i - 1,aj)
    a.update(j - 1,ai)
    a
  }
  def quickSort(a: Array[Int], p: Index, r: Index): Unit =
    if (p < r) {
      val q = partition(a, p, r)
      quickSort(a, p, q - 1)
      quickSort(a, q + 1, r)
    }
  def partition(a: Array[Int], p: Index, r: Index): Index = {
    val pivot = a.fromIndex(r)
    val i = (p until r).foldLeft(p - 1){(i,j) =>
      if (a.fromIndex(j) <= pivot) {
        swap(a,i + 1,j)
        i + 1
      } else i
    }
    swap(a,i + 1,r)
    i + 1
  }
  def randomizedQuickSort(a: Array[Int], p: Index, r: Index): Unit =
    if (p < r) {
      val q = randomizedPartition(a, p, r)
      randomizedQuickSort(a, p, q - 1)
      randomizedQuickSort(a, q + 1, r)
    }
  def randomizedPartition(a: Array[Int], p: Index, r: Index): Index = {
    val i = random(p,r)
    swap(a,i,r)
    partition(a,p,r)
  }
  def randomizedSelect(a: Array[Int],p: Int,r: Int,i: Int): Int = {
    if (p == r) {
      a(p - 1)
    } else {
      val q = randomizedPartition(a,p,r)
      val k = q - p + 1
      if (i == k) {
        a(q - 1)
      } else if (i < k) {
        randomizedSelect(a,p,q-1,i)
      } else {
        randomizedSelect(a,q + 1,r,i - k)
      }
    }
  }

  def random(a: Int,b: Int): Int = {
    val randomDiff = scala.util.Random.nextInt(b - a + 1)
    a + randomDiff
  }

  type Counter = java.util.concurrent.atomic.AtomicInteger
  val counter = new java.util.concurrent.atomic.AtomicInteger(0)
  def tailRecursiveQuickSort(a: Array[Int], p: Index, r: Index): Unit = { // stack depth = O(n)
    var p1 = p
    while (p1 < r) {
      println(counter.incrementAndGet())
      val q = partition(a, p1, r)
      tailRecursiveQuickSort(a, p1, q - 1)
      p1 = q + 1
    }
  }
  def tailRecursiveQuickSort2(a: Array[Int], p: Index, r: Index): Unit = { // stack depth = O(lg n)
    def recur(p1: Int, r1: Int): Unit = if (p1 < r1) {
      counter.incrementAndGet()
      val q = partition(a, p1, r1)
      val m = (p1 + r1) / 2
      if (q < m) {
        tailRecursiveQuickSort2(a, p1, q - 1)
        recur(q + 1,r1)
      } else {
        tailRecursiveQuickSort2(a, q + 1, r1)
        recur(p1, q - 1)
      }
    }
    recur(p,r)
  }

  def test(f: (Array[Int],Int,Int) => Unit, a: Array[Int], expected: Array[Int]): Unit = {
    f(a,1,a.length)
    assert(a sameElements expected, s"${a.mkString("(",",",")")} != ${expected.mkString("(",",",")")}")
  }
}

import QuickSort._

import scala.util.Random

test(quickSort,Array(2,8,7,1,3,5,6,4), Array(1,2,3,4,5,6,7,8))
test(randomizedQuickSort,Array(2,8,7,1,3,5,6,4), Array(1,2,3,4,5,6,7,8))
//test(tailRecursiveQuickSort,Array(2,8,7,1,3,5,6,4,12,9,10,11), Array(1,2,3,4,5,6,7,8,9,10,11,12))
test(tailRecursiveQuickSort2,Array(2,8,7,1,3,5,6,4,12,9,10,11), Array(1,2,3,4,5,6,7,8,9,10,11,12))

//val big = Random.shuffle((1 to 1000000).toList).toArray
//test(tailRecursiveQuickSort2,big, big.sorted)
//counter.get()

val a = Array(2,8,7,1,3,5,6,4)
(1 to 8).foreach(i => assert(randomizedSelect(a,1,8,i) == i))
a
