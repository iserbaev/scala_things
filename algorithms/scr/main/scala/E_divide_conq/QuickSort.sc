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

  def random(a: Int,b: Int): Int = {
    val randomDiff = scala.util.Random.nextInt(b - a + 1)
    a + randomDiff
  }

  def test(f: (Array[Int],Int,Int) => Unit, a: Array[Int], expected: Array[Int]): Unit = {
    f(a,1,8)
    assert(a sameElements expected)
  }
}

import QuickSort._

test(quickSort,Array(2,8,7,1,3,5,6,4), Array(1,2,3,4,5,6,7,8))
test(randomizedQuickSort,Array(2,8,7,1,3,5,6,4), Array(1,2,3,4,5,6,7,8))
