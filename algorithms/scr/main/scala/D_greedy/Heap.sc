/**
  * Первая строка входа содержит число операций
  * 1≤n≤10x5. Каждая из последующих
  * n строк задают операцию одного из следующих двух типов:
  * Insert x, где 0≤x≤10*9 — целое число;
  * ExtractMax.
  * Первая операция добавляет число
  * x в очередь с приоритетами, вторая — извлекает максимальное число и выводит его.
  * Sample Input:
  * 6
  * Insert 200
  * Insert 10
  * ExtractMax
  * Insert 5
  * Insert 500
  * ExtractMax
  * Sample Output:
  * 200
  * 500
  */
object Main {
  import scala.io.StdIn._
  import scala.collection.mutable
  def main(args: Array[String]): Unit = {
    implicit val ord: Ordering[Int] = Ordering.Int
    val queue = mutable.PriorityQueue.empty[Int]
    val n     = readInt()
    (1 to n).foreach(_ => {
      val arr = readLine().split(" ")
      if (arr.size == 1) {
        println(queue.dequeue())
      } else {
        val (_, element) = (arr.head, arr.last.toInt)
        queue.enqueue(element)
      }
    })
  }
}
object Heap {
  private implicit class ArrayOps(val a: Array[Int]) extends AnyVal {
    def fromIndex(i: Int): Int = a(i - 1)
  }
  private def swapHeap(a: Array[Int], i:Int,j: Int): Array[Int] = {
    val ai = a.fromIndex(i)
    val aj = a.fromIndex(j)
    a.update(i - 1,aj)
    a.update(j - 1,ai)
    a
  }
  def parent(i: Int): Int = i >> 1
  def left(i: Int): Int = i << 1
  def right(i: Int): Int = (i << 1).toInt + 1

  def maxHeapify(a: Array[Int], i: Int): Array[Int] = {
    val heapSize = a.length
    val l = left(i)
    val r = right(i)
    val largest = {
      val ll = if (l <= heapSize && a.fromIndex(l) > a.fromIndex(i)) {
        l
      } else i

      if (r <= heapSize && a.fromIndex(r) > a.fromIndex(ll)) {
        r
      } else ll
    }

    if (largest != i) {
      maxHeapify(swapHeap(a,largest,i),largest)
    }
    a
  }
  def buildMaxHeap(a: Array[Int]) = {
    val heapSize = a.length
    ((heapSize / 2) to 1 by -1).foreach(i => maxHeapify(a,i))
    a
  }

  def heapSort(a: Array[Int]) = {
    val length = a.length
    val heap = buildMaxHeap(a)
    (a.length to 2 by -1).foldLeft(length) { case (heapSize,i) =>
      swapHeap(heap,1,i)
      maxHeapify(heap,1)
      heapSize - 1
    }
    a
  }

}
import Heap._

val heap0 = buildMaxHeap(Array(16,4,10,14,7,9,3,2,8,1))
assert(heap0 sameElements Array(16,14,10,8,7,9,3,2,4,1))

val heap = buildMaxHeap(Array(4, 1, 3, 2, 16, 9, 10, 14, 8, 7))
assert(heap sameElements Array(16,14,10,8,7,9,3,2,4,1))

heapSort(Array(5,6,2,3,4,7,8,33,22,3))
