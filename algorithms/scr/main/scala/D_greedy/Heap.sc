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
  def parent(i: Int): Int = i >> 1
  def left(i: Int): Int = i << 1
  def right(i: Int): Int = (i << 1).toInt + 1

  def maxHeapify(a: Array[Int], i: Int, heapSize: Int): Unit = {
    val l = left(i)
    val r = right(i)
    val largest = {
      val ll = if (l < heapSize && a(l) > a(i)) {
        l
      } else i

      if (r < heapSize && a(r) > a(ll)) {
        r
      } else ll
    }

    if (largest != i) {
      val al = a(largest)
      val ai = a(i)
      a.update(i,al)
      a.update(l,ai)
    }
  }
  def swap(a: Array[Int], i:Int,j: Int):Unit = {
    val aj = a(i)
    val ai = a(j)
    a.update(i,aj)
    a.update(j,ai)
  }
  def buildMaxHeap(a: Array[Int]) = {
    val heapSize = a.length
    ((a.length / 2) to 1 by -1).foreach(i => maxHeapify(a,i, heapSize))
  }

  def heapSort(a: Array[Int]) = {
    val length = a.length
    buildMaxHeap(a)
    (a.length to 2 by -2).foldLeft(length) { case (heapSize,i) =>
      swap(a,1,i)
      maxHeapify(a,1,heapSize - 1)
      heapSize - 1
    }
    a
  }

}
import Heap._

parent(3) == 1
left(2) == 4
right(2) == 5

(6 to 1 by -1).toList

heapSort(Array(5,6,2,3,4,7,8,33,22,3))
