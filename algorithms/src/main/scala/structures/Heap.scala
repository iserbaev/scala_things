package structures

import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering.Implicits._
import scala.reflect.ClassTag

case class Heap[A: ClassTag](maxSize: Int)(implicit val ord: Ordering[A]) {
  import Heap._

  def parent(i: Int): Int = i >> 1
  def left(i:   Int): Int = i << 1
  def right(i:  Int): Int = (i << 1) + 1

  def siftUp(a: Array[A], i: Int): Unit = {
    var ii = i
    while (ii > 1 && a.fromIndex(parent(ii)) < a.fromIndex(ii)) {
      swapHeap(a, parent(ii), ii)
      ii = parent(ii)
    }
  }

  private val swapCounter = new ArrayBuffer[String]()
  private def swapHeap(a: Array[A], i: Int, j: Int): Array[A] = {
    swapCounter += s"${i - 1} ${j - 1}"
    val ai = a.fromIndex(i)
    val aj = a.fromIndex(j)
    a.update(i - 1, aj)
    a.update(j - 1, ai)
    a
  }
  def siftDown(a: Array[A], i: Int): Array[A] = {
    val heapSize = a.length
    val l        = left(i)
    val r        = right(i)
    val largest = {
      val ll = if (l <= heapSize && a.fromIndex(l) > a.fromIndex(i)) {
        l
      } else i

      if (r <= heapSize && a.fromIndex(r) > a.fromIndex(ll)) {
        r
      } else ll
    }

    if (largest != i) {
      siftDown(swapHeap(a, i, largest), largest)
    }
    a
  }

  private var buffer = Array.empty[A]
  def buildMaxHeap(a: Array[A]): Array[A] = {
    val heapSize = a.length
    ((heapSize / 2) to 1 by -1).foreach(i => siftDown(a, i))
    buffer = a
    a
  }

  def result: List[A]      = buffer.toList
  def swaps:  List[String] = swapCounter.result().toList

  def heapSort(a: Array[A]): List[A] = {
    val length = a.length
    val heap   = buildMaxHeap(a)
    val res = (a.length to 2 by -1).foldLeft((length, heap, List.empty[A])) {
      case ((heapSize, h, a), i) =>
        val swapped = swapHeap(h, 1, i)
        val nHeap   = siftDown(swapped.init, 1)
        (heapSize - 1, nHeap, swapped.last :: a)
    }
    res._2.head :: res._3
  }

  override def toString: String =
    result.mkString("(", ",", ")")
}
object Heap {
  implicit class ArrayOps[A](val a: Array[A]) extends AnyVal {
    def fromIndex(i: Int): A = a(i - 1)
  }
  def build[A: ClassTag](a: Array[A])(implicit ord: Ordering[A]): Heap[A] = {
    val h = new Heap[A](a.length + 1)
    h.buildMaxHeap(a)
    h
  }
  def min(a: Array[Int]): Heap[Int] =
    build(a)(ClassTag.Int, Ordering.Int.reverse)

  def max(a: Array[Int]): Heap[Int] =
    build(a)(ClassTag.Int, Ordering.Int)
}

object TestHeap extends App {
  val array   = Array(7, 6, 5, 4, 3, 2)
  val minHeap = Heap.min(array)
  val maxHeap = Heap.max(array.reverse)

  def test() = ???

  println(minHeap.swaps)
}
