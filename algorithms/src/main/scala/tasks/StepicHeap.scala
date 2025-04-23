package tasks

import java.io.{ BufferedReader, InputStreamReader }
import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering.Implicits._

class StepicHeap[T]()(implicit ord: Ordering[T]) {
  def getParent(i: Int): Int = (i - 1) >> 1 // (index - 1) / 2
  def getLeft(i: Int): Int   = (i << 1) + 1 // 2 * index + 1
  def getRight(i: Int): Int  = (i << 1) + 2 // 2 * index + 2

  protected val heap: ArrayBuffer[T] = ArrayBuffer.empty[T]

  def heapifyUp(idx: Int): Int = if (idx != 0) {
    val parent = getParent(idx)

    if (heap(idx) > heap(parent)) {
      swap(idx, parent)
      heapifyUp(parent)
    } else idx
  } else idx

  protected def swap(idx1: Int, idx2: Int): Unit = {
    swapHeap(idx1, idx2)(heap)
  }

  protected def add(x: T): Unit = {
    heap += x
  }

  protected def remove(idx: Int): T = {
    heap.remove(idx)
  }

  protected def swapHeap[A](idx1: Int, idx2: Int)(h: ArrayBuffer[A]): Unit = {
    val e1 = h(idx1)
    val e2 = h(idx2)
    h.update(idx1, e2)
    h.update(idx2, e1)
  }

  def heapifyDown(idx: Int): Int = {
    val left    = getLeft(idx)
    val right   = getRight(idx)
    var largest = idx

    if (left < heap.size && heap(left) > heap(largest)) {
      largest = left
    }
    if (right < heap.size && heap(right) > heap(largest)) {
      largest = right
    }

    if (largest != idx) {
      swap(idx, largest)
      heapifyDown(largest): Unit
    }

    largest
  }

  def insert(x: T): Int = {
    add(x)
    val idx = heap.size - 1
    heapifyUp(idx)
  }

  def extractMax(): T = {
    require(heap.nonEmpty, "Empty heap")

    val removedMax = remove(0)
    heapifyDown(0): Unit
    removedMax
  }

  def getMax: T =
    heap(0)

}

class StepicHeapWithCmdCounter(implicit ordering: Ordering[Int]) extends StepicHeap[Int] {
  private var cmdCounter      = 0
  private val cmdBuffer = ArrayBuffer.empty[Int]

  override def insert(x: Int): Int = {
    cmdCounter += 1
    val resultIdx = super.insert(x)
    resultIdx
  }

  override def extractMax(): Int = {
    cmdCounter += 1
    val result = super.extractMax()
    result
  }

  override def getMax: Int = {
    cmdCounter += 1
    val max = super.getMax
    max
  }

  override protected def add(x: Int): Unit = {
    super.add(x): Unit
    cmdBuffer += cmdCounter
  }

  override protected def remove(idx: Int): Int = {
    val result = super.remove(idx)
    cmdBuffer.remove(idx): Unit
    result
  }

  override protected def swap(idx1: Int, idx2: Int): Unit = {
    super.swap(idx1, idx2): Unit
    swapHeap(idx1, idx2)(cmdBuffer)
  }

  def decreaseKey(cmdCount: Int, decreaseValue: Int): Unit = {
    cmdCounter += 1
    val idx = cmdBuffer.indexOf(cmdCount)
    val old = heap(idx)
    heap.update(idx, old - decreaseValue): Unit
    heapifyDown(0): Unit
    ()
  }
}

trait LoggableStepikHeap[T] extends StepicHeap[T] {
  override abstract def getMax: T = {
    val result = super.getMax
    println(result)
    result
  }
}

object IntStepicHeap {

  def buildMaxHeap(heap: ArrayBuffer[Int]): StepicHeap[Int] = {
    val intStepicHeap = new StepicHeapWithCmdCounter()(Ordering.Int)
    heap.foreach(intStepicHeap.insert)
    intStepicHeap
  }

  def buildMinHeap(heap: ArrayBuffer[Int]): StepicHeapWithCmdCounter = {
    val intStepicHeap = new StepicHeapWithCmdCounter()(Ordering.Int.reverse) with LoggableStepikHeap[Int]
    heap.foreach(intStepicHeap.insert)
    intStepicHeap
  }
}

object StepicHeapTestApp {
  def main(args: Array[String]): Unit = {
    val da = IntStepicHeap.buildMinHeap(ArrayBuffer.empty[Int])

    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    var cmdCount = br.readLine().toInt
    var cmdLine  = br.readLine()

    while (cmdCount > 0) {
      val cmdValue     = cmdLine.split(" ")
      val (cmd, value) = (cmdValue.head, cmdValue.lastOption)

      cmd match {
        case "insert" =>
          da.insert(value.get.toInt): Unit
        case "extractMin" =>
          da.extractMax(): Unit
        case "getMin" =>
          da.getMax: Unit
        case "decreaseKey" =>
          da.decreaseKey(cmdValue(1).toInt, cmdValue.last.toInt): Unit
        case _ =>
          throw new IllegalArgumentException(s"Can't parse $cmdLine")
      }

      cmdLine = br.readLine()
      cmdCount -= 1
    }
    sys.exit(0)
  }
}

