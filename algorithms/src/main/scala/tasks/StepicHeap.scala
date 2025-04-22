package tasks

import java.io.{ BufferedReader, InputStreamReader }
import scala.collection.mutable
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
    val e1 = heap(idx1)
    val e2 = heap(idx2)
    heap.update(idx1, e2)
    heap.update(idx2, e1)
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
    heap += x
    val idx = heap.size - 1
    heapifyUp(idx)
  }

  def extractMax(): T = {
    require(heap.nonEmpty, "Empty heap")

    val maxVal = heap(0)
    heap.remove(0): Unit
    heapifyDown(0): Unit
    maxVal
  }

  def getMax: T =
    heap(0)

}

class StepicHeapWithCmdCounter(implicit ordering: Ordering[Int]) extends StepicHeap[Int] {
  private var cmdCounter      = 0
  private val cmdCounterToIdx = mutable.Map.empty[Int, Int]

  override def insert(x: Int): Int = {
    val resultIdx = super.insert(x)
    cmdCounter += 1
    cmdCounterToIdx.update(cmdCounter, resultIdx)
    resultIdx
  }

  override def extractMax(): Int = {
    cmdCounter += 1
    super.extractMax()
  }

  override def getMax: Int = {
    cmdCounter += 1
    super.getMax
  }

  override protected def swap(idx1: Int, idx2: Int): Unit = {
    super.swap(idx1, idx2)
    cmdCounterToIdx.find(_._2 == idx1).foreach{ case (cmd1, _) =>
      cmdCounterToIdx.update(cmd1, idx2)
    }
    cmdCounterToIdx.find(_._2 == idx2).foreach{ case (cmd2, _) =>
      cmdCounterToIdx.update(cmd2, idx1)
    }
  }

  def decreaseKey(cmdCount: Int, decreaseValue: Int): Unit = {
    val idx = cmdCounterToIdx(cmdCount)
    val old = heap(idx)
    heap.update(idx, old - decreaseValue)
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

