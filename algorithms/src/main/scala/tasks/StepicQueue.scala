package tasks

import java.io.{BufferedReader, InputStreamReader}
import scala.collection.mutable

trait StepicQueue[T] {
  def enqueue(x: T): Unit
  def dequeue: T
  def isEmpty: Boolean
}

trait LoggableStepicQueue[T] extends StepicQueue[T] {
  abstract override def enqueue(x: T): Unit = {
    super.enqueue(x)
    println("Элемент добавлен")
  }

  abstract override def dequeue: T = {
    val result = super.dequeue
    println(result)
    result
  }

  abstract override def isEmpty: Boolean = {
    val result = super.isEmpty
    val logMsg = if (result) "Да" else "Нет"
    println(logMsg)
    result
  }
}

class IntStepicQueue extends StepicQueue[Int] {
  private[this] var mutableSeq = mutable.ListBuffer.empty[Int]

  override def enqueue(x: Int): Unit = {
    mutableSeq += x
  }

  override def dequeue: Int = {
    val result = mutableSeq.head
    mutableSeq = mutableSeq.drop(1)
    result
  }

  override def isEmpty: Boolean = {
    mutableSeq.isEmpty
  }
}

object StepicQueueTestApp {
  def main(args: Array[String]): Unit = {
    val da = new IntStepicQueue with LoggableStepicQueue[Int] {}

    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    var cmdLine = br.readLine()

    while (cmdLine != "exit") {
      val cmdValue     = cmdLine.split(" ")
      val (cmd, value) = (cmdValue.head, cmdValue.lastOption)

      cmd match {
        case "enqueue" =>
          da.enqueue(value.get.toInt): Unit
        case "dequeue"       =>
          da.dequeue: Unit
        case "is_empty"  =>
          da.isEmpty: Unit
        case _ =>
          throw new IllegalArgumentException(s"Can't parse $cmdLine")
      }

      cmdLine = br.readLine()
    }
    sys.exit(0)
  }
}
