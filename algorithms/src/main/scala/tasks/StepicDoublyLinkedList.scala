package tasks

import java.io.{BufferedReader, InputStreamReader}
import scala.collection.mutable

trait StepicDoublyLinkedList[T] {
  def pushBack(x: T): Unit
  def popBack(): Unit
  def printReverse(): Unit
}

trait LoggableStepicDoublyLinkedList[T] extends StepicDoublyLinkedList[T] {
  abstract override def pushBack(x: T): Unit = {
    super.pushBack(x)
    println("Элемент добавлен")
  }

  abstract override def popBack(): Unit = {
    super.popBack()
    println("Последний элемент удалён")
  }

  abstract override def printReverse(): Unit = {
    super.printReverse()
  }
}

class IntStepicDoublyLinkedList extends StepicDoublyLinkedList[Int] {
  private[this] var mutableSeq = mutable.ListBuffer.empty[Int]

  override def pushBack(x: Int): Unit =
    mutableSeq += x: Unit

  override def popBack(): Unit = {
    mutableSeq = mutableSeq.dropRight(1)
  }

  override def printReverse(): Unit = {
    println(mutableSeq.reverse.mkString(" "))
  }
}

object StepicDoublyLinkedListTestApp {
  def main(args: Array[String]): Unit = {
    val da = new IntStepicDoublyLinkedList with LoggableStepicDoublyLinkedList[Int] {}

    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    var cmdLine = br.readLine()

    while (cmdLine != "exit") {
      val cmdValue     = cmdLine.split(" ")
      val (cmd, value) = (cmdValue.head, cmdValue.lastOption)

      cmd match {
        case "push_back" =>
          da.pushBack(value.get.toInt): Unit
        case "print_reverse"       =>
          da.printReverse(): Unit
        case "pop_back"  =>
          da.popBack(): Unit
        case _ =>
          throw new IllegalArgumentException(s"Can't parse $cmdLine")
      }

      cmdLine = br.readLine()
    }
    sys.exit(0)
  }
}
