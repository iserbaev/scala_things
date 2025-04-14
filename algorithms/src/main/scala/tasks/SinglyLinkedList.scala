package tasks

import java.io.{BufferedReader, InputStreamReader}
import scala.collection.mutable

trait SinglyLinkedList[T] {
  def pushFront(x: T): Unit
  def popFront(): Unit
  def find(x: T): Boolean
}

trait LoggableSinglyLinkedList[T] extends SinglyLinkedList[T] {
  abstract override def pushFront(x: T): Unit = {
    super.pushFront(x)
    println("Элемент добавлен")
  }

  abstract override def popFront(): Unit = {
    super.popFront()
    println("Первый элемент удалён")
  }

  abstract override def find(x: T): Boolean = {
    val result = super.find(x)
    val logMsg = if (result) "Да" else "Нет"
    println(logMsg)
    result
  }
}

class IntSinglyLinkedList extends SinglyLinkedList[Int] {
  private[this] var mutableSeq = mutable.ListBuffer.empty[Int]

  override def pushFront(x: Int): Unit = {
    mutableSeq += x: Unit
  }

  override def popFront(): Unit = {
    mutableSeq = mutableSeq.dropRight(1)
  }

  override def find(x: Int): Boolean = {
    mutableSeq.contains(x)
  }
}

object SinglyLinkedListTestApp {
  def main(args: Array[String]): Unit = {
    val da = new IntSinglyLinkedList with LoggableSinglyLinkedList[Int] {}

    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    var cmdLine = br.readLine()

    while (cmdLine != "exit") {
      val cmdValue     = cmdLine.split(" ")
      val (cmd, value) = (cmdValue.head, cmdValue.lastOption)

      cmd match {
        case "push_front" =>
          da.pushFront(value.get.toInt): Unit
        case "find"       =>
          da.find(value.get.toInt): Unit
        case "pop_front"  =>
          da.popFront(): Unit
        case _ =>
          throw new IllegalArgumentException(s"Can't parse $cmdLine")
      }

      cmdLine = br.readLine()
    }
    sys.exit(0)
  }
}
