package tasks

import java.io.{BufferedReader, InputStreamReader}

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

class IntSinglyLinkedList(bucket: Int) extends SinglyLinkedList[Int] {
  private[this] var array = newBucket
  private[this] var filledIndex = -1

  private def newBucket = Array.ofDim[Int](bucket)

  override def pushFront(x: Int): Unit = {
    filledIndex = filledIndex + 1
    if (filledIndex >= array.length) array = array ++ newBucket
    array.update(filledIndex, x)
  }

  override def popFront(): Unit = {
    array = array.dropRight(1)
    filledIndex = filledIndex - 1
  }

  override def find(x: Int): Boolean = {
    array.contains(x)
  }
}

object SinglyLinkedListTestApp {
  def main(args: Array[String]): Unit = {
    val da = new IntSinglyLinkedList(1) with LoggableSinglyLinkedList[Int] {}

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
