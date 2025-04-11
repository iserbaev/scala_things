package tasks

import java.io.{ BufferedReader, InputStreamReader }

trait DynamicArray[T] {
  def pushBack(x: T): Unit
  def popBack(): Unit
  def get(index: Int): T
  def size: Int
}

trait LoggableDynamicArray[T] extends DynamicArray[T] {
  abstract override def pushBack(x: T): Unit = {
    super.pushBack(x)
    println("Элемент добавлен")
  }

  abstract override def popBack(): Unit = {
    super.popBack()
    println("Последний элемент удалён")
  }

  abstract override def get(index: Int): T = {
    val element = super.get(index)
    println(element): Unit
    element
  }

  abstract override def size: Int = {
    val sze = super.size
    println(sze)
    sze
  }
}

class IntDynamicArray(bucket: Int) extends DynamicArray[Int] {
  private[this] var array       = newBucket
  private[this] var filledIndex = -1

  private def newBucket = Array.ofDim[Int](bucket)

  override def pushBack(x: Int): Unit = {
    filledIndex += 1
    if (filledIndex >= array.length) array = array ++ newBucket
    array.update(filledIndex, x)
  }

  override def popBack(): Unit = {
    array = array.dropRight(1)
    filledIndex -= 1
  }

  override def get(index: Int): Int =
    array(index)

  override def size: Int =
    filledIndex + 1
}

object DynamicArrayTestApp {
  def main(args: Array[String]): Unit = {
    val da = new IntDynamicArray(2) with LoggableDynamicArray[Int] {}

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
        case "get"       =>
          da.get(value.get.toInt): Unit
        case "size"      =>
          da.size: Unit
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
