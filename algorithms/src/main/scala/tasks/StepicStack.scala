package tasks

import java.io.{ BufferedReader, InputStreamReader }
import scala.collection.mutable

trait StepicStack[T] {
  def push(x: T): Unit
  def pop(): T
  def top(): T
}

trait LoggableStepicStack[T] extends StepicStack[T] {
  override abstract def push(x: T): Unit = {
    super.push(x)
    println("Элемент добавлен")
  }

  override abstract def pop(): T = {
    val result = super.pop()
    println(result)
    result
  }

  override abstract def top(): T = {
    val result = super.top()
    println(result)
    result
  }
}

class IntStepicStack extends StepicStack[Int] {
  private[this] var mutableSeq = mutable.ListBuffer.empty[Int]

  override def push(x: Int): Unit =
    mutableSeq += x: Unit

  override def pop(): Int = {
    val result = mutableSeq.last
    mutableSeq = mutableSeq.dropRight(1)
    result
  }

  override def top(): Int =
    mutableSeq.last
}

object StepicStackTestApp {
  def main(args: Array[String]): Unit = {
    val da = new IntStepicStack with LoggableStepicStack[Int] {}

    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    var cmdLine = br.readLine()

    while (cmdLine != "exit") {
      val cmdValue     = cmdLine.split(" ")
      val (cmd, value) = (cmdValue.head, cmdValue.lastOption)

      cmd match {
        case "push" =>
          da.push(value.get.toInt): Unit
        case "pop" =>
          da.pop(): Unit
        case "top" =>
          da.top(): Unit
        case _ =>
          throw new IllegalArgumentException(s"Can't parse $cmdLine")
      }

      cmdLine = br.readLine()
    }
    sys.exit(0)
  }
}
