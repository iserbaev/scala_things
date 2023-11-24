package structures

import scala.collection.mutable.ArrayBuffer

case class MaxWindow(windowSize: Int) {
  private val input  = MaxStack(windowSize)
  private val output = MaxStack(windowSize)
  private val buf    = ArrayBuffer[Int]()

  def add(e: Int): MaxWindow = {
    input.push(e)
    if (input.isFull && output.isEmpty) {
      while (input.nonEmpty) {
        output.push(input.pop()._1)
      }
    }

    if (input.length + output.length == windowSize) {
      buf.append(
        math.max(
          input.topMaxOption.getOrElse(Int.MinValue),
          output.topMaxOption.getOrElse(Int.MinValue)
        )
      )
      output.popSafely: Unit
    }

    this
  }

  def result: Seq[Int] = buf.toSeq
}

object MaxWindow {
  def apply(n: Int, a: Array[Int], m: Int): Seq[Int] =
    if (a.length <= m || m == n)
      Seq(a.max)
    else
      a.foldLeft(MaxWindow(m)) { case (acc, e) =>
        acc.add(e)
      }.result
}
