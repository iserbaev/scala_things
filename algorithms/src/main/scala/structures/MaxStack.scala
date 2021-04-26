package structures

import scala.collection.mutable.{ArrayStack => ScalaStack}

case class MaxStack(size: Int) {
  private val underlying = ScalaStack[(Int, Int)]()

  def length: Int = underlying.length
  def apply(i: Int): (Int, Int) = underlying(i)

  def isFull:  Boolean = underlying.size >= size
  def nonFull: Boolean = !isFull

  def isEmpty:  Boolean = underlying.isEmpty
  def nonEmpty: Boolean = !isEmpty

  def clear(): Unit = underlying.clear()

  def push(e: Int): Unit = {
    if (isFull) throw new IndexOutOfBoundsException("MaxStack is full")
    val max = if (isEmpty) e else math.max(e, underlying.top._2)
    underlying.push((e, max))
  }

  def pop(): (Int, Int) = underlying.pop()

  def topMaxOption: Option[Int] =
    if (isEmpty) None else Some(underlying.top._2)

  def popSafely: Option[(Int, Int)] =
    if (isEmpty) None else Some(underlying.pop)

  def result: Seq[(Int, Int)] = underlying
}
