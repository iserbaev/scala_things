package tasks_hostmann.ch10

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer

/**
  * Created by ilnur on 26.11.16.
  */
class BitSequence extends Seq[Long] {
  private val arr = ArrayBuffer[Long]()
  override def length: Int = arr.length
  override def apply(idx: Int): Long = arr(idx)
  override def iterator:   Iterator[Long] = arr.iterator
  override def toString(): String         = this.mkString("BitSequence(", ", ", ")")
  def update(index: Int, x: Long): BitSequence = {
    this.arr(index) = x
    this
  }
}

object BitSequence {
  def apply(xs: Long*): BitSequence = {
    val bs = new BitSequence
    xs.foreach(bs.arr.append(_))
    bs
  }
}
