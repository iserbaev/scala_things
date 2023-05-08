package course3

import common.TaskBuilder._

import scala.collection.parallel.Combiner
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import org.scalameter._

import java.util.concurrent.ForkJoinPool

class ArrayCombiner[T <: AnyRef: ClassTag](val parallelism: Int) extends Combiner[T, Array[T]] {
  private var numElems = 0
  private val buffers  = new ArrayBuffer[ArrayBuffer[T]]
  buffers += new ArrayBuffer[T]

  // Amortized O(1), low constant factors – as efficient as an array buffer.
  def +=(x: T) = {
    buffers.last += x
    numElems += 1
    this
  }

  // O(P), assuming that buffers contains no more than O(P) nested array buffers.
  override def combine[N <: T, NewTo >: Array[T]](
      that: Combiner[N, NewTo]
  ): Combiner[N, NewTo] =
    (that: @unchecked) match {
      case that: ArrayCombiner[T] =>
        buffers ++= that.buffers
        numElems += that.numElems
        this
    }

  override def size: Int = numElems

  override def clear(): Unit = buffers.clear()

  private def copyTo(array: Array[T], from: Int, end: Int) = {
    var i = from
    var j = 0
    while (i >= buffers(j).length) {
      i -= buffers(j).length
      j += 1
    }
    var k = from
    while (k < end) {
      array(k) = buffers(j)(i)
      i += 1
      if (i >= buffers(j).length) {
        i = 0
        j += 1
      }
      k += 1
    }
  }

  def result: Array[T] = {
    val array  = new Array[T](numElems)
    val step   = math.max(1, numElems / parallelism)
    val starts = (0 until numElems by step) :+ numElems
    val chunks = starts.zip(starts.tail)
    val tasks = for ((from, end) <- chunks) yield task {
      copyTo(array, from, end)
    }
    tasks.foreach(_.join())
    array
  }
}

object ArrayCombiner {
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns     -> 60,
    Key.verbose            -> true
  ).withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val size = 1000000

    def run(p: Int) = {
      val taskSupport = new collection.parallel.ForkJoinTaskSupport(
        new ForkJoinPool(p)
      )
      val strings = (0 until size).map(_.toString)
      val time = standardConfig.measure {
        val parallelized = strings.par
        parallelized.tasksupport = taskSupport
        def newCombiner = new ArrayCombiner(p): Combiner[String, Array[String]]
        parallelized.aggregate(newCombiner)(_ += _, _.combine(_)).result
      }
      println(s"p=$p, time=$time")
    }
    run(1)
    run(2)
    run(4)
    run(8)
  }

}
