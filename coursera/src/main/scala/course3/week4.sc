import java.util.concurrent.ForkJoinPool

import org.scalameter.{Key, Warmer, config}

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.{Combiner, ForkJoinTaskSupport}
import scala.reflect.ClassTag

/**
trait Builder[T, Repr] {
  def +=(elem: T): this.type
  def result: Repr
}

trait Combiner[T, Repr] extends Builder[T, Repr] {
  def combine(that: Combiner[T, Repr]): Combiner[T, Repr]
}

Typically, set data structures have efficient lookup, insertion and deletion.
  ▶ hash tables – expected O(1) ▶ balanced trees – O(log n)
  ▶ linked lists – O(n)
Most set implementations do not have efficient union operation.


Operation complexity for sequences can vary.
  ▶ mutable linked lists – O(1) prepend and append, O(n) insertion
  ▶ functional (cons) lists – O(1) prepend operations, everything else O(n)
  ▶ array lists – amortized O(1) append, O(1) random accesss, otherwise O(n)
Mutable linked list can have O(1) concatenation, but for most sequences, concatenation is O(n).

  */

/**
  * Parallel data structure using Two-Phase Construction
  *
  * The intermediate data structure is a data structure that:
  *  ▶ has an efficient combine method – O(log n + log m) or better
  *  ▶ has an efficient += method
  *  ▶ can be converted to the resulting data structure in O(n/P) time
  */

/**
  * Example: Array Combiner
  */

