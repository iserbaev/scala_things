package structures

import scala.collection.mutable

// Hash table
// Collision resolution: Separate chaining with linked lists
trait TC[A] {
  def hash(a: A): Int
  def add(a: A): Unit
  def del(a: A): Unit
  def find(a: A): Boolean
  def check(i: Int): Iterable[A]

  def checkMkString(i: Int): String =
    check(i).mkString(" ")
}

case class TC_String(m: Int) extends TC[String] {

  private val multiplier: BigInt = BigInt(263)
  private val prime: Int         = 1000000007

  private val underlying: mutable.IndexedSeq[(List[String], Set[String])] =
    scala.collection.mutable.IndexedSeq.fill(m)((List.empty, Set.empty))

  private val multipliers: IndexedSeq[BigInt] =
    (0 until 16).map(index => multiplier.pow(index))

  def hash(a: String): Int =
    ((a.toCharArray.zipWithIndex.map { case (c, index) =>
      (c.toInt * multipliers.applyOrElse(index, multiplier.pow))
    }.sum % prime) % m).toInt

  def add(a: String): Unit = {
    val idx    = hash(a)
    val (l, s) = underlying(idx)
    if (s.contains(a)) ()
    else {
      underlying.update(
        idx,
        (a :: l, s + a)
      )
    }
  }

  def del(a: String): Unit = {
    val idx    = hash(a)
    val (l, s) = underlying(idx)
    if (s.contains(a)) {
      underlying.update(idx, (rm(l, a), s - a))
    }
  }

  private def rm[A](xs: List[A], value: A): List[A] = xs match {
    case `value` :: tail => tail
    case x :: tail       => x :: rm(tail, value)
    case _               => Nil
  }

  def find(a: String): Boolean = {
    val idx = hash(a)
    underlying(idx)._2.contains(a)
  }

  def check(i: Int): Iterable[String] =
    underlying(i)._1
}
