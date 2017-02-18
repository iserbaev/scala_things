package course2

/**
  * Created by ilnur on 17.02.17.
  */
object MainWeek2 extends App{
  /**
    * lecture 2.1 Structural induction of trees
    */
  abstract class IntSet{
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
  }
  object Empty extends IntSet{
    def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
    def contains(x: Int): Boolean = false
    def union(other: IntSet): IntSet = other
  }
  case class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet{
    def incl(x: Int): IntSet = {
      if (x < elem) NonEmpty(elem, left incl x, right)
      else if (x > elem) NonEmpty(elem, left, right incl x)
      else this
    }

    def contains(x: Int): Boolean = {
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true
    }

    def union(other: IntSet): IntSet = (left union (right union (other))) incl elem
  }

  /**
    * Laws of IntSet
    *
    * 1. Empty contains x = false
    * 2. (s incl x) contains x = true
    * 3. (s incl x) contains y = s contains y (if x != y)
    * 4. (xs union ys) contains x = xs contains x || ys contains x
    */

  /**
    * lecture 2.2 Streams
    */
  def isPrime(n: Int): Boolean =
    (2 until n).forall(x => n%x != 0)

  ((1000 to 10000) filter isPrime)(1) //answer is 1013, but it unefficient - because calc all range

  val xs = Stream.cons(1, Stream.cons(2, Stream.empty))
  val ys = Stream(1,2,3)
  val zs = (1 to 1000).toStream

  /**
    * example - stream and list
    * for function streamRange(1,10) and listRange(1,10)
    * stream construct only first element, however list construct all element
    */
  def streamRange(lo: Int, hi: Int): Stream[Int] = {
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo+1, hi))
  }

  def listRange(lo: Int, hi: Int): List[Int] = {
    if (lo >= hi) Nil
    else lo :: listRange(lo+1, hi)
  }

  ((1000 to 10000).toStream filter isPrime)(1) //efficient because calc only needed range

  val s = 1
  val ss = Stream.cons(2, Stream.cons(3, Stream.empty))
  val newStream = s #:: ss // #:: - construct the Stream

  streamRange(1,10).take(3) //will init stream with only 3 digit
}
