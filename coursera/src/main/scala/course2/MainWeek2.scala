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

}
