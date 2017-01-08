def nth[T](index:Int, xs:List[T]):T = {
  if (xs.isEmpty) throw new IndexOutOfBoundsException
  else if (index == 0) xs.head
  else nth(index-1, xs.tail)
  }

val list = new ConsList(1, new ConsList(2, new ConsList(3, new Nil)))
nth(2, list)
nth(3, list)


trait List[+T]{
  def ::[T](h: T): List[T] = {
    new ::(h, this)
  }

  def splitAt(n: Int): (List[T], List[T]) = {
    var b = List[T]
    var i = 0
    var these = this
    while (!these.isEmpty && i < n) {
      i += 1
      b = these.head :: b
      these = these.tail
    }
    (b, these)
  }

  def length:Int = {
    var these = this
    var len =0
    while (!these.isEmpty){
      len += 1
      these = these.tail
    }
    len
  }
  def isEmpty:Boolean
  def head:T
  def tail:List[T]
  def prepend[U >: T](elem: U): List[U] = new ConsList(elem, this)
}

class ConsList[T] (val head: T, val tail: List[T]) extends List[T]{
  def isEmpty:Boolean = false
}
class Nil[T] extends List[T]{
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchElementException("Nil.head")
  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}
class ::[T](h: T, list: List[T]) extends List[T]{
  override def isEmpty: Boolean = false
  override def head: T = h
  override def tail: List[T] = list
}
object List{
  def apply[T](): List[T] = new Nil
  def apply[T](x: T): List[T] = new ConsList[T](x, new Nil)
  def apply[T](x: T, y: T): List[T] = new ConsList[T](x, new ConsList[T](y, new Nil[T]))
  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => x :: Nil
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }

  /**
    *
    *week 5.1
    */
  def last[T](xs: List[T]):T = xs match {
    case List() => throw new Error("last of empty list")
    case List(x) => x
    case y :: ys => last(ys)
  }
  def init[T](xs: List[T]):List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) =>List()
    case y :: ys => y :: init(ys)
  }
  def concat[T](xs: List[T], ys: List[T]):List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }
  def reverse[T](xs: List[T]):List[T] = xs match {
    case List() => xs
    case y :: ys => reverse(ys) ++ List(y)
  }
  def removeAt[T](xs: List[T], n: Int): List[T] = (xs take n) ::: (xs drop n+1)
  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => xs
    case List(x) => x match {
      case `x` => x
      case y :: ys => y :: flatten(ys)
    }
    case z :: zs => z :: flatten(zs)
  }

  /**
    * week 5.2
    */
  import math.Ordering
  def msort[T](xs: List[T])(implicit ord : Ordering[T]): List[T] = {
    val n = xs.length/2
    if(n==0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = {
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y :: ys1) =>
              if (ord.lt(x,y)) x :: merge(xs1, ys)
              else y :: merge(xs, ys1)
        }
      }
      val (f,s) = xs splitAt n
      merge(msort(f), msort(s))
    }
  }
}