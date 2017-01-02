def nth[T](index:Int, xs:List[T]):T = {
  if (xs.isEmpty) throw new IndexOutOfBoundsException
  else if (index == 0) xs.head
  else nth(index-1, xs.tail)
  }

val list = new ConsList(1, new ConsList(2, new ConsList(3, new Nil)))
nth(2, list)
nth(3, list)


trait List[+T]{
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
}