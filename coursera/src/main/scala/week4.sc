def nth[T](index:Int, xs:List[T]):T = {
  if (xs.isEmpty) throw new IndexOutOfBoundsException
  else if (index == 0) xs.head
  else nth(index-1, xs.tail)
  }

val list = new ConsList(1, new ConsList(2, new ConsList(3, new Nil)))
nth(2, list)
nth(4, list)


trait List[T]{
  def isEmpty:Boolean
  def head:T
  def tail:List[T]
}

class ConsList[T] (val head: T, val tail: List[T]) extends List[T]{
  def isEmpty:Boolean = false
}
class Nil[T] extends List[T]{
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchElementException("Nil.head")
  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}