package object tasks {
  def arrayHeadTail[T](array: Array[T]): (T, Array[T]) = {
    require(array.nonEmpty, "Array is empty")

    (array.head, array.tail)
  }
}
