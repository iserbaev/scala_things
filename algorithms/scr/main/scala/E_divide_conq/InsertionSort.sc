object InsertionSort {
  implicit private class ArrayOps(val a: Array[Int]) extends AnyVal {
    def fromIndex(i: Int): Int = a(i - 1)
  }
  def insertionSort(a: Array[Int]) = {
    def recur(i: Int, key: Int): Int =
      if (i > 0 && a.fromIndex(i) > key) {
        a.update(i, a.fromIndex(i))
        recur(i - 1, key)
      } else i
    (2 to a.length).foreach(j => {
      val key = a.fromIndex(j)
      val i = recur(j - 1, key)
      a.update(i,key)
    })
  }
}
val a = Array(2,8,7,1,3,5,6,4)
InsertionSort.insertionSort(a)
a
