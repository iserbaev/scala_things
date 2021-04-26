object Main {
  private implicit class ArrayOps(val a: Array[Int])  {
    def fromIndex(i: Int): Int = a(i - 1)
  }
  def countSort(input: Array[Int], min: Int, max: Int): Array[Int] =
    input.foldLeft(Array.fill(max - min + 1)(0)) { (arr, n) =>
      arr(n - min) += 1
      arr
    }.zipWithIndex.reverse.foldLeft(List[Int]()) {
      case (lst, (cnt, ndx)) => List.fill(cnt)(ndx + min) ::: lst
    }.toArray
  def main(args: Array[String]): Unit = {
    val length = scala.io.StdIn.readLine().toInt
    val array = scala.io.StdIn.readLine().split(" ").tail.map(_.toInt)
    assert(array.length == length)

    println(countSort(array,array.min,array.max).mkString(" "))
  }
  def countSort2(a: Array[Int],max: Int) = {
    val b = new Array[Int](a.length)
    val c = new Array[Int](max + 1)
    (1 to a.length).foreach(j => {
      c(a.fromIndex(j)) = c(a.fromIndex(j)) + 1
    })
    (1 to max).foreach(i => {
      c(i) = c(i) + c(i - 1)
    })
    (a.length to 1 by -1).foreach(j => {
      b(c(a.fromIndex(j)) - 1) = a.fromIndex(j)
      c(a.fromIndex(j)) = c(a.fromIndex(j)) - 1
    })
    b
  }
  def test() = {
    def check(array: Array[Int]) = {
      val defaultSorted = array.clone().sorted
      val sorted = countSort(array,array.min, array.max)
      val sorted2 = countSort2(array, array.max)
      println(sorted.mkString(" "))
      println(sorted2.mkString(" "))

      assert(sorted sameElements defaultSorted, s"sorted elements not same defaultSorted \n ${sorted.mkString(" ")}")
      assert(sorted2 sameElements defaultSorted, s"sorted elements not same defaultSorted \n ${sorted.mkString(" ")}")
    }

    check(Array(2,2,2,5,5,5,5,5,1))
    check(Array(2,3,9,2,9))
    "success"
  }
}
Main.test()