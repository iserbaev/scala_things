object Main {
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
  def test() = {
    def check(array: Array[Int]) = {
      val defaultSorted = array.clone().sorted
      val sorted = countSort(array,array.min, array.max)
      println(sorted.mkString(" "))

      assert(sorted sameElements defaultSorted, s"sorted elements not same defaultSorted \n ${sorted.mkString(" ")}")
    }

    check(Array(2,2,2,5,5,5,5,5,1))
    check(Array(2,3,9,2,9))
    "success"
  }
}
Main.test()