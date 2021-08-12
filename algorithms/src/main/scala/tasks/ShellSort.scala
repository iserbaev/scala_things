package tasks

object ShellSort {
  def incSeq(len: Int) = new Iterator[Int] {
    private[this] var x: Int = len / 2
    def hasNext = x > 0
    def next()  = { x = if (x == 2) 1 else x * 5 / 11; x }
  }

  def InsertionSort(a: Array[Int], inc: Int) =
    for (i <- inc until a.length; temp = a(i)) {
      var j = i;
      while (j >= inc && a(j - inc) > temp) {
        a(j) = a(j - inc)
        j    = j - inc
      }
      a(j) = temp
      println(a.toSeq)
    }

  def shellSort(a: Array[Int]) =
    for (inc <- incSeq(a.length)) InsertionSort(a, inc)

  def main(args: Array[String]): Unit = {
    val a = Array(54, 98, 67, 11, 23, 4, 1, 4, 1, -7)
    println(a.mkString(","))
    shellSort(a)
    println(a.mkString(","))
  }
}
