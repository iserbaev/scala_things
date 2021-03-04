object Main {
  def knapsack(W: Int, n: Int, wn: Array[Int], cn: Array[Int]) = {
    val matrix = Array.ofDim[Int](W + 1,n + 1)

    for {
      i <- 1 to n
      w <- 1 to W
    } yield {
      matrix(w)(i) = matrix(w)(i-1)
      val wi = wn(i - 1)
      val ci = cn(i - 1)
      if (wi <= w) {
        matrix(w)(i) = scala.math.max(matrix(w)(i-1), matrix(w-wi)(i-1) + ci)
      }
    }

    matrix.map(_.toList).toList
  }
  def main(args: Array[String]): Unit = {
    val arr = scala.io.StdIn.readLine().split(" ").map(_.toInt)
    val W = arr.head
    val n = arr.last
    val wn = scala.io.StdIn.readLine().split(" ").map(_.toInt)
    val cn = wn

    val D = knapsack(W,n,wn,cn)
    println(D(W)(n))
  }

  def testOne(W: Int, n: Int, wn: Array[Int], cn: Array[Int], expected: Int) = {
    val D = knapsack(W,n,wn,cn)
    println(D)
    assert(D(W)(n) == expected)
  }

  def test() = {
    testOne(10,3,Array(1,4,8), Array(1,4,8),9)
  }
}
Main.test()