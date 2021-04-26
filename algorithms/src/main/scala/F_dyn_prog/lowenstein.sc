object Main {
  def min(i1: Int, i2: Int, i3: Int): Int = scala.math.min(i1, scala.math.min(i2,i3))
  def lowenstein(s1: String, s2: String): Int = {
    val n = s1.length
    val m = s2.length
    val matrix = Array.ofDim[Int](n + 1,m + 1)

    for {
      i <- 1 to n
      j <- 1 to m
    } yield {
      matrix(i)(j) = min(
        matrix(i-1)(j) + 1,
        matrix(i)(j-1) + 1,
        matrix(i-1)(j-1) + (if (s1(i-1) == s2(j-1)) 0 else 1)
      )
    }

    matrix(n)(m)
  }
  def main(args: Array[String]): Unit = {
    val s1 = scala.io.StdIn.readLine()
    val s2 = scala.io.StdIn.readLine()

    println(lowenstein(s1,s2))
  }

  def testOne(s1: String,s2:String, expected: Int) = {
    val r = lowenstein(s1,s2)
    println(s"test s1=$s1, s2=$s2, result=$r")
    assert(r == expected, s"lowenstein(s1=$s1, s2=$s2) = $r != $expected")
  }

  def test() = {
    testOne("ab","ab", 0)
    testOne("short","ports",3)
  }
}
Main.test()