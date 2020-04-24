object Main {
  def lisBottomUp3(a: Array[Int]): Int = {
    val n = a.length
    val d = new Array[Int](n)
    val prev = new Array[Int](n)

    (0 until n).foreach(i => {
      d(i) = 1
      prev(i) = -1

      (0 until i).foreach(j => {
        if ((a(i) % a(j) == 0) && (d(j) + 1) > d(i)) {
          d(i) = d(j) + 1
          prev(i) = j
        }
      })
    })
    d.max
  }
  def main(args: Array[String]): Unit = {
    scala.io.StdIn.readLine().toInt
    val array = scala.io.StdIn.readLine().split(" ").map(_.toInt)

    println(lisBottomUp3(array))
  }
  def test(): Unit = {
    val first  = Array(3,6,7,12)

    assert(lisBottomUp3(first) == 3, s"nvp(${first.mkString("["," ","]")})=${lisBottomUp3(first)} =! 3")
  }

}
Main.test()
