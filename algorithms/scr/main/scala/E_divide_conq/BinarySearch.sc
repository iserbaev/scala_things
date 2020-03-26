import scala.math

object Main {
  def bs(num: Int, array: Array[Int],l: Int, r: Int): Int = {
    val index = (l + r) / 2
    if (l > r || r < l) {
      -1
    } else {
      val am = array(index)
      if (am == num) {
        index
      } else if (am > num) {
        bs(num,array,l, index - 1)
      } else {
        bs(num,array,index + 1, r)
      }
    }
  }
  def main(args: Array[String]): Unit = {
    val first  = scala.io.StdIn.readLine().split(" ").tail.map(_.toInt)
    val second = scala.io.StdIn.readLine().split(" ").tail.map(_.toInt)

    val result = second.map(s => bs(s, first,0,first.length - 1)).map(i => if (i != -1) i + 1 else i)

    println(result.mkString(" "))
  }
  def test(): Unit = {
    val first  = Array(1,5,8,12,13)
    val second = Array(8,1,23,1,11)

    val result = second.map(s => bs(s, first,0,first.length - 1)).map(i => if (i != -1) i + 1 else i)
    println(result.mkString(" "))
  }
}
Main.test()
