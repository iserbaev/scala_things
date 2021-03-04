import scala.io.StdIn
object Main {
  def main(args: Array[String]): Unit = {
    val sum = StdIn.readLong()

    val x = math.floor((math.sqrt(1 + 8 * sum) - 1) / 2) - 1
    val n = math.floor((-1 + math.sqrt((1 + 8 * sum))) / 2).toInt - 1
    val range = (1 to n).toSeq

    println(n)
    println(range.mkString(" "))
  }
}