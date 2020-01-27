object Main {
  def gcd(a: Long, b: Long): Long = (a, b) match {
    case (0, _)                   => b
    case (_, 0)                   => a
    case (aa, bb) if aa >= bb     => gcd(a % b, b)
    case (aaa, bbb) if aaa <= bbb => gcd(a, b % a)
  }
  def main(args: Array[String]): Unit = {
    val arr    = readLine().split(" ")
    val (a, b) = (arr.head.toLong, arr.tail.head.toLong)
    val result = gcd(a, b)
    println(result)
  }
}
