package B_gcd

object GCD {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    val ar = br.readLine().split(" ").map(_.toLong)
    val a = ar.head
    val b = ar.last
    val result = gcd(a,b)
    println(result)
  }

  @scala.annotation.tailrec
  def gcd(a: Long, b: Long): Long = (a, b) match {
    case (0, _) => b
    case (_, 0) => a
    case (aa, bb) if aa >= bb => gcd(a % b, b)
    case (_, _) => gcd(a, b % a)
  }
}

object LCM {
  def main(args: Array[String]): Unit = {
    val br: java.io.BufferedReader = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in)
    )

    val ar = br.readLine().split(" ").map(_.toLong)
    val a = ar.head
    val b = ar.last
    val result = lcm(a,b)
    println(result)
  }

  def lcm(a: Long, b: Long): Long =
    math.abs(a) * (math.abs(b) / gcd(a, b))

  @scala.annotation.tailrec
  def gcd(a: Long, b: Long): Long = (a, b) match {
    case (0, _) => b
    case (_, 0) => a
    case (aa, bb) if aa >= bb => gcd(a % b, b)
    case (_, _) => gcd(a, b % a)
  }
}