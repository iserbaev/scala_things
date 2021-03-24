package tasks

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.collection.mutable.{ArrayStack => ScalaStack}

object Statement_5 {
  case class MaxStack(size: Int) {
    private val underlying = ScalaStack[(Int, Int)]()

    def length: Int = underlying.length
    def apply(i: Int): (Int, Int) = underlying(i)

    def isFull:  Boolean = underlying.size >= size
    def nonFull: Boolean = !isFull

    def isEmpty:  Boolean = underlying.isEmpty
    def nonEmpty: Boolean = !isEmpty

    def clear(): Unit = underlying.clear()

    def push(e: Int): Unit = {
      if (isFull) throw new IndexOutOfBoundsException("MaxStack is full")
      val max = if (isEmpty) e else math.max(e, underlying.top._2)
      underlying.push((e, max))
    }

    def pop(): (Int, Int) = underlying.pop()

    def topMaxOption: Option[Int] =
      if (isEmpty) None else Some(underlying.top._2)

    def popSafely: Option[(Int, Int)] =
      if (isEmpty) None else Some(underlying.pop)

    def result: Seq[(Int, Int)] = underlying
  }

  case class MaxWindow(windowSize: Int) {
    private val input  = MaxStack(windowSize)
    private val output = MaxStack(windowSize)
    private val buf    = ArrayBuffer[Int]()

    def add(e: Int): MaxWindow = {
      input.push(e)
      if (input.isFull && output.isEmpty) {
        while (input.nonEmpty) {
          output.push(input.pop()._1)
        }
      }

      if (input.length + output.length == windowSize) {
        buf.append(
          math.max(
            input.topMaxOption.getOrElse(Int.MinValue),
            output.topMaxOption.getOrElse(Int.MinValue)
          )
        )
        output.popSafely
      }

      this
    }

    def printResult(): Unit =
      if (buf.length < windowSize) println(buf.last)
      else {
        print(result.mkString(" "))
      }

    def result: Seq[Int] = buf
  }

  def maxWindow(n: Int, a: Array[Int], m: Int): MaxWindow =
    if (a.length <= m || m == n)
      MaxWindow(m).add(a.max)
    else
      a.foldLeft(MaxWindow(m)) {
        case (acc, e) => acc.add(e)
      }

  def main(args: Array[String]): Unit = {
    val n  = scala.io.StdIn.readInt()
    val ar = scala.io.StdIn.readLine().split(" ").map(_.toInt)
    val m  = scala.io.StdIn.readInt()

    val res = maxWindow(n, ar, m)
    res.printResult()
  }

}

object TestApp5 extends App {
  def test(a: String, m: Int, expected: String): Unit = {
    val aa = a.split(" ").map(_.toInt)
    val result: String = t(m, aa)
    if (result != expected) println(s"($result) != ($expected)")
  }

  private def t(m: Int, aa: Array[Int]) = {
    val before = System.currentTimeMillis()
    val result =
      Statement_5.maxWindow(aa.length, aa, m).result.mkString(" ")

    val after = System.currentTimeMillis()
    println(s"Time = ${after - before} MS, n = ${aa.length}, m = $m")
    result
  }

  val beforeAll = System.currentTimeMillis()
  test("2 1 5", 1, "2 1 5")
  test("2 7 3 1 5 2 6 2", 4, "7 7 5 6 6")
  test(
    "73 65 24 14 44 20 65 97 27 6 42 1 6 41 16",
    7,
    "73 97 97 97 97 97 97 97 42"
  )
  test("28 7 64 40 68 86 80 93 4 53 32 56 68 18 59", 12, "93 93 93 93")
  test("16 79 20 19 43 72 78 33 40 52 70 79 66 43 60 ", 12, "79 79 79 79")

  test(
    "34 51 61 90 26 84 2 25 7 8 25 78 21 47 25",
    3,
    "61 90 90 90 84 84 25 25 25 78 78 78 47"
  )

  test(
    "27 83 29 77 6 3 48 2 16 72 46 38 55 2 58",
    5,
    "83 83 77 77 48 72 72 72 72 72 58"
  )

  test("1 4 5 6 1 1 1 1", 4, "6 6 6 6 1")
  test("1 4 5 6 1 1 1 1", 4, "6 6 6 6 1")

  val a = (1 to 100).map(_     => Random.nextInt()).toArray
  val b = (1 to 1000).map(_    => Random.nextInt()).toArray
  val c = (1 to 10000).map(_   => Random.nextInt()).toArray
  val d = (1 to 100000).map(_  => Random.nextInt()).toArray
  val e = (1 to 1000000).map(_ => Random.nextInt()).toArray

  val mm = (1 to 100).toArray

  mm.foreach { m =>
    t(m, a)
    t(m, b)
    t(m, c)
    t(m, d)
    t(m, e)
  }

  val afterAll = System.currentTimeMillis()

  println(s"All test timeline = ${afterAll - beforeAll} MS")
}
