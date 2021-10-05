package tasks

import tasks.Main11.TC_String

import java.io.{BufferedReader, InputStreamReader}
import scala.collection.mutable

// Hash table
// Collision resolution: Separate chaining with linked lists
object Main11 {

  trait TC[A] {
    def hash(a:  A):   Int
    def add(a:   A):   Unit
    def del(a:   A):   Unit
    def find(a:  A):   Boolean
    def check(i: Int): Iterable[A]

    def checkMkString(i: Int): String =
      check(i).mkString(" ")
  }

  case class TC_String(m: Int) extends TC[String] {

    private val multiplier: Double = 263d
    private val prime:      Int    = 1000000007

    private val underlying: mutable.IndexedSeq[mutable.Map[String, String]] =
      scala.collection.mutable.IndexedSeq.fill(m)(mutable.TreeMap.empty)

    //h(world) = ((119 + 111 × 263 + 114 × 2632 + 108 × 2633+ 100 × 2634) mod 1 000 000 007) mod 5 = 4
    def hash(a: String): Int =
      ((a.toCharArray.zipWithIndex.map {
        case (c, index) =>
          c.toInt * scala.math.pow(multiplier, index.toDouble)
      }.sum % prime) % m).toInt

    def add(a: String): Unit = {
      val idx = hash(a)
      underlying.update(idx, underlying(idx).updated(a, a))
    }

    def del(a: String): Unit = {
      val idx = hash(a)
      underlying.update(idx, underlying(idx) -= a)
    }

    def find(a: String): Boolean = {
      val idx = hash(a)
      underlying(idx).isDefinedAt(a)
    }

    def check(i: Int): Iterable[String] =
      underlying(i).values
  }

  def main(args: Array[String]) = {
    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    val n = br.read()
    val m = br.read()

    val cmds = (1 to n).flatMap(_ => Option(br.readLine()))

    val table = TC_String(m)

    process(cmds, table)

    br.close()
  }

  def process(arr: Seq[String], table: TC_String): Unit =
    arr.foreach { cmd =>
      cmd.split(" ") match {
        case Array("add", a) =>
          table.add(a)
        case Array("del", a) =>
          table.del(a)
        case Array("find", a) =>
          println(if (table.find(a)) "yes" else "no")
        case Array("check", a) =>
          println(table.checkMkString(a.toInt))
        case other =>
          println(s"Unknown command ${other.mkString(" ")}")
      }
    }

}

object Test11 extends App {
  val test_1 = Seq(
    "add world",
    "add HellO",
    "check 4",
    "find World",
    "find world",
    "del world",
    "check 4",
    "del HellO",
    "add luck",
    "add GooD",
    "check 2",
    "del good"
  )

  val test_1_OutputExpected =
    Seq(
      "HellO world",
      "no",
      "yes",
      "HellO",
      "GooD luck"
    )

  val test_2 = Seq(
    "add test",
    "add test",
    "find test",
    "del test",
    "find test",
    "find Test",
    "add Test",
    "find Test"
  )

  val test_2_output_expected = Seq(
    "yes",
    "no",
    "no",
    "yes"
  )

  val table = TC_String(5)
  Main11.process(test_1, table)

  println("------------")
  Main11.process(test_2, table)
  println("------------")
}
