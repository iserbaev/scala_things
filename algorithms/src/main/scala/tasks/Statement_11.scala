package tasks

import structures._

import java.io.{ BufferedReader, InputStreamReader }
object Main11 {
  def main(args: Array[String]) = {
    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    val m = br.readLine().toInt
    val n = br.readLine().toInt

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

  val multiplier: BigInt = BigInt(263)
  val prime: Int         = 1000000007
  def hash(a: String, m: Int = 5): Int =
    ((a.toCharArray.zipWithIndex.map { case (c, index) =>
      (c.toInt * (multiplier.pow(index))) % prime
    }.sum % prime) % m).toInt

  require(hash("qaxndhusptgrewo", m = 25) == 7)
  require(hash("uiljkwhypgmfdst", m = 250) == 72)
  require(hash("pweiknqgcxazjyh", m = 2500) == 263)
  require(hash("ilvpygszwdeurjn", m = 25000) == 9134)
  require(hash("xnzrvwcutfgbqje", m = 250000) == 170995)
  require(hash("rpqlfogsamhjkic", m = 2500000) == 954764)
  require(hash("zhjutqslrpyfcoa", m = 25000000) == 9581368)
  require(hash("kxlcfgpezjmuynv", m = 250000000) == 127141737)
}
