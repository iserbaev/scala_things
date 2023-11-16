package hadoop.count

object Mapper {

  @scala.annotation.tailrec
  def processLine(m: Map[String, Int]): Map[String, Int] = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) return m
    val resultAcc = line.split(" ").foldLeft(m) { case (acc, k) =>
      acc.updated(k, acc.get(k).map(_ + 1).getOrElse(1))
    }
    processLine(resultAcc)
  }

  def main(args: Array[String]): Unit = {
    val kv       = Map.empty[String, Int]
    val resultKV = processLine(kv)
    resultKV.foreach { case (k, v) => println(k ++ s"\t${v.toString}") }
  }
}
