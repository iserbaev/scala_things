package hadoop.distinct

object Mapper2 {

  @scala.annotation.tailrec
  def processLine(m: Vector[(String, Int)]): Vector[(String, Int)] = {
    val line = scala.io.StdIn.readLine()

    if (line == null || line.isEmpty) return m

    val Array(_, group) = line.split(",")

    processLine(m :+ ((group,1)))
  }

  def main(args: Array[String]): Unit = {
    val result = processLine(Vector.empty[(String, Int)])

    result.foreach { case (k, v) => println(k ++ s"\t${v.toString}") }
  }

}
