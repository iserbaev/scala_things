package hadoop.graphs

object MapperBFS {

  @scala.annotation.tailrec
  def processLine(): Unit = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) return ()
    println(line) // forward to reducer for reconstructing graph
    val List(_, distance, adjacentsRaw) = line.split("\t", 3).toList
    val newD = if (!distance.contains("INF")) (distance.trim.toInt + 1).toString else distance
    adjacentsRaw
      .replaceAll("\\{", "")
      .replaceAll("}", "")
      .split(",")
      .foreach { a =>
        if (a.trim.nonEmpty) println(a ++ "\t" ++ newD ++ "\t" ++ "{}")
      }

    processLine()
  }

  def main(args: Array[String]): Unit =
    processLine()
}
