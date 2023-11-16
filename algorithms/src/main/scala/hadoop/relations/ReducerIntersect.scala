package hadoop.relations

object ReducerIntersect {

  @scala.annotation.tailrec
  def processLine(acc: Map[String, Set[String]]): Map[String, Set[String]] = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) return acc
    val Array(value, setTag) = line.split("\t")

    processLine(acc.updated(value, acc.getOrElse(value, Set.empty) + setTag))
  }

  def main(args: Array[String]): Unit =
    processLine(Map.empty).foreach{ case (str, values) =>
      if (values.size == 2) println(str)
    }
}
