package hadoop.relations

object ReducerLeftJoinOnly {

  @scala.annotation.tailrec
  def processLine(acc: Map[String, Set[String]]): Map[String, Set[String]] = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) return acc
    val arr = line.split("\t").filterNot(_.isEmpty)

    processLine(
      acc.updated(arr.head, acc.getOrElse(arr.head, Set.empty) + arr.last)
    )
  }

  def main(args: Array[String]): Unit =
    processLine(Map.empty[String, Set[String]])
      .toList.sortBy(_._1)
      .foreach { case (str, values) =>
        if (values.size == 1 && values.head == "A") println(str)
      }
}
