package hadoop.relations

object ReducerUnion {

  @scala.annotation.tailrec
  def processLine(prev: Option[Int]): Unit = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) return ()
    val Array(value, _) = line.split("\t")

    if (prev.contains(value.toInt)) processLine(prev)
    else {
      println(value)
      processLine(Some(value.toInt))
    }
  }

  def main(args: Array[String]): Unit =
    processLine(None)
}
