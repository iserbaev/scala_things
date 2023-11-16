package hadoop.relations

object MapperUrl {

  @scala.annotation.tailrec
  def processLine(): Unit = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) return ()
    val Array(_, _, url) = line.split("\t")

    println(url)

    processLine()
  }

  def main(args: Array[String]): Unit =
    processLine()
}
