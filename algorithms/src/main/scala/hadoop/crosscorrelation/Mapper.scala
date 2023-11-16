package hadoop.crosscorrelation

object Mapper {

  @scala.annotation.tailrec
  def processLine(): Unit = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) {
      return ()
    }
    val arr = line.split(" ")

    arr.foreach { i =>
      arr.foreach { j =>
        if (i != j) println(s"$i,$j\t1")
      }

    }
    processLine()
  }

  def main(args: Array[String]): Unit =
    processLine()
}
