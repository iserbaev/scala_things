package hadoop.count

object Reducer {

  def processLine(): Unit =
    recur("", 0, 0)

  @scala.annotation.tailrec
  private def recur(previousLetter: String, sum: Integer, count: Integer): Unit = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) {
      println(previousLetter + "\t" + sum / count)
      return
    }
    val data = line.split("\t")

    if (previousLetter == "") {
      recur(data(0), data(1).toInt + sum, count + 1)
    } else if (data(0) != previousLetter) {
      println(previousLetter + "\t" + sum / count)
      recur(data(0), data(1).toInt, 1)
    } else if (data(0) == previousLetter) {
      recur(data(0), data(1).toInt + sum, count + 1)
    }
  }

  def main(args: Array[String]): Unit =
    processLine()
}
