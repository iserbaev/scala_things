package hadoop.count

object Combiner {

  def processLine(): Unit = {
    recur("", 0, 0)
  }

  @scala.annotation.tailrec
  private def recur(previousLetter: String, sum: Integer, count: Integer): Unit = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) {
      println(previousLetter + "\t" + s"$sum;$count")
      return
    }
    val data = line.split("\t")
    val sumCount = data(1).split(";")

    if (previousLetter == "") {
      recur(data(0), sumCount.head.toInt, sumCount(1).toInt)
    } else if (data(0) != previousLetter) {
      println(previousLetter + "\t" + s"$sum;$count")
      recur(data(0), sumCount.head.toInt, sumCount(1).toInt)
    } else if (data(0) == previousLetter) {
      recur(data(0), sumCount.head.toInt + sum, sumCount(1).toInt + count)
    }
  }

  def main(args: Array[String]): Unit = {
    processLine()
  }
}