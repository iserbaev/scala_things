package hadoop.crosscorrelation

import scala.collection.immutable.ListMap

object MapperStripe {

  @scala.annotation.tailrec
  def processLine(): Unit = {
    val line = scala.io.StdIn.readLine()
    if (line == null || line.isEmpty) {
      return ()
    }
    val arr = line.split(" ")

    arr.foreach { i =>
      var stripes = ListMap.empty[String, Int]
      arr.foreach { j =>
        if(i != j) {
          stripes = stripes.updated(j, stripes.getOrElse(j, 0) + 1)
        }
      }

      println(s"$i\t${stripes.map { case (k, v) => s"$k:$v" }.mkString(",")}")
    }
    processLine()
  }

  def main(args: Array[String]): Unit =
    processLine()
}
