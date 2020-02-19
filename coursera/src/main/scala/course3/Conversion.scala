package course3

import org.scalameter._

object Conversion {

  val standartConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 20,
    Key.exec.benchRuns     -> 20,
    Key.verbose            -> true
  ).withWarmer(new Warmer.Default)

  val memConfig = config(
    Key.exec.minWarmupRuns -> 0,
    Key.exec.maxWarmupRuns -> 0,
    Key.exec.benchRuns     -> 10,
    Key.verbose            -> true
  ).withWarmer(Warmer.Zero)

  val vector = Vector.fill(10000000)("")
  val list   = vector.toList

  def main(args: Array[String]): Unit = {
    val listtime = standartConfig.measure {
      list.par
    }
    println(s"list conversion time time:$listtime")

    val vectortime = standartConfig.measure {
      vector.par
    }
    println(s"vector conversion time:$vectortime")
    println(s"difference: ${listtime.value / vectortime.value}")
  }

}
