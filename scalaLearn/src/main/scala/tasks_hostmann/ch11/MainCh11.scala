package tasks_hostmann.ch11

/**
  * Created by ilnur on 30.11.16.
  */
object MainCh11 extends App {

  /**
    * ch11_tsk4
    */
  println(Money(1, 75) + Money(0, 50) == Money(2, 25))
  println(Money(1, 75) + Money(0, 50) < Money(2, 26))
  println(Money(1, 75) + Money(0, 50) > Money(2, 26))

  /**
    * ch11_tsk6
    */
  val art =
    ASCIIArt("  /\\_/\\", "(  ' '  )", "(   -   )", "  | | | ", " (__|__)")
  println(art)
  val art2 =
    ASCIIArt("   -----", " / Hello \\", "<  Scala |", " \\ Coder /", "   _____")
  println(art2)
  val art3 = art + art2
  println(art3)
}
