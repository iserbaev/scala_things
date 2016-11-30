package tasks_hostmann.ch11

/**
  * Created by ilnur on 30.11.16.
  */
object MainCh11 extends App{
  /**
    * ch11_tsk4
    */
  println(Money(1,75)+Money(0,50)==Money(2,25))
  println(Money(1,75)+Money(0,50)<Money(2,26))
  println(Money(1,75)+Money(0,50)>Money(2,26))
}
