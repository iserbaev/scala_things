package tasks_hostmann.ch1_8

/**
  * Created by ilnur on 17.11.16.
  * ch6_tsk5
  */
object Reverse extends App{
  args.reverse
    .map(_+" ")
    .foreach(print(_))
}
