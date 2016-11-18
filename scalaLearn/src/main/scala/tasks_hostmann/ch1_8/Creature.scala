package tasks_hostmann.ch1_8

/**
  * Created by ilnur on 18.11.16.
  * ch8_tsk9
  */
class Creature {
  def range: Int = 10
  val env: Array[Int] = new Array[Int](range)
}

class Ant extends Creature{
  override def range: Int = 2
}