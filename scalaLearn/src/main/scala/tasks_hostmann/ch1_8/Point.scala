package tasks_hostmann.ch1_8

/**
  * Created by ilnur on 17.11.16.
  * ch6_tsk4
  * ch8_tsk5
  */
class Point(val x: Int, val y: Int) {}

object Point {
  def apply(x: Int, y: Int) = new Point(x, y)
}

class LabeledPoint(val label: String, override val x: Int, override val y: Int)
    extends Point(x, y) {}
