package tasks_hostmann.ch10
import java.awt.Point

/**
  * Created by ilnur on 21.11.16.
  * ch10_tsk2
  */
class OrderedPoint extends scala.math.Ordered[java.awt.Point] {
  var y: Int = 0
  var x: Int = 0
  override def compare(that: Point): Int =
    if (this.y < that.y && this.x <= that.x) -1
    else if (this.y == that.y && this.x == that.x) 0
    else 1
}
