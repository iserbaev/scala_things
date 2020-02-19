package tasks_hostmann.ch1_8

/**
  * Created by ilnur on 18.11.16.
  * ch8_tsk6
  */
abstract class Shape {
  def centerPoint(): Point
}

class Rectangle(
  val leftTop:  Point,
  val rightTop: Point,
  val width:    Int,
  val height:   Int
) extends Shape {
  override def centerPoint(): Point =
    Point(leftTop.x + width / 2, leftTop.y - height / 2)
}

class Circle(override val centerPoint: Point, val radius: Int) extends Shape {}

class Square(x: Int, y: Int, width: Int)
    extends java.awt.Rectangle(x, y, width, width) {
  def this(width: Int) {
    this(0, 0, width)
  }
  def this() {
    this(0, 0, 0)
  }
}
