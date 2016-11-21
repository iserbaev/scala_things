package tasks_hostmann.ch10

/**
  * Created by ilnur on 21.11.16.
  * ch10_tsk1
  */
object MainCh10 extends App{
  val egg = new java.awt.geom.Ellipse2D.Double(5,10,20,30) with RectangleLike
  egg.translate(10,-10)
  egg.grow(10,20)
}
