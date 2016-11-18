package tasks_hostmann

/**
  * Created by ilnur on 17.11.16.
  * ch6_tsk4
  */
class Point(val x:Int,val y:Int) {

}

object Point{
  def apply(x: Int, y: Int) = new Point(x, y)
}
