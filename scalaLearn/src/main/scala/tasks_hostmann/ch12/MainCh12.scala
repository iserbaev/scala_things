package tasks_hostmann.ch12

/**
  * Created by ilnur on 30.11.16.
  */
object MainCh12 extends App{
  /**
    * c12_tsk1
    */
  def values(fun:(Int)=>Int, low:Int, high:Int) = {
    (low to high).map(x => (x, fun(x)))
  }
  println(values(x=>x * x, -5, 5).mkString(", "))

  /**
    * ch12_tsk2
    */
  def maxInArray(arr:Array[Int]):Int = {
    arr.reduceLeft(math.max(_,_))
  }
  println(maxInArray(Array(2,5,6,9,456,2)))

  /**
    * ch12_tsk3
    */
  def factorial(n:Int):Long = {
    (1 to n).reduceLeft(_*_)
  }
  println(factorial(10))

  /**
    * ch12_tsk4
    */
  def factorial2(n:Int):Int = {
    (1 to n).foldLeft(1)(_*_)
  }
  println(factorial2(10))
  println(factorial2(-10))

  /**
    * ch12_tsk5
    */
  def largest(fun:(Int) => Int, inputs:Seq[Int]):Int = {
    inputs.map(fun(_)).max
  }
  println(largest(x => 10*x-x*x, 1 to 10))

  /**
    * ch12_tsk6
    */
  def largestAt(fun:(Int) => Int, inputs:Seq[Int]):Int = {
    val m = inputs.map(fun(_))
    inputs(m.indexOf(m.max))
  }
  println(largestAt(x => 10*x-x*x, 1 to 10))
}
