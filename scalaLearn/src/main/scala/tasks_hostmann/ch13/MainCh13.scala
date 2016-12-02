package tasks_hostmann.ch13

import scala.collection.mutable.ArrayBuffer

/**
  * Created by ilnur on 02.12.16.
  */
object MainCh13 extends App{
  /**
    * ch13_tsk1
    */

  def indexesOfString(input:String) = {
    val setS = scala.collection.immutable.Set
    val mapS = scala.collection.mutable.Map(input.charAt(0) -> setS())
    input.foreach(mapS(_) = setS())
    mapS.map(x => (x._1,indexesOf(input.toList, x._1,0).toSet))
  }
  def indexesOf(input: List[Char], s: Char, accumulator:Int):List[Int] = {
    val ss = ArrayBuffer[Int]()
    for (i <- input.indices) if (input(i).equals(s)) ss.append(i)
    ss.toList.sorted
  }
  println(indexesOfString("Mississippi"))
}
