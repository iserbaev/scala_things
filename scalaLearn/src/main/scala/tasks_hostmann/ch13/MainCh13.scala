package tasks_hostmann.ch13

import scala.collection.mutable
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

  /**
    * ch13_tsk3
    */
  def delZeroFrom(linkedList: scala.collection.mutable.LinkedList[Int]):mutable.LinkedList[Int] = {
    var cur = linkedList
    var res = ArrayBuffer[Int]().mapResult{xs => mutable.LinkedList(xs:_*)}
    while (cur != Nil){
      if (cur.elem !=0)  res += cur.elem
      cur = cur.next
    }
    res.result()
  }
  println(delZeroFrom(scala.collection.mutable.LinkedList(2,0,3,6,0,9)))

  /**
    * ch13_tsk4
    */
  def findCountOf(arr:Array[String], assoc: Map[String, Int]):Array[Int] = {
    arr.filter(assoc.contains).map(assoc(_))
  }
  println(findCountOf(Array("Tom","Fred","Harry"),Map("Tom" -> 3, "Dick" ->4, "Harry" -> 5)).mkString("Array(",", ",")"))

  /**
    * ch13_tsk5
    */
  def suchAsMkString(a: Array[String]):String = {
    a.reduceLeft(_.toString +", "+_.toString)
  }
  println(suchAsMkString("kjsdfhkjsdh".map(String.valueOf).toArray))
  println(suchAsMkString(Array(1,2,3,5,6,8,7).map(String.valueOf)))
}
