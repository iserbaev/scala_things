package tasks_hostmann

import scala.collection.mutable.ArrayBuffer
import java.awt.datatransfer.{DataFlavor, SystemFlavorMap}
import java.util

import tasks_hostmann.ch1_8._

import scala.collection.JavaConverters._
import scala.collection.mutable
/**
  * Created by ilnur on 16.11.16.
  */
object Tasks extends App{
  println("test")
  def codesSumOf1(s:String):Long= {
    if (s.isEmpty) 1
    else s.codePointAt(0)*codesSumOf1(s.tail)
  }
  def codesSumOf2(s:String):Long= {
    (for (i <- 0 until s.length) yield s.codePointAt(i).toLong).product
  }
  def codesSumOf3(s:String):Long = s.codePoints().asLongStream().toArray.product
  println(codesSumOf1("Hello"))
  println(codesSumOf2("Hello"))
  println(codesSumOf3("Hello"))

  def pow_ch2_tsk10(x:Int, i:Int):Int = {
    if (i==0) 1
    else if (i%2==0 && i>0) pow_ch2_tsk10(x, i/2)*pow_ch2_tsk10(x, i/2)
    else if (i%2 !=0 && i>0) x*pow_ch2_tsk10(x, i-1)
    else 1/pow_ch2_tsk10(x, -i)
  }
  println(pow_ch2_tsk10(2,2))

  def arr_ch3_tsk1(ar:Array[Int], n:Int):Array[Int] = {
    (for (i<-0 until n) yield i).toArray
  }
  println(arr_ch3_tsk1(new Array[Int](40), 40).toBuffer.toString)

  def arr_ch3_tsk2(ar:Array[Int]):Array[Int] = {
    (for (i<-ar.indices) yield
      if (i % 2 == 0 && i!=ar.length-1) ar(i+1)
      else if (i % 2 == 0 && i==ar.length-1) ar(i)
      else ar(i-1)
      ).toArray
  }
  println(arr_ch3_tsk2(Array(1,2,3,4,5,6)).toBuffer.toString())

  def arr_ch3_tsk4(ar:Array[Int]):Array[Int] = {
    Array(ar.partition(_ >0)._1, ar.partition(_ <= 0)._1).flatten
  }
  println(arr_ch3_tsk4(Array(-1,-2,1,0,2,-5,3,4)).toBuffer.toString())

  def arr_ch3_tsk5(ar:Array[Double]):Double = ar.sum/ar.length
  println(arr_ch3_tsk5(Array(1,2,3,4)))

  def arr_ch3_tsk6_1(ar:Array[Int]):Array[Int] = scala.util.Sorting.stableSort(ar, _ > _)
  println(arr_ch3_tsk6_1(Array(1,2,5,4,6,9,2)).toBuffer.toString())

  def arr_ch3_tsk6_2(ar:ArrayBuffer[Int]):ArrayBuffer[Int] = ar.sorted.reverse
  println(arr_ch3_tsk6_2(ArrayBuffer(1,2,5,4,6,9,2)).toString())

  def arr_ch3_tsk9():Array[String]=
    java.util.TimeZone.getAvailableIDs()
      .filter(_.contains("America/"))
      .map(_.substring(8))
  println(arr_ch3_tsk9().toBuffer.toString())

  def arr_ch3_tsk10():Array[AnyRef] ={
    SystemFlavorMap.getDefaultFlavorMap()
      .asInstanceOf[SystemFlavorMap]
      .getNativesForFlavor(DataFlavor.imageFlavor)
      .toArray
  }
  println(arr_ch3_tsk10().toBuffer.toString())

  def map_ch4_tsk1():Map[String, Double]={
    val pc = Map("PC"->50, "LapTop"->75,"Dev"->5)
    for ((k, v) <- pc) yield (k, v * 0.9)
  }

  def map_ch4_tsk2(fileName: String):Map[String, Int] = {
    val ar = new mutable.ArrayBuffer[String]
    val in = new java.util.Scanner(new java.io.File(fileName))
    while (in.hasNext) {
      ar += in.next()
    }
    (for (i<-ar) yield (i, ar.count(_.eq(i)))).toMap
  }
  println(map_ch4_tsk2("/home/ilnur/Загрузки/ant.conf"))

  def map_ch4_tsk4(fileName: String):mutable.SortedMap[String, Int] = {
    val ar = new mutable.ArrayBuffer[String]
    val in = new java.util.Scanner(new java.io.File(fileName))
    while (in.hasNext) {
      ar += in.next()
    }
    val m = (for (i<-ar) yield (i, ar.count(_.eq(i)))).toMap
    val sm = mutable.SortedMap[String, Int]()
    sm.++(m)
  }
  println(map_ch4_tsk4("/home/ilnur/Загрузки/ant.conf"))

  def map_ch4_tsk5():Unit = {
    import scala.collection.JavaConversions.mapAsScalaMap
    val scores: collection.mutable.Map[String, Int] = new java.util.TreeMap[String, Int]

    import scala.collection.JavaConversions.propertiesAsScalaMap
    val props: scala.collection.Map[String, String] = System.getProperties()
    println(props)
  }
  map_ch4_tsk5()

  def map_ch4_tsk7():Unit = {
    import scala.collection.JavaConversions.propertiesAsScalaMap
    val props: scala.collection.Map[String, String] = System.getProperties()
    println("___________________________________________________________________________________________________")
    for ((k,v)<-props) Console.print(k.concat("                                                                "
      .substring(k.length))+" |  "+v+"\n")
    println("______________________________________________________________________________________")
  }
  map_ch4_tsk7()

  def map_ch4_tsk8(ar:Array[Int]):(Int,Int) = ar.min -> ar.max
  println(map_ch4_tsk8(Array(2,3,54,5,6,9)))

  def map_ch4_tsk9_lteqgt(ar: Array[Int], v:Int):(Int,Int,Int) = (ar.count(_ < v), ar.count(_ == v), ar.count(_ > v))
  println(map_ch4_tsk9_lteqgt(Array(2,3,5,6,9,5,6,5,4,6,2,65899,0), 8))

  println("Hello".zip("World"))

  println(Time(47,98).toString)
  assert(Time(49,98).hour == Time(1,38).hour)

  println(new Student("name", 1))
  new Person("Fred Smith")

  new Car("lada", "granta")
  new CarJava("lada", "granta")

  println(CardEnum.toString())

  val creature = new Creature
  println(creature.range+" "+creature.env.length)

  val ant = new Ant
  println(ant.range+" "+ant.env.length)
}