package tasks_hostmann

import scala.collection.mutable.ArrayBuffer

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
}