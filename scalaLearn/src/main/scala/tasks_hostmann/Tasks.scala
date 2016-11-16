package tasks_hostmann

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
}
