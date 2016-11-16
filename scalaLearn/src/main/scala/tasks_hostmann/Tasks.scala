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
}
