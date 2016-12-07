/**
  * Created by ilnur on 07.12.16.
  */
object MainCoursera extends App{
  /**
    * wk2_tsk1
    */
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a>b) acc
      else loop(a+1, f(a)+acc)
    }
    loop(a, 0)
  }
  println(sum(x=>x)(1,100))
  println(sum(x=>x*x)(1,100))
  println(sum(x=>x*x*x)(1,100))

  /**
    * wk2-tsk2
    */
  def product(f:Int => Int)(a:Int, b:Int): Int ={
    if (a>b) 1
    else f(a)*product(f)(a+1,b)
  }
  println(product(x=>x)(1,4))

  def fact(n:Int) = product(x=>x)(1,n)
}
