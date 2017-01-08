package course1; /**
  * Created by ilnur on 07.12.16.
  */
object MainCoursera_week2 extends App{
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

  /**
    * wk2_tsk3
    */
  def isCloseEnough(x:Double, y:Double):Boolean = {
    val tolerance=0.01
    math.abs((x-y)/x)/x < tolerance
  }
  def fixedPoint(f:Double => Double)(firstGuess : Double) = {
    def iterate(guess:Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
  fixedPoint(x => 1+x/2)(1)
  def sqrtIncrt(x:Double) = fixedPoint(y => x/y)(1)
  sqrtIncrt(2)
  def averageDump(f: Double => Double)(x:Double) = (x+f(x))/2
  def sqrt(x:Double) = fixedPoint(averageDump(y => x/y))(1)
  sqrt(2)

  /**
    * wk2_tsk4
    */
  class Rational(x:Int, y:Int){
    require(y!=0,"denominator must be non zero")
    private def gcd(a:Int, b:Int):Int = if(b==0)a else gcd(b,a%b)
    private val g = gcd(x,y)
    def numer = x / g
    def denom = y/ g

    def + (that: Rational) =
      new Rational(
        numer*that.denom+that.numer*denom,
        denom*that.denom
      )

    def unary_- : Rational = new Rational(-numer, denom)
    def unary_+ : Rational = new Rational(numer, denom)
    def - (that:Rational): Rational = this + -that
    def / (that:Rational) =
      new Rational(
        numer*that.denom,
        denom*that.numer
      )
    def * (that:Rational) =
      new Rational(
        numer*that.numer,
        denom*that.denom
      )

    def < (that:Rational) = numer*that.denom < that.numer*denom
    def max (that:Rational) = if (this < that) that else this
    override def toString() = numer + "/" + denom
  }
  val x = new Rational(1,3)
  val y = new Rational(5,7)
  val z = new Rational(3,2)
  x + y
  -x
  +x
  x - y
  x - y
  x - y - z
  x + y * z
  x < y


}
