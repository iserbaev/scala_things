abstract class Booleans{
  def ifThenElse[T](t: => T, e: => T):T

  def && (x: => Booleans): Booleans = ifThenElse(x, falses)
  def || (x: => Booleans): Booleans = ifThenElse(trues, x)
  def unary_! : Booleans = ifThenElse(falses, trues)

  def == (x: => Booleans): Booleans = ifThenElse(x, x.unary_!)
  def != (x: => Booleans): Booleans = ifThenElse(x.unary_!, x)
  def < (x: => Booleans): Booleans = ifThenElse(falses, x)
}
object trues extends Booleans{
  def ifThenElse[T](t: => T, e: => T):T = t
}
object falses extends Booleans{
  def ifThenElse[T](t: => T, e: => T):T = e
}

//Peano numbers
abstract class Nat{
  def isZero : Booleans
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}
object Zero extends Nat{
  def isZero: Booleans = trues
  def predecessor: Nat = throw new Error("0.predecessor")
  def +(that: Nat): Nat = that
  def -(that: Nat): Nat = if (that.isZero) this else throw new Error("negativ number")
}
class Succ(n: Nat) extends Nat{
  def isZero: Booleans = falses
  def predecessor: Nat = n
  def +(that: Nat): Nat = new Succ(n + that)
  def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}