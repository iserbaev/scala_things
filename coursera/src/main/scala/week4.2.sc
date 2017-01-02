trait Expr{
  def eval:Int = this match {
    case Numb(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }
}
case class Numb(n: Int) extends Expr
case class Sum(e1:Expr, e2: Expr) extends Expr
object exprs{
  def show(e: Expr):String = e match {
    case Numb(n) => String.valueOf(n)
    case Sum(e1, e2) => show(e1).concat("+").concat(show(e2))
  }
}
exprs.show(Sum(Numb(1),Numb(44)))