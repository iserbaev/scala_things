package tasks_hostmann.scala-impatient-code.code.ch14.sec14

sealed abstract class Amount

case class Dollar(value: Double) extends Amount
case class Currency(value: Double, unit: String) extends Amount

object Main extends App {
  def show(amt: Amount) = 
    // You will get a "match not exhaustive" warning. 
    // That warning is not there if Amount isn't sealed.
    amt match { 
      case Currency(v, u) => "I have " + v + " " + u
    }  

  println(show(Currency(100000, "JPY")))
}
