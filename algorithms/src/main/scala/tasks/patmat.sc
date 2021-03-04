// for example program that print value to console

// ADT + patmat
sealed trait Data1
case class StrContainer(value: String) extends Data1
case class DouContainer(value: Double) extends Data1

def foo1(foo: Data1) = foo match {
  case StrContainer(value) => println(value)
  case DouContainer(value) => println(value)
}

// Generic Value CLass + patmat
case class Container[+A](value: A)

val scont = Container("string")
val dcont = Container(2.0d)

def foo2[T](container: Container[T]) = container match {
  case Container(x: String) => println(x)
  case Container(d: Double) => println(d)
  case other => println(other.value.toString)
}

// TF = TypeClass + ContextBound + instances
trait Data[A] {
  def logic(value: A): Unit
}
object Data {
  implicit val strData: Data[String] = new Data[String] {
    override def logic(value: String): Unit = println(value)
  }

  implicit val douData: Data[Double] = new Data[Double] {
    override def logic(value: Double): Unit = println(value)
  }
}

def foo3[A: Data](value: A): Unit = implicitly[Data[A]].logic(value)

// Checks
foo1(StrContainer("string"))
foo1(DouContainer(2.0d))

foo2(scont)
foo2(dcont)

import Data._
foo3("string")
foo3(2.0d)

// Comparision

// |  ADT + partmat           | TypeClass + ContextBound + instances  |
// |  Allocations             | No allocations                        |
// |  on new nodes -          | just add new instance                 |
// |  we have to modify       | -                                     |
// |  each patmat             | -                                     |
