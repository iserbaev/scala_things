package tasks_hostmann.scala-impatient-code.code.ch06.sec05

object Hello extends App {
  if (args.length > 0)
    println("Hello, " + args(0))
  else
    println("Hello, World!")
}
