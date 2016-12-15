package tasks_hostmann.scala-impatient-code.code.ch07.sec05

package object people {
  val defaultName = "John Q. Public"
}

object Main extends App {
  val john = new com.horstmann.impatient.people.Person
  println(john.description)
}

// Run as scala com.horstmann.impatient.Main

class Person {
    var name = defaultName // A constant from the package
    def description = "A person with name " + name
  }
