package course2

/**
  * Created by ilnur on 11.02.17.
  */
object MainWeek1 extends App{
  /**
    * lecture 1.1 - Queries
    */
  val books: List[Book] = List(
    Book(title = "Structure and Interpretation of Computer Programs",
      authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Programming",
      authors = List("Bird, Richard", "Wadler, Phil")),
    Book(title = "Effective Java",
      authors = List(" Bloch, Joshua")),
    Book(title = "Effective Java 2",
      authors = List(" Bloch, Joshua")),
    Book(title = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Programming in Scala",
      authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill"))
  )

  /**
    * find the names of all authors who have written at least two books presented in database "books"
    */
  val query = { for {
      b1 <- books
      b2 <- books
      if b1.title < b2.title
      a1 <- b1.authors
      a2 <- b2.authors
      if a1 == a2
    } yield a1
  }.distinct

  println(query)
}

case class Book (title: String, authors: List[String])
