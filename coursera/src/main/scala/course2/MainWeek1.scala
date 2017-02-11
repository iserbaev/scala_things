package course2

/**
  * week 1 - for expressions and monads
  */
object MainWeek1 extends App{
  /**
    * lecture 1.1 - Queries with for
    */
  case class Book (title: String, authors: List[String])
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


  /**
    * lecture 2.2 - translation of for
    */
  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    for (x <- xs) yield f(x)

  def flatMapFun[T, U](xs: List[T], f: T => Iterable[U]): List[U] =
    for (x <- xs; y <- f(x)) yield y

  def filterFun[T](xs: List[T], p: T => Boolean): List[T] =
    for (x <- xs; if p(x)) yield x

  /**
    * simple for expression
    * for (x <- e1) yield e2
    * will transform by compiler to
    * e1.map(x => e2)
    */

  /**
    * for expression
    * for (x <- e1 if f; s) yield e2
    * will transform by compiler to
    * for (x <- e1.withFilter(x => f); s) yield e2
    * and then translate to simplier expressions
    */
  /**
    * s - mean is "something more sequence of generators or filters"
    */
  /**
    * for expression
    * for (x <- e1; y <- e2; s) yield e3
    * will transform by compiler to
    * e1.flatMap( x => for (y <- e2); s) yield e3)
    * and then translate to simplier expressions
    */
  /**
    * for expression
    * for {
    * i <- 1 until n
    * j <- 1 until n
    * if isPrime(i + j)
    * } yield (i, j)
    * will transform by compiler to
    * (1 until n).flatMap( i =>
    *   (1 until i).withFilter(j => isPrime(i+j))
    *   .map(j => (i,j))
    *   )
    */
  val forQuery = for(b <- books; a <- b.authors if a startsWith "Bird")
    yield b.title
  val translatedQuery = books flatMap (b =>
    b.authors withFilter(a => a startsWith "Bird") map(y => b.title))
  println(translatedQuery)
}
