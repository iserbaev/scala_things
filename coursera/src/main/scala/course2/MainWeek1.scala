package course2

import scala.util.Try

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

  /**
    * lecture 1.3 Functional random generators
    */
  trait Generator[+T] {
    self =>  // an alias for "this"

    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }
    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }
  }

  val integers = new Generator[Int] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  }
  val booleansBoilerplate = new Generator[Boolean] {
    def generate = integers.generate > 0
  }
  val booleans = integers map(x => x > 0)

  val pairsBoilerplate = new Generator[(Int, Int)] {
    def generate = (integers.generate, integers.generate)
  }
  def pairs[T, U](t: Generator[T], u: Generator[U]) =
    t flatMap (x => u map(y => (x,y)))
  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate = x
  }
  def choose(lo: Int, hi: Int): Generator[Int] = {
    for (x <- integers) yield lo + x % (hi - lo)
  }
  def oneOf[T](xs: T*): Generator[T] =
    for (idx <- choose(0, xs.length)) yield xs(idx)
  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list
  def emptyLists = single(Nil)
  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail

  trait Tree
  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree
  def leafs: Generator[Leaf] = for{
    x <- integers
  } yield Leaf(x)
  def inners: Generator[Inner] = for {
    l <- trees
    r <- trees
  } yield Inner(l, r)
  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  } yield tree

  /**
    * tests generator
    */
  def test[T](g: Generator[T], numTimes: Int = 100)
             (test: T => Boolean): Unit = {
    for (i <- 0 until numTimes){
      val  value = g.generate
      assert(test(value), "test failed for " + value)
    }
    println("passed " + numTimes + " tests")
  }
  /**
    * example of test using -> if ">" must be assertion failed or if ">=" must be true
    */
  test(pairs(lists, lists)) {
    case (xs: List[Int], ys: List[Int]) => (xs ++ ys).length >= xs.length
  }

  /**
    * ScalaCheck tool
    *
    * forAll { (l1: List[Int], l2: List[Int]) =>
    *   l1.size + l2.size == (l1 ++ l2).size
    * }
  */

  /**
    * lecture 1.4 Monads
    */
  trait M[T] {
    def flatMap[U](f: T => M[U]): M[U]  //in literature called as "bind"
  }

  /**
    *     def unit[T](x: T): M[T]
    *
    *     List is a monad with unit(x) = List(x)
    *     Option is a monad with unit(x) = Some(x)
    *
    *     map for monad is combination of flatMap and unit
    *     m map f == m flatMap (x => unit(f(x)))
    *             == m flatMap (f andThen unit)
    */
  val trySum = Try(5+1)
  println(trySum)

  val tryFail = Try(5/0)
  println(tryFail)
}
