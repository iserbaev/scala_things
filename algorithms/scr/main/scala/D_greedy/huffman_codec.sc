import scala.collection.mutable
import scala.io.StdIn
import scala.collection.mutable.PriorityQueue

/**
  * По данной непустой строке
  * s длины не более 10&#94;4,
  * состоящей из строчных букв латинского алфавита,
  * постройте оптимальный беспрефиксный код. В первой строке выведите количество различных букв k, встречающихся в строке,
  * и размер получившейся закодированной строки.
  * В следующих k строках запишите коды букв в формате "letter: code".
  * В последней строке выведите закодированную строку.
  */
object Main {
  case class Edge(code: Option[String] = None, prev: Option[Edge] = None) {
    def mergeCodes: Option[String] =
      prev
        .flatMap(_.mergeCodes)
        .fold(code)(acc => code.map(acc + _).orElse(Some(acc)))
  }
  trait Tree[T] {
    def priority: Int
    def edge:     Option[Edge]
    def updateEdges(): Tree[T] = this match {
      case l: Leaf[T] =>
        l.copy(edge = edge)
      case n: Node[T] =>
        n.copy(edge = edge)
          .copy(left = n.left.updateEdges(), right = n.right.updateEdges())
    }
    def setCode(c: String): Tree[T] = this match {
      case l: Leaf[T] =>
        l.copy(
          edge = l.edge.map(_.copy(code = Some(c))).orElse(Some(Edge(Some(c))))
        )
      case n: Node[T] =>
        n.copy(
          edge = n.edge.map(_.copy(code = Some(c))).orElse(Some(Edge(Some(c))))
        )
    }
  }
  object Tree {
    def create[T](value1: (T, Int), value2: (T, Int)): Tree[T] = {
      val ((t1, p1), (t2, p2)) =
        if (value1._2 <= value2._2) (value1, value2) else (value2, value1)
      Node(
        Leaf(t1, Some(Edge(Some("0"))), p1),
        Leaf(t2, Some(Edge(Some("1"))), p2),
        None,
        p1 + p2
      )
    }
    def add[T](v1: T, p1: Int, tree: Tree[T]): Tree[T] =
      if (p1 <= tree.priority) {
        val t = tree.setCode("1")
        Node(
          Leaf(v1, Some(Edge(Some("0"), t.edge)), p1),
          t,
          None,
          p1 + tree.priority
        ).updateEdges()
      } else {
        val t = tree.setCode("0")
        Node(
          t,
          Leaf(v1, Some(Edge(Some("1"), t.edge)), p1),
          None,
          p1 + tree.priority
        ).updateEdges()
      }

    def merge[T](tree1: Tree[T], tree2: Tree[T]): Tree[T] = {
      val (t1, t2) =
        if (tree1.priority <= tree2.priority) tree1 -> tree2 else tree2 -> tree1

      Node(
        t1.setCode("0"),
        t2.setCode("1"),
        None,
        t1.priority + t2.priority
      ).updateEdges()
    }
  }
  case class Node[T](
    left:     Tree[T],
    right:    Tree[T],
    edge:     Option[Edge],
    priority: Int
  ) extends Tree[T]

  case class Leaf[T](value: T, edge: Option[Edge], priority: Int)
      extends Tree[T]

  type A     = (Char, Int)
  type Queue = mutable.PriorityQueue[A]
  implicit val ord: Ordering[A] = (x: (Char, Int), y: (Char, Int)) =>
    -Ordering.Int.compare(x._2, y._2)
  def frequency(chars: Array[Char]): Map[Char, Int] = {
    val (m, (c, freq)) =
      chars.sorted.foldLeft((Map.empty[Char, Int], ('a', 0))) {
        case ((m, (accChar, freq)), c) =>
          accChar match {
            case a if a == c =>
              (m, (accChar, freq + 1))
            case aa if aa != c =>
              val mm = if (freq == 0) m else m.+((accChar, freq))
              (mm, (c, 1))
          }
      }
    m.+((c, freq))
  }

  def code(in: String): Unit = {
    val chars = in.toCharArray
    val map   = frequency(chars)
    val ppq   = mutable.PriorityQueue.empty[A]
    ppq.addAll(map)

    println(in)
    println(map)
    println(ppq)
  }
  def main(args: Array[String]): Unit = {
    val in = StdIn.readLine()
    code(in)

  }
}
import Main._
val tests = List(
    "a" -> List(1 -> 1, List('a' -> "0"), "0"),
    "abacabad" -> List(
      4 -> 14,
      List('a' -> "0", 'b' -> "10", 'c' -> "110", 'd' -> "111"),
      "01001100100111"
    ),
    "accepted" -> List(
      6 -> 20,
      List(
        'p' -> "110",
        'a' -> "111",
        'c' -> "10",
        't' -> "011",
        'd' -> "010",
        'e' -> "00"
      ),
      "11110100011001100010"
    )
  )

code(tests.head._1)
code(tests.tail.head._1)
code(tests.tail.tail.head._1)
