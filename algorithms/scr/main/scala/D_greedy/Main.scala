package D_greedy

import scala.collection.mutable

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
  trait Codec[I,O] {
    def code: I => O
    def decode: O => I
  }
  case class HuffmanCodec() extends Codec[String,String] {
    var codes: Map[Char,String] = Map.empty[Char,String]
    override def code: String => String = in => {
      println(in)
      val chars = in.toCharArray
      val map   = frequency(chars)
      println(map)
      val ppq = mutable.PriorityQueue.empty[A]
      ppq.addAll(map)
      println(ppq)
      def recur(pq: mutable.PriorityQueue[A]): mutable.PriorityQueue[A] =
        if (pq.size >= 2) {
          val (e1, i1) = pq.dequeue()
          println(s"ppq get next = ($e1,$i1)")
          val (e2, i2) = pq.dequeue()
          println(s"ppq get next = ($e2,$i2)")
          (e1, e2) match {
            case (c1: Left[Char, Tree[Char]], c2: Left[Char, Tree[Char]]) =>
              val leaf = Tree.create((c1.value, i1), (c2.value, i2))
              pq.enqueue((Right(leaf), leaf.priority))
              recur(pq)
            case (c1: Left[Char, Tree[Char]], t2: Right[Char, Tree[Char]]) =>
              val node = Tree.add(c1.value, i1, t2.value)
              pq.enqueue((Right(node), node.priority))
              recur(pq)
            case (t1: Right[Char, Tree[Char]], c2: Left[Char, Tree[Char]]) =>
              val node = Tree.add(c2.value, i2, t1.value)
              pq.enqueue((Right(node), node.priority))
              recur(pq)
            case (t1: Right[Char, Tree[Char]], t2: Right[Char, Tree[Char]]) =>
              val node = Tree.merge(t1.value, t2.value)
              pq.enqueue((Right(node), node.priority))
              recur(pq)
          }
        } else {
          println(s"nothing do ${pq}")
          pq
        }
      val tree: Tree[Char] =
        recur(ppq).dequeue()._1.getOrElse(sys.error("queue is empty"))

      println(tree)
      codes = Tree.codes[Char](tree)
      println(codes)
      val coded = chars.foldLeft("")((acc,c) => acc + codes(c))
      println(coded)
      coded
    }

    override def decode: String => String = ???
  }
  trait Tree[T] {
    def priority: Int
    def asString(acc: String = ""): String = this match {
      case Leaf(value, priority) =>
        acc + s"Leaf(value: $value, priority: $priority)"
      case Node(left, right, priority) =>
        val next = acc + "     "
        acc + s"""Node(
         ${left.asString(next)}
         ${right.asString(next)}
         ${acc + s"priority: $priority"}
         $acc)
         """
    }

    override def toString: String = this.asString()
  }
  object Tree {
    def create[T](value: (T, Int)): Tree[T] =
      Leaf(value._1, value._2)
    def create[T](value1: (T, Int), value2: (T, Int)): Tree[T] = {
      val ((t1, p1), (t2, p2)) =
        if (value1._2 <= value2._2) (value1, value2) else (value2, value1)
      Node(
        Leaf(t1, p1),
        Leaf(t2, p2),
        p1 + p2
      )
    }
    def add[T](v1: T, p1: Int, tree: Tree[T]): Tree[T] =
      if (p1 <= tree.priority) {
        Node(
          Leaf(v1, p1),
          tree,
          p1 + tree.priority
        )
      } else {
        Node(
          tree,
          Leaf(v1, p1),
          p1 + tree.priority
        )
      }

    def merge[T](tree1: Tree[T], tree2: Tree[T]): Tree[T] = {
      val (t1, t2) =
        if (tree1.priority <= tree2.priority) tree1 -> tree2 else tree2 -> tree1

      Node(
        t1,
        t2,
        t1.priority + t2.priority
      )
    }

    def codes[T](tree: Tree[T],
                 treeCodeAcc: String = "",
                 acc: Map[T,String] = Map.empty[T,String]): Map[T,String] = tree match {
      case Leaf(value, _) =>
        acc.+((value, treeCodeAcc))
      case Node(left, right, _) =>
        codes[T](left, treeCodeAcc + "0", acc) ++ codes[T](right, treeCodeAcc + "1", acc)
    }
  }
  case class Node[T](
    left:     Tree[T],
    right:    Tree[T],
    priority: Int
  ) extends Tree[T]

  case class Leaf[T](value: T, priority: Int)
      extends Tree[T]

  type E     = Either[Char, Tree[Char]]
  type A     = (E, Int)
  type Queue = mutable.PriorityQueue[A]
  implicit val ord: Ordering[A] = (x: A, y: A) =>
    -Ordering.Int.compare(x._2, y._2)
  def frequency(chars: Array[Char]): Map[E, Int] = {
    val (m, (c, freq)) =
      chars.sorted.foldLeft((Map.empty[E, Int], ('a', 0))) {
        case ((m, (accChar, freq)), c) =>
          accChar match {
            case a if a == c =>
              (m, (accChar, freq + 1))
            case aa if aa != c =>
              val mm = if (freq == 0) m else m.+((Left(accChar), freq))
              (mm, (c, 1))
          }
      }
    m.+((Left(c), freq))
  }

  def main(args: Array[String]): Unit = {
    val in  = "beep boop beer!"
    HuffmanCodec().code(in)
    ()
  }

}

//    val in = scala.io.StdIn.readLine()
//    code(in)

//    val tests = List(
//      "a" -> List(1 -> 1, List('a' -> "0"), "0"),
//      "abacabad" -> List(
//        4 -> 14,
//        List('a' -> "0", 'b' -> "10", 'c' -> "110", 'd' -> "111"),
//        "01001100100111"
//      ),
//      "accepted" -> List(
//        6 -> 20,
//        List(
//          'p' -> "110",
//          'a' -> "111",
//          'c' -> "10",
//          't' -> "011",
//          'd' -> "010",
//          'e' -> "00"
//        ),
//        "11110100011001100010"
//      )
//    )

//    code(tests.head._1)
//    code(tests.tail.head._1)
//    code(tests.tail.tail.head._1)
