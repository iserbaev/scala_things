package D_greedy

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
  type E           = Either[Char, Tree[Char]]
  type HeapElement = (E, Int)

  trait Dictionary[I, O, IS, OS] {
    def splitInput:              I            => List[IS]
    def splitOutput:             (O, Set[OS]) => List[OS]
    def calculateRelationsInput: List[IS]     => Map[IS, OS]
    def combineInput:            List[IS]     => I
    def combineOutput:           List[OS]     => O
  }
  trait Codec[I, O, IS, OS] {
    def dictionary: Dictionary[I, O, IS, OS]
    def code:   I                => O = calcIn
    def decode: (O, Map[IS, OS]) => I = calcOut
    def calcIn: I => O = in => {
      val inSplitted     = dictionary.splitInput(in)
      val relationsInput = dictionary.calculateRelationsInput(inSplitted)
      val outSeq         = inSplitted.map(relationsInput)
      dictionary.combineOutput(outSeq)
    }
    def calcOut: (O, Map[IS, OS]) => I = (out, relationsInput) => {
      val relationsOutput = relationsInput.map { case (is, os) => os -> is }
      val outSplitted     = dictionary.splitOutput(out, relationsOutput.keySet)
      val inSeq           = outSplitted.map(relationsOutput)
      dictionary.combineInput(inSeq)
    }
  }
  trait Heap[T] {
    def getMin: (T, Int)
    def add(t: T, priority: Int): Unit
    def size: Int
  }
  object Heap {
    def treesChars(pq: Heap[E]): Heap[E] =
      if (pq.size >= 2) {
        val (e1, i1) = pq.getMin
        val (e2, i2) = pq.getMin
        (e1, e2) match {
          case (c1: Left[Char, Tree[Char]], c2: Left[Char, Tree[Char]]) =>
            val leaf = Tree.create((c1.value, i1), (c2.value, i2))
            pq.add(Right(leaf), leaf.priority)
            treesChars(pq)
          case (c1: Left[Char, Tree[Char]], t2: Right[Char, Tree[Char]]) =>
            val node = Tree.add(c1.value, i1, t2.value)
            pq.add(Right(node), node.priority)
            treesChars(pq)
          case (t1: Right[Char, Tree[Char]], c2: Left[Char, Tree[Char]]) =>
            val node = Tree.add(c2.value, i2, t1.value)
            pq.add(Right(node), node.priority)
            treesChars(pq)
          case (t1: Right[Char, Tree[Char]], t2: Right[Char, Tree[Char]]) =>
            val node = Tree.merge(t1.value, t2.value)
            pq.add(Right(node), node.priority)
            treesChars(pq)
        }
      } else {
        pq
      }
  }
  trait Tree[T] {
    def priority: Int
    override def toString: String = Tree.asString(this)
  }
  object Tree {
    case class Node[T](
      left:     Tree[T],
      right:    Tree[T],
      priority: Int
    ) extends Tree[T]

    case class Leaf[T](value: T, priority: Int) extends Tree[T]
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
    def treeCodes[T](
      tree:        Tree[T],
      treeCodeAcc: String = "",
      acc:         Map[T, String] = Map.empty[T, String]
    ): Map[T, String] = tree match {
      case Leaf(value, _) =>
        acc.+((value, treeCodeAcc))
      case Node(left, right, _) =>
        treeCodes[T](left, treeCodeAcc + "0", acc) ++ treeCodes[T](
          right,
          treeCodeAcc + "1",
          acc
        )
    }
    def asString[T](t: Tree[T], acc: String = ""): String = t match {
      case Leaf(value, priority) =>
        acc + s"Leaf(value: $value, priority: $priority)"
      case Node(left, right, priority) =>
        val next = acc + "     "
        acc + s"""Node(
         ${asString(left, next)}
         ${asString(right, next)}
         ${acc + s"priority: $priority"}
         $acc)
         """
    }
  }

  case class PriorityQueueHeap(val chars: Array[Char]) extends Heap[E] {
    type Queue = mutable.PriorityQueue[HeapElement]
    implicit val ord: Ordering[HeapElement] =
      (x: HeapElement, y: HeapElement) => -Ordering.Int.compare(x._2, y._2)

    val map:        Map[E, Int] = frequency(chars)
    val underlying: Queue       = mutable.PriorityQueue.empty[HeapElement]
    underlying.addAll(map)

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

    override def getMin: (E, Int) = underlying.dequeue()
    override def add(e: E, priority: Int): Unit =
      underlying.enqueue((e, priority))
    override def size: Int = underlying.size
  }

  case class HuffmanDictionary()
      extends Dictionary[String, String, Char, String] {
    override def splitInput: String => List[Char] = _.toCharArray.toList

    override def splitOutput: (String, Set[String]) => List[String] =
      (s, codes) => {
        val chars = s.toCharArray.toList
        val (notCollected, splitted) =
          chars.foldLeft((ListBuffer.empty[Char], ListBuffer.empty[String])) {
            case ((prevs, acc), ch) =>
              val prevs2 = prevs.appended(ch)
              val s      = prevs2.mkString("")
              if (codes.contains(s)) {
                (ListBuffer.empty[Char], acc.appended(s))
              } else {
                (prevs2, acc)
              }
          }
        if (notCollected.nonEmpty)
          sys.error(
            s"can't split out not collected chars ${notCollected.mkString(",")}"
          )

        splitted.toList
      }

    override def calculateRelationsInput: List[Char] => Map[Char, String] =
      chars => {
        val heap = PriorityQueueHeap(chars.toArray)

        val tree: Tree[Char] =
          Heap.treesChars(heap).getMin._1.getOrElse(sys.error("queue is empty"))

        Tree.treeCodes(tree)
      }

    override def combineInput: List[Char] => String = _.mkString("")

    override def combineOutput: List[String] => String = _.mkString("")
  }
  case class HuffmanCodec() extends Codec[String, String, Char, String] {
    override val dictionary: Dictionary[String, String, Char, String] =
      HuffmanDictionary()
  }

  def main(args: Array[String]): Unit = {
    val in      = "beep boop beer!"
    val codec   = HuffmanCodec()
    val coded   = codec.code(in)
    val inRel   = codec.dictionary.calculateRelationsInput(in.toCharArray.toList)
    val decoded = codec.decode(coded, inRel)
    println(coded)
    println(decoded)

    println(codec.code("abacabad"))
    println("01001100100111")
    assert(codec.code("abacabad") == "01001100100111")
    assert(
      codec.decode(
        "01001100100111",
        Map('a' -> "0", 'b' -> "10", 'c' -> "110", 'd' -> "111")
      ) == "abacabad"
    )
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
