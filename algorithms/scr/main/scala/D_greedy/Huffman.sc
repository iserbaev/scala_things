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
    type E           = Either[Char, Tree[Char]]
    def treesChars(
                    charsWithFrequencies: List[(Char, Int)],
                    trees:   ListBuffer[Tree[Char]] = ListBuffer.empty[Tree[Char]]
                  ): ListBuffer[Tree[Char]] = {
      def resolve(
                   ch:         List[(Char, Int)],
                   trs:         ListBuffer[Tree[Char]],
                   charsPriority: Boolean = true
                 ) = (ch.headOption, trs.headOption) match {
        case (None, None) =>
          sys.error("Both chars and trees is empty")
        case (None, Some(tree)) =>
          (Right(tree) -> tree.priority, ch, trs.tail)
        case (Some((c, i)), None) =>
          (Left(c) -> i, ch.tail, trs)
        case (Some((c, i)), Some(tree)) =>
          if (i < tree.priority || (i == tree.priority && charsPriority)) {
            (Left(c) -> i, ch.tail, trs)
          } else {
            (Right(tree) -> tree.priority, ch, trs.tail)
          }
      }

      val ((e1: E, i1: Int), chars1, acc1) = resolve(charsWithFrequencies, trees)
      val ((e2: E, i2: Int), chars2, acc2) =
        resolve(chars1, acc1, charsPriority = false)

      val node = (e1, e2) match {
        case (c1: Left[Char, Tree[Char]], c2: Left[Char, Tree[Char]]) =>
          Tree.create((c1.value, i1), (c2.value, i2))
        case (c1: Left[Char, Tree[Char]], t2: Right[Char, Tree[Char]]) =>
          Tree.add(c1.value, i1, t2.value)
        case (t1: Right[Char, Tree[Char]], c2: Left[Char, Tree[Char]]) =>
          Tree.add(c2.value, i2, t1.value)
        case (t1: Right[Char, Tree[Char]], t2: Right[Char, Tree[Char]]) =>
          Tree.merge(t1.value, t2.value)
      }

      if (acc2.isEmpty && chars2.isEmpty) {
        ListBuffer(node)
      } else {
        acc2.append(node)
        treesChars(chars2, acc2)
      }
    }
  }

  case class HuffmanDictionary()
    extends Dictionary[String, String, Char, String] {
    def frequency(chars: List[Char]): List[(Char, Int)] = {
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
      val mapFreq = m.+((c, freq))
      val result = chars.distinct
        .map(c => {
          (c, mapFreq(c))
        })
        .sortBy(_._2)
      result
    }
    override def splitInput: String => List[Char] = _.toCharArray.toList

    override def splitOutput: (String, Set[String]) => List[String] =
      (s, codes) => {
        val chars = s.toCharArray.toList
        val (notCollected, splitted) =
          chars.foldLeft((ListBuffer.empty[Char], ListBuffer.empty[String])) {
            case ((prevs, acc), ch) =>
              prevs.append(ch)
              val s      = prevs.mkString("")
              if (codes.contains(s)) {
                acc.append(s)
                (ListBuffer.empty[Char], acc)
              } else {
                (prevs, acc)
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
        if (chars.size == 1) Map(chars.head -> "0")
        else {
          val tree: Tree[Char] =
            Tree
              .treesChars(frequency(chars))
              .headOption
              .getOrElse(sys.error("queue is empty"))

          Tree.treeCodes(tree)
        }
      }

    override def combineInput: List[Char] => String = _.mkString("")

    override def combineOutput: List[String] => String = _.mkString("")
  }
  case class HuffmanCodec() extends Codec[String, String, Char, String] {
    override val dictionary: Dictionary[String, String, Char, String] =
      HuffmanDictionary()
  }

  def main(args: Array[String]): Unit = {
    val codec = HuffmanCodec()

//    val in = "beep boop beer!"
//    val coded = codec.code(in)
//    val inRel = codec.dictionary.calculateRelationsInput(in.toCharArray.toList)
//    println(s"${inRel.size} ${coded.toCharArray.length}")
//    println(inRel.toList.map(t => s"${t._1}: ${t._2}").mkString("", "\n", ""))
//    println(coded)
//
//    val decoded = codec.decode(coded, inRel)
//    println(decoded)
//
//
//    println(codec.code("abacabad"))
//    println("01001100100111")
//    assert(codec.code("abacabad") == "01001100100111")
//    assert(
//      codec.decode(
//        "01001100100111",
//        Map('a' -> "0", 'b' -> "10", 'c' -> "110", 'd' -> "111")
//      ) == "abacabad"
//    )
//
//    println(codec.code("a"))

    val accepted = "accepted"
    println("11110100011001100010")
    println(codec.code(accepted))
    println(
      Map(
        'p' -> "110",
        'a' -> "111",
        'c' -> "10",
        't' -> "011",
        'd' -> "010",
        'e' -> "00"
      )
    )
    println(
      codec.dictionary.calculateRelationsInput(accepted.toCharArray.toList)
    )
    assert(codec.code(accepted) == "11110100011001100010")
  }
}

Main.main(Array())