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
  type A = (Char,Int)
  type Queue = mutable.PriorityQueue[A]
  implicit val ord: Ordering[A] = (x: (Char, Int), y: (Char, Int)) => Ordering.Int.compare(x._2, y._2)
  def frequency(chars: Array[Char]): Map[Char,Int] = {
    val (m,(c,freq)) = chars.sorted.foldLeft((Map.empty[Char,Int], ('a',0))){
      case ((m,(accChar,freq)),c) =>
        accChar match {
          case a if a == c =>
            (m,(accChar,freq + 1))
          case aa if aa != c =>
            val mm = if (freq == 0) m else m.+((accChar,freq))
            (mm,(c,1))
        }
    }
    m.+((c,freq))
  }

  def code(in: String): Unit = {
    val chars = in.toCharArray
    val map = frequency(chars)
    val ppq = mutable.PriorityQueue.empty[A]
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
val tests= List(
  "a" -> List(1 -> 1, List('a' -> "0"), "0"),
  "abacabad" -> List(4 -> 14, List('a' -> "0", 'b' -> "10", 'c' -> "110", 'd' -> "111"), "01001100100111"),
  "accepted" -> List(6 -> 20, List('p' -> "110", 'a' -> "111", 'c' -> "10", 't' -> "011",'d' -> "010", 'e' -> "00"),"11110100011001100010")
)

code(tests.head._1)
code(tests.tail.head._1)
code(tests.tail.tail.head._1)
