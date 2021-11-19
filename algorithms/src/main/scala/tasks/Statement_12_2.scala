package tasks

import java.io.{BufferedReader, InputStreamReader}
import scala.collection.immutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

// Rabin–Karp algorithm
// Найти все вхождения строки Pattern в строку Text.
object Main12_2 {

  def main(args: Array[String]) = {
    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    val pattern = br.readLine()
    val text    = br.readLine()

    process(pattern, text)

    br.close()
  }

  def process(pattern: String, text: String): Unit = {
    val runner = new RCAlg(pattern, text)

    println(runner.getMatches.mkString(" "))
  }

}

class RCAlg(pattern: String, text: String) {
  private val DIVIDER: Short = 10007
  private val BASE:    Short = 47
  private val STEPS:   Short = 6

  private val patternLength: Int = pattern.length

  private val powers: immutable.IndexedSeq[Int] = fillPowers

  private def fillPowers: immutable.IndexedSeq[Int] = {
    val powers = ArrayBuffer.empty[Int]

    powers.insert(0, 1)
    powers.insert(1, BASE.toInt)

    if (patternLength > 2) {
      (2 to patternLength)
        .foreach(
          power => powers.insert(power, (powers(power - 1) * BASE) % DIVIDER)
        )
    }
    powers.result().toIndexedSeq
  }

  private def hash(chars: Array[Char]): Int = {
    val (lastHC, _) = chars.foldLeft((0, 0)) {
      case ((hash, power), ch) =>
        val newHC = (((hash + (ch.toInt * powers(power))) % DIVIDER) + DIVIDER) % DIVIDER

        (newHC, power + 1)
    }

    lastHC
  }

  private var subStringHashCode: Int = _
  private def hash(chars: Array[Char], i: Int): Int =
    if (i != chars.length - patternLength) {
      val h0 = ((subStringHashCode - (chars(i + patternLength) * powers(
          patternLength - 1
        )) % DIVIDER) + DIVIDER) % DIVIDER
      (h0 * BASE + chars(i)) % DIVIDER
    } else {
      val (lastHash, _) = (i until i + patternLength).foldLeft((0, 0)) {
        case ((hash, power), pos) =>
          val hashCode = (hash + (chars(pos) * powers(power)) % DIVIDER) % DIVIDER

          (hashCode, power + 1)
      }

      lastHash
    }

  def getMatches: Seq[Int] = {
    val patternChars = pattern.toCharArray
    val textChars    = text.toCharArray

    val patternHashCode = hash(patternChars)
    val matches         = ListBuffer[Int]()

    (text.length - pattern.length to 0 by -1).foreach { i =>
      subStringHashCode = hash(textChars, i)
      if (patternHashCode == subStringHashCode) {
        var equals = true
        var pLeft  = 0
        var pRight = patternLength - 1
        var tLeft  = i
        var tRight = i + pRight
        var steps  = 0
        while (tLeft < tRight && steps <= STEPS && equals) {
          if (pattern(pLeft) != text(tLeft) || pattern(pRight) != text(
                tRight
              )) {
            equals = false
          }

          pLeft += 1
          tLeft += 1
          pRight -= 1
          tRight -= 1
          steps += 1
        }
        if (equals) matches.prepend(i)
      }
    }

    matches.result()
  }

}

object Test12_2 extends App {
  val test_1 = (
    "aba",
    "abacaba"
  )

  val test_2 = (
    "Test",
    "testTesttesT"
  )

  val test_3 =
    ("aaaaa", "baaaaaaa")

  Main12_2.process(test_1._1, test_1._2)

  println("------------")

  Main12_2.process(test_2._1, test_2._2)

  println("------------")

  Main12_2.process(test_3._1, test_3._2)
}
