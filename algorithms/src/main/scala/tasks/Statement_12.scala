package tasks

import java.io.{ BufferedReader, InputStreamReader }
import scala.collection.mutable.ArrayBuffer

// Rabin–Karp algorithm
object Main12 {

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
    val patternSize = pattern.length
    val textSize    = text.length

    val patternCodes = pattern.toCharArray.map(_.toInt)
    val textCodes    = text.toCharArray.map(_.toInt)

    val patternHash = patternCodes.sum

    val p                           = Int.MaxValue
    def hash(ints: Array[Int]): Int = ints.sum % p

    def recalcHash(oldHash: Int, oldHead: Int, newLast: Int): Int =
      (oldHash - oldHead % p) + newLast % p

    def isEqual(window: ArrayBuffer[Int], windowHash: Int) =
      patternHash == windowHash && patternCodes.sameElements(window)

    if (patternSize > textSize) ()
    else if (patternSize == textSize) {
      if (patternHash == hash(textCodes)) println(0)
    } else {
      val (textCodesHead, textCodesTail) = textCodes.splitAt(patternSize)

      val (lastWindow, lastIndex, lastHash) = textCodesTail.foldLeft(
        (ArrayBuffer.from(textCodesHead), 0, hash(textCodesHead))
      ) { case ((window, headIndex, oldHash), c) =>
        if (isEqual(window, oldHash)) print(s"$headIndex ")

        val newHash = recalcHash(oldHash, window(0), c)

        window.remove(0): Unit
        window.append(c): Unit

        (window, headIndex + 1, newHash)

      }
      if (isEqual(lastWindow, lastHash)) print(s"$lastIndex ")

      println()
    }
  }

}

object Test12 extends App {
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

  Main12.process(test_1._1, test_1._2)

  println("------------")

  Main12.process(test_2._1, test_2._2)

  println("------------")

  Main12.process(test_3._1, test_3._2)
}
