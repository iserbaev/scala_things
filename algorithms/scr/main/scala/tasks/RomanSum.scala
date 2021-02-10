package tasks

import language.postfixOps
import scala.util.{Failure, Success, Try}

//Roman Calculator
//  You need to create a calculator that accepts two strings representing Roman numbers
//  and returns the sum of the two numbers presented.
//You should return an error if any of the inputs are invalid.
//Do not use any libraries or built-in language features that operate on Roman numbers.
//* Numerals are I V X L C D M (1, 5, 10, 50, 100, 500, 1000)
//* V, L and D can only be once in a number
//  * I, C and M can only be repeated three times in succession
//* Numbers bigger than MMMCMXCIX are out of scope of this exercise

case class Roman(s: String)  {
  def validate(): Unit = {
    require(s.toUpperCase.count(_ == 'V') <= 1, "V can only be once in a number")
    require(s.toUpperCase.count(_ == 'L') <= 1, "L can only be once in a number")
    require(s.toUpperCase.count(_ == 'D') <= 1, "D can only be once in a number")

    require(!s.toUpperCase.contains("MMMM"), "M can only be repeated three times in succession")
    require(!s.toUpperCase.contains("CCCC"), "C can only be repeated three times in succession")
    require(!s.toUpperCase.contains("IIII"), "I can only be repeated three times in succession")
  }

  def toArabic: Int =
    s.toUpperCase.map(Roman.mapping).foldLeft((0,0)) {
      case ((acc, previous), currentDigit) =>
        (acc + currentDigit + (if (previous < currentDigit) -2 * previous else 0), currentDigit)
    }._1

}

object Roman {
  val mapping: Map[Char, Int] =
    Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100,  'D' -> 500, 'M' -> 1000)

  def toRoman(value: Int): String = {
    "M" * (value / 1000) +
      ("", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM").productElement(value % 1000 / 100) +
      ("", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC").productElement(value % 100 / 10) +
      ("", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX").productElement(value % 10)
  }

  implicit class RomanSyntaxSupport(val r: Roman) {
    def +(r2: Roman): String = toRoman(r.toArabic + r2.toArabic)
  }
}

object RomanCalculator extends App {
  import Roman._

  sealed trait Command {
    def value: String
  }
  object Command {
    case object RomanSum extends Command {
      val value = "romanSum"
    }
    case object Exit extends Command {
      val value = "exit"
    }
  }

  def runApp(): Unit = {
    val preamble =
      s"""
         |If you want to calculate sum - press 1
         |If you want to exit          - press 0
         |""".stripMargin

    println(preamble)
    val command = Try(io.StdIn.readInt())

    command.flatMap(runCommand) match {
      case Failure(exception) =>
        println(exception.getMessage)
        runApp()
      case Success(value) =>
        println(value)
        runApp()
    }
  }

  def runCommand(command: Int): Try[String] = command match {
    case 1 => calculateRomanSum()
    case 0 => exit
    case _ => Failure(new RuntimeException("You can choose only sum or exit"))
  }

  def calculateRomanSum(): Try[String] = Try {
    println("type first roman number, put enter, and then second roman number. After validating and calc sum will print")
    val input1: String = io.StdIn.readLine()
    val input2: String = io.StdIn.readLine()

    val r1 = Roman(input1)
    val r2 = Roman(input2)

    r1.validate()
    r2.validate()

    val sum = r1 + r2

    sum
  }

  def exit: Try[Nothing] = Try(sys.exit(0))


  println("It is program to calculate sum of roman numbers")
  runApp()
}

object SumTest extends App {
  import Roman._

  def testSum(s1: String, s2: String, expected: String): Unit = {
    val result = Roman(s1) + Roman(s2)
    assert(result == expected, s"$result != $expected")
  }

  def testConvert(s: String, expected: Int): Unit = {
    val res = Roman(s)
    assert(res.toArabic == expected, s"$res != $expected")
  }

  testConvert("MCMXC", 1990)
  testConvert("MMVIII", 2008)
  testConvert("MDCLXVI", 1666)
  testConvert("MMMCMXCVIII", 3998)

  testSum("MCMXC","MMVIII","MMMCMXCVIII")
}
