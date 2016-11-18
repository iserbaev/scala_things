package tasks_hostmann.ch9

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.io.Source

/**
  * Created by ilnur on 18.11.16.
  */
object MainCh9 extends App{

  /**
    * Write a Scala code snippet that reverses the lines in a file (making the last
    * line the first one, and so on).
    * @param filePath
    */
  def ch9_tsk1(filePath:String):Unit ={
    val source = Source.fromFile(filePath, "UTF-8")
    val tokens = source.mkString.split("\\s+")
    val reversedLineArray = tokens.map(_+" ").toBuffer.reverse
    source.close()
    Files.write(Paths.get(filePath), reversedLineArray.mkString.getBytes(StandardCharsets.UTF_8))
  }

  /**
    * Write a Scala program that reads a file with tabs, replaces each tab with spaces
    * so that tab stops are at n-column boundaries, and writes the result to the
    * same file.
    * @param filePath
    */
  def ch9_tsk2(filePath:String):Unit = {
    val source = Source.fromFile(filePath, "UTF-8")
    val tokens = source.mkString.split("\\t")
    val reversedLineArray = tokens.map(_+" ").toBuffer.reverse
    source.close()
    Files.write(Paths.get(filePath), reversedLineArray.mkString.getBytes(StandardCharsets.UTF_8))
  }

  /**
    * Write a Scala code snippet that reads a file and prints all words with more than 12 characters to the console.
    * Extra credit if you can do this in a single line.
    * @param filePath
    */
  def ch9_tsk3(filePath:String):Unit = {
    Source.fromFile(filePath).mkString.split("\\s+").filter(_.length>12).foreach(println(_))
  }

  /**
    * Write a Scala program that reads a text file containing only floating-point
    * numbers. Print the sum, average, maximum, and minimum of the numbers
    * in the file.
    * @param filePath
    */
  def ch9_tsk4(filePath:String):Unit = {
    val source = Source.fromFile(filePath)
    val tokens = source.mkString.split("[+-]([0-9]*[.][0-9])+").flatMap(_.split(" "))
    println(tokens.toBuffer.toString())
    val buffer = tokens.map(_.toDouble)
    println("sum="+buffer.sum+", max="+buffer.max+", min="+buffer.min+", avg="+buffer.sum/buffer.length)
  }

//  ch9_tsk1("/home/ilnur/Загрузки/ant.conf")
//  ch9_tsk2("/home/ilnur/Загрузки/ant.conf")
//  ch9_tsk3("/home/ilnur/Загрузки/ant.conf")
  ch9_tsk4("/home/ilnur/Загрузки/test.conf")
}
