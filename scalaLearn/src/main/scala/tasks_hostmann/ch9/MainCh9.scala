package tasks_hostmann.ch9

import java.io._
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDate

import org.joda.time._
import org.joda.time.format.DateTimeFormat
import tasks_hostmann.ch1_8.Person

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

  /**
    * Write a Scala program that prints the src attributes of all img tags of a web
    * page. Use regular expressions and groups.
    * @param url
    * @return
    */
  def ch9_tsk8(url:String):Array[String] = {
    val sourceURL = Source.fromURL(url, "UTF-8")
    val urlString = sourceURL.mkString

    val pattern = """(<img+) (src=.*+)""".r
    val s= for (pattern(img, src) <- pattern.findAllIn(urlString)) yield src
    val srcBuffer= s.map(_.split(" ")(0)).toArray
    println(srcBuffer.toBuffer.toString())
    srcBuffer
  }

  /**
    * Alternatively, if you use Java 7, you can adapt the walkFileTree method of the
    * java.nio.file.Files class. That class makes use of a FileVisitor interface. In Scala,
    * we generally prefer to use function objects, not interfaces, for specifying work
    * (even though in this case the interface allows more fine-grained control—see the
    * Javadoc for details). The following implicit conversion adapts a function to the interface
    */
  import java.nio.file._
  implicit def makeFileVisitor(f:(Path) => Unit) = new SimpleFileVisitor[Path] {
    override def visitFile(file: Path, attrs: BasicFileAttributes) = {
      f(file)
      FileVisitResult.CONTINUE
    }
  }
  /**
    * Write a Scala program that counts how many files with .class extension are
    * in a given directory and its subdirectories.
    */
  def ch9_tsk9(catalog:String):Int = {
    var count =0
    Files.walkFileTree(Paths.get(catalog), (f:Path) => if (f.getFileName.toString.contains(".class")) count=count+1)
    count
  }

  /**
    * Expand the example with the serializable Person class that stores a collection
    * of friends. Construct a few Person objects, make some of them friends of
    * another, and then save an Array[Person] to a file. Read the array back in and
    * verify that the friend relations are intact.
    */
  def ch9_tsk10():Unit = {
    val fred = new Person("Fred M.")
    val stiv = new Person("Stiv K.")
    fred.friends += stiv

    val out = new ObjectOutputStream(new FileOutputStream(
      "/home/ilnur/repo/scala/scalaLearn/src/main/scala/tasks_hostmann/ch9/test.obj"))
    out.writeObject(fred)
    out.close()

    val in = new ObjectInputStream(new FileInputStream(
      "/home/ilnur/repo/scala/scalaLearn/src/main/scala/tasks_hostmann/ch9/test.obj"))
    val savedFred = in.readObject().asInstanceOf[Person]
    print(savedFred+" and his friend: ")
    savedFred.friends.foreach(println(_))
  }

  def markDateIfWeekend(inputDate: String): String = {
    val format = DateTimeFormat.forPattern("dd.MM.yy")
    val date = DateTime.parse(inputDate, format)
    val dayOfWeek = date.getDayOfWeek()
    if (dayOfWeek == 6 || dayOfWeek == 7) "*".concat(inputDate) else inputDate
  }




//  ch9_tsk1("/home/ilnur/Загрузки/ant.conf")
//  ch9_tsk2("/home/ilnur/Загрузки/ant.conf")
//  ch9_tsk3("/home/ilnur/Загрузки/ant.conf")
//  ch9_tsk4("/home/ilnur/Загрузки/test.conf")
//  ch9_tsk8("http://eax.me/")
//  println(ch9_tsk9("/home/ilnur/repo/scala/scalaLearn/src/main/scala/"))
  ch9_tsk10()
  println(markDateIfWeekend("12.11.16")+" "+markDateIfWeekend("13.11.16")+" "+markDateIfWeekend("14.11.16")
    +" "+markDateIfWeekend("15.11.16")+" "+markDateIfWeekend("21.11.16"))
}
