package tasks_hostmann.scala-impatient-code.code.ch18.sec12

trait Reader {
  type Contents
  def read(fileName: String): Contents
}

import scala.io._

class StringReader extends Reader {
  type Contents = String
  def read(fileName: String) = Source.fromFile(fileName, "UTF-8").mkString
}

import java.awt.image._
import java.io._
import javax.imageio._

class ImageReader extends Reader {
  type Contents = BufferedImage
  def read(fileName: String) = ImageIO.read(new File(fileName))
}
