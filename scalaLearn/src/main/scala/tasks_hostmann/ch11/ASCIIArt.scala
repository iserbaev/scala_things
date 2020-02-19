package tasks_hostmann.ch11

/**
  * Created by ilnur on 30.11.16.
  */
class ASCIIArt(val artObject: Array[String]) {
  override def toString =
    artObject.mkString

  def getSpaces(i: Int): String =
    " " * i

  def +(other: ASCIIArt): ASCIIArt = {
    val first  = this.artObject
    val firstL = first.max.length
    val sec    = other.artObject
    val res = first
      .zip(sec)
      .map(
        p =>
          p._1.substring(0, p._1.length - 1) + getSpaces(
            firstL - p._1.length + 1
          ) + p._2
      )
    new ASCIIArt(res)
  }
}
object ASCIIArt {
  def apply(s: String*): ASCIIArt = {
    val artObject = s.map(_ + "\n").toArray
    new ASCIIArt(artObject)
  }

}
