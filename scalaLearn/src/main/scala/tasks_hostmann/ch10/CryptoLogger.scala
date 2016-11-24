package tasks_hostmann.ch10

/**
  * Created by ilnur on 25.11.16.
  * ch10_tsk4
  */
class CryptoLogger(val alphabet:Array[Char], var shift:Int, val text:String) {

  def getIndexCrypto(x: Char): Int = {
    val maxIndex = alphabet.length-1
    val i = alphabet.indexOf(x)+shift
    if (i<0) {maxIndex+i}
    else if (i>maxIndex) {i-maxIndex}
    else i
  }

  def crypto():String = {
    text.map(x => alphabet(getIndexCrypto(x))).mkString
  }

  def getIndexDecrypto(x: Char): Int = {
    val maxIndex = alphabet.length-1
    val i = alphabet.indexOf(x)-shift
    if (i<0) {maxIndex+i}
    else if (i>maxIndex) {i-maxIndex}
    else i
  }

  def decrypto():String ={
    crypto().map(x => alphabet(getIndexDecrypto(x))).mkString
  }
}

object CryptoLogger{
  private var chars = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toArray
  chars = chars.+:(' ')
  chars = chars.+:(',')
  chars = chars.+:('.')
  def apply(text: String): CryptoLogger = new CryptoLogger(chars, -3, text)
}