package tasks_hostmann.ch15

import java.io.IOException

import scala.annotation.varargs
import scala.io.Source

/**
  * Created by ilnur on 18.01.17.
  */
object Main15 extends App{
  /**
    * 4. напишите метод sum с переменным числом целочисленных аргументов,
    * возвращающий сумму своих аргументов.
    * Вызовите его из Java.
    */
  @varargs def sum(e: Int*): Int = {
    e.sum
  }
  println(sum(5, 8, 9, 6, 6, 3, 2, 5, 6))

  /**
    * 5. напишите метод возвращающий строковое значение с содержимым текстового файла.
    * Вызовите его из Java.
    */
  @throws(classOf[IOException]) def readFile(path: String): String = {
    val source = Source.fromFile(path, "UTF-8")
    source.mkString
  }

  /**
    * 6. Реализуйте объект с volatile-полем типа Boolean. Приостановите выполнение одного потока на некоторое время,
    * затем присвойте этому полю значение true в этом же потоке, выведите сообщение и завершите работу потока.
    * Другой поток, выполняющийся параллельно, должен проверять значение этого поля, и если оно имеет значение true -
    * выводить сообщение и завершаться. В противном случае он должен приостанавливаться на короткое время
    * и повторять попытку.
    * Что случится если поле не объявлено как volatile?
    */
  @volatile var done = false
  val firstThread = new Thread( new Runnable {
    override def run() = {
      Thread.sleep(3003)
      done = true
      println(s"first thread - done is changed to $done")
    }
  })
  val secondThread = new Thread(new Runnable {
    override def run() = {
      while (!done){
        Thread.sleep(1000)
        println(s"second thread - done is $done")
      }
      println(s"second thread - done is changed to $done")
    }
  })
  firstThread.start()
  secondThread.start()


}
