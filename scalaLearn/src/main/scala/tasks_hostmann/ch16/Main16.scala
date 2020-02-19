package tasks_hostmann.ch16

import java.io.File

import scala.xml.parsing.{ConstructingParser, XhtmlParser}
import scala.xml.{Elem, Node, Text}

/**
  * Created by ilnur on 31.01.17.
  */
object Main16 extends App {
  val example: Elem =
    <html><head><title>Fred's Memoirs</title></head><body></body></html>
  println(example)

  /**
    * 1. Что означает < fred/>(0)? < fred/>(0)(0)?
    */
  val first = <fred/>.head
  val sec   = <fred/>.head.head

  /**
    * 2. Какой будет результат след. выражения
    */
  val second =
    <ul>
      <li>Opening bracket: [</li>
      <li>Closing bracket: ]</li>
      <li>Opening brace: {{</li>
      <li>Closing brace: }}</li>
    </ul>
  println(second)

  /**
    * 3. Сравните ... почему они действуют по разному?
    */
  val third_1 = <li>Fred</li> match { case <li>{Text(t)}</li> => t }
  println(third_1)
//  val third_2 = <li>{"Fred"}</li> match { case <li>{Text(t)}</li> => t }
//  println(third_2)
  /**
    * Здесь выскакивает ошибка - Exception in thread "main" scala.MatchError: < li>Fred</ li> (of class scala.xml.Elem)
    * потому что значения внутри {} представляет собой узлы типа Atom[String] а не Text[String]
    * чтобы получилось нужно делать как в 3 случае
    */
  val third_3 = <li>{Text("Fred")}</li> match { case <li>{Text(t)}</li> => t }
  println(third_3)

  /**
    * 4. Прочитайте файл *.xhtml и выведите все элементы img не имеющие атрибута alt.
    */
  val parser = new XhtmlParser(
    scala.io.Source.fromFile(
      "/home/ilnur/IdeaProjects/scala/scalaLearn/src/main/scala/tasks_hostmann/ch16/myfile.xhtml"
    )
  )
  val doc = parser.initialize.document()
  val root: Node = doc.docElem
  val imgElements = doc \\ "img"
  for (n <- imgElements) {
    val attr = n.attributes.asAttrMap
    if (!attr.keySet.contains("alt")) println(n)
  }

}
