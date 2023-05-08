package tasks

import structures.AVLTree

import java.io.{ BufferedReader, InputStreamReader }

/** Реализуйте структуру данных для хранения множества целых чисел, поддерживающую запросы
  * добавления, удаления, поиска, а также суммы на отрезке. На вход в данной задаче будет
  * дана последовательность таких запросов. Чтобы гарантировать, что ваша программа
  * обрабатывает каждый запрос по мере поступления (то есть онлайн), каждый запрос будет
  * зависеть от результата выполнения одного из предыдущих запросов. Если бы такой
  * зависимости не было, задачу можно было бы решить оффлайн: сначала прочитать весь вход
  * и сохранить все запросы в каком-нибудь виде, а потом прочитать вход ещё раз,
  * параллельно отвечая на запросы.
  *
  * Формат входа. Изначально множество пусто. Первая строка содержит число запросов n.
  * Каждая из n следующих строк содержит запрос в одном из следующих четырёх форматов: • +
  * i: добавить число f(i) в множество (если оно уже есть, проигнорировать запрос); • - i:
  * удалить число f(i) из множества (если его нет, проигнорировать запрос); • ? i:
  * проверить принадлежность числа f (i) множеству; • s l r: посчитать сумму всех
  * элементов множества, попадающих в отрезок [f (l), f (r)].
  *
  * Функция f определяется следующим образом. Пусть s — результат последнего запроса суммы
  * на отрезке (если таких запросов ещё не было, то s = 0). Тогда f (x) = (x + s) mod 1
  * 000 000 001 .
  */
object Main15 {
  def main(args: Array[String]) = {
    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    val n = br.readLine().toInt
    val rows = (0 until n).map { _ =>
      br.readLine()
    }.toIndexedSeq

    process(rows)

    br.close()
  }

  private val intOrdering             = Ordering.Int
  private def fx(x: Int, s: Int): Int = (x + s) % 1000000001
  def process(lines: IndexedSeq[String]) = {
    val init: AVLTree[Int] = AVLTree.Leaf

    lines.foldLeft((init, 0)) { case ((tree, s0), line) =>
      val ar        = line.split(" ")
      val (head, i) = (ar.head, ar(1).toInt)
      def r         = ar(2).toInt

      head match {
        case "+" =>
          val hash = fx(i, s0)

          val resultTree = if (!tree.contains(hash, intOrdering)) {
            tree.insert(hash, intOrdering)
          } else tree

          resultTree -> s0
        case "-" =>
          val hash = fx(i, s0)

          val resultTree =
            if (tree.contains(hash, intOrdering))
              tree.remove(hash, intOrdering)
            else tree

          resultTree -> s0
        case "?" =>
          val hash = fx(i, s0)

          if (tree.contains(hash, intOrdering)) {
            println("Found")
          } else {
            println("Not found")
          }

          tree -> s0
        case "s" =>
          val hash_l = fx(i, s0)
          val hash_r = fx(r, s0)
          var sum    = 0

          AVLTree.inOrder(tree) {
            case AVLTree.Leaf =>
              ()
            case AVLTree.Node(data, _, _) =>
              if (hash_l <= data && data <= hash_r)
                sum = sum + data
              else ()
          }

          println(sum)

          tree -> sum
        case other =>
          throw new IllegalArgumentException(
            s"$other command illegal in $line"
          )
      }
    }
  }
}

object Test15 extends App {
  val test1 = IndexedSeq(
    "? 1",
    "+ 1",
    "? 1",
    "+ 2",
    "s 1 2",
    "+ 1000000000",
    "? 1000000000",
    "- 1000000000",
    "? 1000000000",
    "s 999999999 1000000000",
    "- 2",
    "? 2",
    "- 0",
    "+ 9",
    "s 0 9"
  ) -> IndexedSeq(
    "Not found",
    "Found",
    "3",
    "Found",
    "Not found",
    "1",
    "Not found",
    "10"
  )

  def test(lines: IndexedSeq[String], expected: Seq[String]): Unit = {
    val result = Main15.process(lines)
    assert(result._1 == expected)
  }

  test(test1._1, test1._2)
}
