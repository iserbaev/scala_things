package tasks

import structures.BinarySearchTree

import java.io.{BufferedReader, InputStreamReader}

/** Обход двоичного дерева
  * Построить in-order, pre-order и post-order обходы данного двоичного дерева.
  *
  * Формат входа.
  * Первая строка содержит число вершин n.
  * Вершины дерева пронумерованы числами от 0 до n−1.
  * Вершина 0 является корнем.
  * Каждая из следующих n строк содержит информацию о вершинах 0, 1, . . . , n − 1:
  *   i-я строка задаёт числа key_i, left_i и right_i,
  *     где key_i — ключ вершины i,
  *         left_i — индекс левого сына вершины i,
  *         а right_i — индекс правого сына вершины i.
  *         Если у вершины i нет одного или обоих сыновей, соответствующее значение равно −1.
  *
  * Формат выхода.
  * Три строки: in-order,pre-order и post-order обходы.
  */
object Main13 {

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

  def process(lines: IndexedSeq[String]): Unit = {
    val rows = lines.map { line =>
      val ar = line.split(" ")
      (ar.head, ar(1).toInt, ar(2).toInt)
    }

    def mkTree(
      row:    (String, Int, Int),
      parent: Option[String]
    ): BinarySearchTree[String] = {
      val (s, leftIndex, rightIndex) = row

      BinarySearchTree.Node(
        s,
        if (leftIndex != -1) mkTree(rows(leftIndex), Some(s))
        else BinarySearchTree.BNil,
        if (rightIndex != -1) mkTree(rows(rightIndex), Some(s))
        else BinarySearchTree.BNil,
        parent
      )
    }

    if (lines.nonEmpty) {
      val headRow = rows.head
      val tree    = mkTree(headRow, None)
      val runNode: BinarySearchTree.Node[String] => Unit = n =>
        print(s"${n.k} ")
      BinarySearchTree.inOrder(tree, runNode)
      println()
      BinarySearchTree.preOrder(tree, runNode)
      println()
      BinarySearchTree.postOrder(tree, runNode)
    }
  }
}

object Test13 extends App {
  val test1 = IndexedSeq(
    "0 7 2",
    "10 -1 -1",
    "20 -1 6",
    "30 8 9",
    "40 3 -1",
    "50 -1 -1",
    "60 1 -1",
    "70 5 4",
    "80 -1 -1",
    "90 -1 -1"
  )

  def test(lines: IndexedSeq[String]): Unit =
    Main13.process(lines)

  test(test1)
}
