package tasks

import structures.BinarySearchTree

import java.io.{BufferedReader, InputStreamReader}

/** Проверить, является ли данное двоичное дерево деревом поиска.
  *
  * Вход. Двоичное дерево.
  *
  * Выход.
  *   Проверить, является ли оно корректным деревом поиска:
  *   верно ли, что для любой вершины дерева её
  *     ключ больше всех ключей в левом поддереве данной вершины и
  *          меньше всех ключей в правом поддереве.
  */
object Main14 {
  def main(args: Array[String]) = {
    val br: BufferedReader = new BufferedReader(
      new InputStreamReader(System.in)
    )

    val n = br.readLine().toInt
    val rows = (0 until n).map { _ =>
      br.readLine()
    }.toIndexedSeq

    println(process(rows))

    br.close()
  }

  val INCORRECT = "INCORRECT"
  val CORRECT   = "CORRECT"

  def process(lines: IndexedSeq[String]): String = {
    lazy val rows = lines.map { line =>
      val ar = line.split(" ")
      (ar.head.toInt, ar(1).toInt, ar(2).toInt)
    }

    def mkTree(
      row:    (Int, Int, Int),
      parent: Option[Int]
    ): BinarySearchTree[Int] = {
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
      if (lines.size == 1 && lines.head.split(" ").length == 1) {
        CORRECT
      } else {
        val headRow = rows.head
        val tree    = mkTree(headRow, None)

        val correct = BinarySearchTree.isValid(tree)
        if (correct) CORRECT else INCORRECT
      }
    } else CORRECT
  }
}

object Test14 extends App {
  val INCORRECT = "INCORRECT"
  val CORRECT   = "CORRECT"

  val test1 = IndexedSeq(
    "2 -1 1",
    "5 2 -1",
    "3 -1 3",
    "5 -1 -1"
  )

  val test2 = IndexedSeq("0")

  val test3 = IndexedSeq(
    "4 1 2",
    "2 3 4",
    "6 5 6",
    "1 -1 -1",
    "3 -1 -1",
    "5 -1 -1",
    "7 -1 -1"
  )

  def test(lines: IndexedSeq[String], expected: String): Unit =
    assert(Main14.process(lines) == expected)

  test(test1, INCORRECT)
  test(test2, CORRECT)
  test(test3, CORRECT)
}
