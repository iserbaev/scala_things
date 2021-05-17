package tasks

import tasks.Statement_7.MergeTable

import scala.collection.mutable

object Statement_7 {
  sealed trait MergeTable {
    def id: Int
  }
  object MergeTable {
    case class TableLink(id: Int, link:    Int, maxRecords: Int) extends MergeTable
    case class RealTable(id: Int, records: Int) extends MergeTable {
      def toLink(destination: RealTable): TableLink =
        TableLink(id, destination.id, destination.records)
    }

    def merge(sourceId: Int, destinationId: Int)(
      mapper:           Int => MergeTable,
      updateF:          MergeTable => Unit
    ): RealTable = {
      @scala.annotation.tailrec
      def getRealTable(id: Int): RealTable = mapper(id) match {
        case l: TableLink => getRealTable(l.link)
        case r: RealTable => r
      }

      val sourceReal      = getRealTable(sourceId)
      val destinationReal = getRealTable(destinationId)

      if (sourceReal != destinationReal) {
        val destinationToUpdate = destinationReal.copy(
          records = sourceReal.records + destinationReal.records
        )
        val sourceToUpdate = sourceReal.toLink(destinationToUpdate)
        updateF(sourceToUpdate)
        updateF(destinationToUpdate)
        destinationToUpdate
      } else {
        destinationReal
      }
    }

    def calculateUnions(
      sizes:      Array[Int],
      mergeTasks: Seq[(Int, Int)]
    ): Seq[Int] = {
      val builder = mutable.Map.newBuilder[Int, MergeTable]

      sizes.zipWithIndex.foldLeft(builder) {
        case (acc, (size, index)) =>
          acc += index + 1 -> RealTable(index + 1, size)
      }

      val mapper = builder.result()

      val getFun:    Int        => MergeTable = mapper(_)
      val updateFun: MergeTable => Unit       = mt => mapper.update(mt.id, mt)

      mergeTasks.map {
        case (destinationId, sourceId) =>
          MergeTable.merge(sourceId, destinationId)(getFun, updateFun).records
      }
    }
  }

}

object Main7 {

  import tasks.Statement_7.MergeTable

  def readTuple: (Int, Int) = {
    val t = scala.io.StdIn.readLine().split(" ")
    require(t.length == 2)
    t.head.toInt -> t.last.toInt
  }
  def main(args: Array[String]) = {
    val m = scala.io.StdIn.readLine().split(" ").last.toInt
    val sizes:      Array[Int]      = scala.io.StdIn.readLine().split(" ").map(_.toInt)
    val mergeTasks: Seq[(Int, Int)] = (1 to m).map(_ => readTuple)

    val result: Seq[Int] = MergeTable.calculateUnions(sizes, mergeTasks)

    result.foreach(println)
  }
}

object TestApp7 extends App {
  def test(
    sizes:           Array[Int],
    mergeTasks:      Array[(Int, Int)],
    expectedRecords: Seq[Int]
  ): Unit = {
    val result: Seq[Int] = MergeTable.calculateUnions(sizes, mergeTasks)

    assert(
      expectedRecords == result,
      s"expectedRecords != result. \n ${expectedRecords
        .mkString("(", ",", ")")}. \n  ${result.mkString("(", ",", ")")}"
    )
  }

  test(
    Array(1, 1, 1, 1, 1),
    Array(
      3 -> 5,
      2 -> 4,
      1 -> 4,
      5 -> 4,
      5 -> 3
    ),
    Seq(2, 2, 3, 5, 5)
  )
}
