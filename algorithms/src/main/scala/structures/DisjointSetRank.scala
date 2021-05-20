package structures

import scala.collection.mutable

class DisjointSetRank(val origin: mutable.IndexedSeq[Int]) {
  private val parent     = mutable.Map.empty[Int, Int]
  private val rank       = mutable.Map.empty[Int, Int]
  private val maxes      = IndexedSeq.newBuilder[Int]
  private var currentMax = Int.MinValue

  def makeSet(index: Int): Unit = {
    parent += ((index, index))
    rank += ((index, 0))

    if (origin(index) > currentMax) currentMax = origin(index)
  }

  def find(index: Int): Int = {
    def recur(i: Int): Int = parent(i) match {
      case ii if ii == i =>
        ii
      case ii =>
        val res = recur(ii)
        parent += ((i, res))
        res
    }

    recur(index)
  }

  def union(destinationIndex: Int, sourceIndex: Int): Unit = {
    val destinationParent = find(destinationIndex)
    val sourceParent      = find(sourceIndex)

    val destinationRank = rank(destinationParent)
    val sourceRank      = rank(sourceParent)

    if (destinationParent != sourceParent) {
      if (destinationRank > sourceRank) {
        updateParent(destinationParent, sourceParent)
      } else {
        if (destinationRank == sourceRank) {
          rank.update(sourceParent, sourceRank + 1)
          updateParent(destinationParent, sourceParent)
        } else {
          updateParent(sourceParent, destinationParent)
        }
      }
    } else {
      maxes += currentMax
    }
  }

  private def updateParent(destination: Int, source: Int): Unit = {
    parent.update(source, destination)

    val destinationValue = origin(destination)
    val sourceValue      = origin(source)

    val sum = destinationValue + sourceValue

    origin.update(destination, sum)
    origin.update(source, 0)
    if (sum > currentMax) currentMax = sum
    maxes += currentMax
  }

  def result: IndexedSeq[Int] = maxes.result()
}
