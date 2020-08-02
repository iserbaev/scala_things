package structures

import cats.syntax.option._

import scala.annotation.tailrec
import scala.collection.immutable

sealed trait BinarySearchTree[+T] {
  def key:    Option[T]
  def left:   BinarySearchTree[T]
  def right:  BinarySearchTree[T]
  def parent: BinarySearchTree[T]
}

case object Nil extends BinarySearchTree[Nothing] {
  override def key: Option[Nothing] = None

  override def left:   BinarySearchTree[Nothing] = Nil
  override def right:  BinarySearchTree[Nothing] = Nil
  override def parent: BinarySearchTree[Nothing] = Nil
}

case class Node[+T](
  k: T,
  l: BinarySearchTree[T],
  r: BinarySearchTree[T],
  p: BinarySearchTree[T]
) extends BinarySearchTree[T] {
  override def key:    Option[T]           = k.some
  override def left:   BinarySearchTree[T] = l
  override def right:  BinarySearchTree[T] = r
  override def parent: BinarySearchTree[T] = p
}

object BinarySearchTree {
  def inorderTreeWalk[T](x: BinarySearchTree[T]): List[T] = {
    @tailrec
    def recur(ts: List[BinarySearchTree[T]], acc: List[T]): List[T] = ts match {
      case immutable.Nil => acc
      case ::(head, tl) =>
        (head.left, head.right) match {
          case (Nil, Nil) =>
            recur(tl, head.key.map(_ :: acc).getOrElse(acc))
          case (Nil, value) =>
            recur(value :: tl, head.key.map(_ :: acc).getOrElse(acc))
          case (value, Nil) =>
            recur(value :: tl, head.key.map(_ :: acc).getOrElse(acc))
          case (value, value1) =>
            recur(value :: value1 :: tl, head.key.map(_ :: acc).getOrElse(acc))
        }
    }

    recur(List(x), List.empty)
  }

  @tailrec
  def treeSearch[T](x: BinarySearchTree[T], t: T)(
    implicit ordering: Ordering[T]
  ): BinarySearchTree[T] =
    x match {
      case Nil =>
        x
      case Node(k, _, _, _) if ordering.equiv(t, k) =>
        x
      case Node(k, l, _, _) if ordering.lt(t, k) =>
        treeSearch(l, t)
      case Node(k, _, r, _) if ordering.gt(t, k) =>
        treeSearch(r, t)
    }

  def treeMin[T](x: BinarySearchTree[T]): BinarySearchTree[T] = {
    @tailrec
    def recur(
      xx:  BinarySearchTree[T],
      acc: BinarySearchTree[T]
    ): BinarySearchTree[T] = xx.left match {
      case Nil =>
        acc
      case value: Node[T] =>
        recur(value, value)

    }
    recur(x, x)
  }

  def treeMax[T](x: BinarySearchTree[T]): BinarySearchTree[T] = {
    @tailrec
    def recur(
      xx:  BinarySearchTree[T],
      acc: BinarySearchTree[T]
    ): BinarySearchTree[T] = xx.right match {
      case Nil =>
        acc
      case value =>
        recur(value, value)
    }
    recur(x, x)
  }

  def treeSuccessor[T](x: BinarySearchTree[T])(
    implicit ordering:    Ordering[Option[T]]
  ): BinarySearchTree[T] =
    x.right match {
      case value: Node[T] =>
        treeMin(value)
      case Nil =>
        @tailrec
        def recur(xx: BinarySearchTree[T]): BinarySearchTree[T] =
          xx.parent match {
            case value: Node[T]
                if value.right.key.isDefined && ordering
                  .equiv(value.right.key, xx.key) =>
              recur(value)
            case Nil =>
              xx
          }
        recur(x.parent)
    }

  def treePredecessor[T](x: BinarySearchTree[T])(
    implicit ordering:      Ordering[Option[T]]
  ): BinarySearchTree[T] =
    treeSuccessor(x) // TODO

  def treeInsert[T](x: BinarySearchTree[T], v: T)(
    implicit ordering: Ordering[T]
  ): BinarySearchTree[T] =
    x match {
      case Nil => Node(v, Nil, Nil, x)
      case pp @ Node(k, l, _, _) if ordering.lt(v, k) =>
        pp.copy(k = k, l = treeInsert(l, v))
      case pp @ Node(_, _, r, _) =>
        pp.copy(r = treeInsert(r, v))
    }

  def treeInserts[T](x: BinarySearchTree[T], vv: T*)(
    implicit ordering:  Ordering[T]
  ): BinarySearchTree[T] = vv.toSeq.foldLeft(x) {
    case (xx, v) =>
      treeInsert(xx, v)
  }
}

object TestTree extends App {
  import BinarySearchTree._

  val res  = treeInsert(Nil, 15)
  val res2 = treeInsert(res, 6)
  val res3 = treeInsert(res2, 18)
  println(res3)

  val res4 = treeInsert(treeInsert(treeInsert(treeInsert(Nil, 7), 15), 6), 18)
  println(res4)

  val seq = Seq(15, 6, 3, 7, 2, 4, 13, 9, 18, 17, 20)

  val result = treeInserts(Nil, seq: _*)
  println(result)

  println("inorderTreeWalk=" + inorderTreeWalk(result))

  println("treeMin = " + treeMin(result))
  println("treeMax = " + treeMax(result))

  println("succ=" + treeSuccessor(result))

  println(result.key)

}
