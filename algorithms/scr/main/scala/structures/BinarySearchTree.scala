package structures

import cats.syntax.option._

import scala.annotation.tailrec
import scala.collection.immutable

sealed trait BinarySearchTree[+T] {
  def key:         Option[T]
  def left:        BinarySearchTree[T]
  def right:       BinarySearchTree[T]
  def parentLabel: Option[T]
}

case object Nil extends BinarySearchTree[Nothing] {
  override def key: Option[Nothing] = None

  override def left:        BinarySearchTree[Nothing] = Nil
  override def right:       BinarySearchTree[Nothing] = Nil
  override def parentLabel: Option[Nothing]           = None
}

case class Node[+T](
  k:           T,
  left:        BinarySearchTree[T],
  right:       BinarySearchTree[T],
  parentLabel: Option[T]
) extends BinarySearchTree[T] {
  override def key: Option[T] = k.some
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

  // O(log n)
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

  // O(log n)
  def parent[T](root:  BinarySearchTree[T], parentLabel: Option[T])(
    implicit ordering: Ordering[T]
  ): BinarySearchTree[T] =
    parentLabel.map(BinarySearchTree.treeSearch(root, _)).getOrElse(Nil)

  def treeSuccessor[T](x: BinarySearchTree[T], root: BinarySearchTree[T])(
    implicit opt:         Ordering[Option[T]],
    tOrd:                 Ordering[T]
  ): BinarySearchTree[T] =
    x.right match {
      case value: Node[T] =>
        treeMin(value)
      case Nil =>
        @tailrec
        def recur(
          y:  BinarySearchTree[T],
          xx: BinarySearchTree[T]
        ): BinarySearchTree[T] =
          parent(root, y.parentLabel) match {
            case yp: Node[T] if y.right.key == xx.key =>
              recur(yp, y)
            case _ =>
              y
          }
        recur(parent(root, x.parentLabel), x)
    }

  def treePredecessor[T](x: BinarySearchTree[T], root: BinarySearchTree[T])(
    implicit opt:           Ordering[Option[T]],
    tOrd:                   Ordering[T]
  ): BinarySearchTree[T] =
    x.left match {
      case value: Node[T] =>
        treeMax(value)
      case Nil =>
        @tailrec
        def recur(
          y:  BinarySearchTree[T],
          xx: BinarySearchTree[T]
        ): BinarySearchTree[T] =
          parent(root, y.parentLabel) match {
            case yp: Node[T] if y.left.key == xx.key =>
              recur(yp, y)
            case _ =>
              y
          }
        recur(parent(root, x.parentLabel), x)
    }

  def treeInsert[T](
    x:           BinarySearchTree[T],
    v:           T,
    parentLabel: Option[T] = None
  )(
    implicit ordering: Ordering[T]
  ): BinarySearchTree[T] =
    x match {
      case Nil => Node(v, Nil, Nil, parentLabel)
      case pp @ Node(k, l, _, _) if ordering.lt(v, k) =>
        pp.copy(left = treeInsert(l, v, pp.key))
      case pp @ Node(_, _, r, _) =>
        pp.copy(right = treeInsert(r, v, pp.key))
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

  println("treeSearch 15 = " + treeSearch(result, 15))
  println("treeSearch 2 = " + treeSearch(result, 2))
  println("treeSearch 17 = " + treeSearch(result, 17))
  println("treeSearch 6 = " + treeSearch(result, 6))
  println("treeSearch 20 = " + treeSearch(result, 20))

  println("parent 6 == 15 =" + parent(result, result.left.parentLabel))
  println("parent 3 == 6 =" + parent(result, result.left.left.parentLabel))
  println("parent 15 == Nil =" + parent(result, result.key))

  println("succ 15 == 17 =" + treeSuccessor(result, result))
  println("succ 13 == 15 =" + treeSuccessor(result.left.right.right, result))
  println("pred 15 == 13 =" + treePredecessor(result, result))
  println("pred 7 == 6 =" + treePredecessor(result.left.right, result))
  println("pred 17 == 15 =" + treePredecessor(result.right.left, result))

}
