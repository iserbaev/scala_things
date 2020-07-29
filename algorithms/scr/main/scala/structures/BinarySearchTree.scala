package structures

import cats.syntax.option._

import scala.annotation.tailrec
import scala.collection.immutable

sealed trait BinarySearchTree[+T] {
  def key:    Option[T]
  def left:   Option[BinarySearchTree[T]]
  def right:  Option[BinarySearchTree[T]]
  def parent: Option[BinarySearchTree[T]]
}

case object Nil extends BinarySearchTree[Nothing] {
  override def key:    Option[Nothing]                   = None
  override def left:   Option[BinarySearchTree[Nothing]] = None
  override def right:  Option[BinarySearchTree[Nothing]] = None
  override def parent: Option[BinarySearchTree[Nothing]] = None
}

case class Node[+T](
  k: T,
  l: BinarySearchTree[T],
  r: BinarySearchTree[T],
  p: BinarySearchTree[T]
) extends BinarySearchTree[T] {
  override def key:    Option[T]                   = k.some
  override def left:   Option[BinarySearchTree[T]] = l.some
  override def right:  Option[BinarySearchTree[T]] = r.some
  override def parent: Option[BinarySearchTree[T]] = p.some
}

object BinarySearchTree {
  def inorderTreeWalk[T](x: BinarySearchTree[T]): List[T] = {
    @tailrec
    def recur(ts: List[BinarySearchTree[T]], acc: List[T]): List[T] = ts match {
      case immutable.Nil => acc
      case ::(head, tl) =>
        recur(
          List(head.left, head.right).flatten ++ tl,
          head.key.map(_ :: acc).getOrElse(acc)
        )
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

//  def treeMin[T](x:    BinarySearchTree[T])(
//    implicit ordering: Ordering[T]
//  ): BinarySearchTree[T] = {}
}
