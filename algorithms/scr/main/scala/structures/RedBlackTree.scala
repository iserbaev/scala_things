package structures

import scala.annotation.tailrec
import scala.collection.immutable

sealed trait RBColor {
  def isBlack: Boolean = this match {
    case Red   => false
    case Black => true
  }

  def isRed: Boolean = !isBlack
}
case object Red extends RBColor
case object Black extends RBColor

sealed trait RedBlackTree[+T] {
  def color:       RBColor
  def key:         Option[T]
  def left:        RedBlackTree[T]
  def right:       RedBlackTree[T]
  def parentLabel: Option[T]

  /**
    * 1. Each Node can be red or black
    * 2. Root must be black
    * 3. Each Nil black
    * 4. if Node is red then children must be black
    * 5. for each Node all simple paths must contain same black nodes
    * @return
    */
  private def validateCurrent: List[String] = this match {
    case Nil =>
      List.empty
    case Node(color, k, left, right, parentLabel) =>
      val second = Either.cond(
        (parentLabel.isEmpty && color.isBlack) || parentLabel.isDefined,
        s"$k - 2 passed",
        s"$k - Root must be black"
      )
      val fourth = Either.cond(
        (color.isRed && left.color.isBlack && right.color.isBlack) || color.isBlack,
        s"$k - 4 passed",
        s"$k - if Node is red then children must be black"
      )

      List(second.merge, fourth.merge)
  }

  def validate: List[String] =
    foldTree(List.empty[String])(_ ++ _.validateCurrent)

  def foldTree[R](r: R)(f: (R, RedBlackTree[T]) => R): R = {
    @tailrec
    def recur(rr: R, acc: List[RedBlackTree[T]]): R = acc match {
      case immutable.Nil =>
        rr
      case ::(head, tl) =>
        head match {
          case Nil =>
            recur(rr, tl)
          case Node(_, _, left, right, _) =>
            recur(f(rr, head), left :: right :: tl)
        }
    }

    recur(r, List(this))
  }

  def fold[R](r: R)(f: (R, T) => R): R = {
    @tailrec
    def recur(rr: R, acc: List[RedBlackTree[T]]): R = acc match {
      case immutable.Nil =>
        rr
      case ::(head, tl) =>
        head match {
          case Nil =>
            recur(rr, tl)
          case Node(_, k, left, right, _) =>
            recur(f(rr, k), left :: right :: tl)
        }
    }

    recur(r, List(this))
  }

  def map[R](f: T => R): RedBlackTree[R] = this match {
    case Nil =>
      Nil
    case Node(color, k, left, right, parentLabel) =>
      Node(color, f(k), left.map(f), right.map(f), parentLabel.map(f))
  }
}

object RedBlackTree {
  def leftRotate[T](root: RedBlackTree[T], keyToRotate: T)(
    implicit ordering:    Ordering[T]
  ): RedBlackTree[T] = {
    @tailrec
    def recur(a: RedBlackTree[T]): RedBlackTree[T] = a match {
      case Nil => Nil
      case x @ Node(xcolor, xk, xl, xr, xpl)
          if ordering.equiv(xk, keyToRotate) =>
        xr match {
          case Nil => x
          case Node(ycolor, yk, yl, yr, _) =>
            yl match {
              case Nil =>
                Node(ycolor, yk, Node(xcolor, xk, xl, Nil, Some(yk)), yr, xpl)
              case yln @ Node(_, _, _, _, _) =>
                Node(
                  ycolor,
                  yk,
                  Node(
                    xcolor,
                    xk,
                    xl,
                    yln.copy(parentLabel = Some(xk)),
                    Some(yk)
                  ),
                  yr,
                  xpl
                )
            }

        }
      case Node(_, lk, _, right, _) if ordering.lt(lk, keyToRotate) =>
        recur(right)
      case Node(_, _, left, _, _) =>
        recur(left)
    }

    recur(root)
  }
}

case object Nil extends RedBlackTree[Nothing] {
  override def color:       RBColor               = Black
  override def key:         Option[Nothing]       = None
  override def left:        RedBlackTree[Nothing] = Nil
  override def right:       RedBlackTree[Nothing] = Nil
  override def parentLabel: Option[Nothing]       = None
}

case class Node[+T](
  color:       RBColor,
  k:           T,
  left:        RedBlackTree[T],
  right:       RedBlackTree[T],
  parentLabel: Option[T]
) extends RedBlackTree[T] {
  override def key: Option[T] = Some(k)
}
