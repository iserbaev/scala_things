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
    case RBTNil =>
      List.empty
    case RBTNode(color, k, left, right, parentLabel) =>
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
          case RBTNil =>
            recur(rr, tl)
          case RBTNode(_, _, left, right, _) =>
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
          case RBTNil =>
            recur(rr, tl)
          case RBTNode(_, k, left, right, _) =>
            recur(f(rr, k), left :: right :: tl)
        }
    }

    recur(r, List(this))
  }

  def map[R](f: T => R): RedBlackTree[R] = this match {
    case RBTNil =>
      RBTNil
    case RBTNode(color, k, left, right, parentLabel) =>
      RBTNode(color, f(k), left.map(f), right.map(f), parentLabel.map(f))
  }
}

object RedBlackTree {
  def leftRotate[T](root: RedBlackTree[T], keyToRotate: T)(
    implicit ordering:    Ordering[T]
  ): RedBlackTree[T] = {
    def recur(a: RedBlackTree[T]): RedBlackTree[T] = a match {
      case RBTNil => RBTNil
      case x @ RBTNode(xcolor, xk, xl, xr, xpl)
          if ordering.equiv(xk, keyToRotate) =>
        xr match {
          case RBTNil => x
          case RBTNode(ycolor, yk, yl, yr, _) =>
            yl match {
              case RBTNil =>
                RBTNode(
                  ycolor,
                  yk,
                  RBTNode(xcolor, xk, xl, RBTNil, Some(yk)),
                  yr,
                  xpl
                )
              case yln @ RBTNode(_, _, _, _, _) =>
                RBTNode(
                  ycolor,
                  yk,
                  RBTNode(
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
      case RBTNode(color, k, l, r, parentLabel)
          if ordering.lt(k, keyToRotate) =>
        RBTNode(color, k, l, recur(r), parentLabel)
      case RBTNode(color, k, l, r, parentLabel) =>
        RBTNode(color, k, recur(l), r, parentLabel)
    }

    recur(root)
  }

  def rightRotate[T](root: RedBlackTree[T], keyToRotate: T)(
    implicit ordering:     Ordering[T]
  ): RedBlackTree[T] = {
    def recur(a: RedBlackTree[T]): RedBlackTree[T] = a match {
      case RBTNil => RBTNil
      case y @ RBTNode(ycolor, yk, yl, yr, yparentLabel)
          if ordering.equiv(keyToRotate, yk) =>
        yl match {
          case RBTNil => y
          case RBTNode(xcolor, xk, xl, xr, _) =>
            xr match {
              case RBTNil =>
                RBTNode(
                  xcolor,
                  xk,
                  xl,
                  RBTNode(ycolor, yk, RBTNil, yr, Some(xk)),
                  yparentLabel
                )
              case xrn @ RBTNode(_, _, _, _, _) =>
                RBTNode(
                  xcolor,
                  xk,
                  xl,
                  RBTNode(
                    ycolor,
                    yk,
                    xrn.copy(parentLabel = Some(yk)),
                    yr,
                    Some(xk)
                  ),
                  yparentLabel
                )
            }
        }

      case RBTNode(color, xk, xl, xr, parentLabel)
          if ordering.lt(keyToRotate, xk) =>
        RBTNode(color, xk, recur(xl), xr, parentLabel)
      case RBTNode(color, xk, xl, xr, parentLabel)
          if ordering.gt(keyToRotate, xk) =>
        RBTNode(color, xk, xl, recur(xr), parentLabel)
    }

    recur(root)
  }
}

case object RBTNil extends RedBlackTree[Nothing] {
  override def color:       RBColor               = Black
  override def key:         Option[Nothing]       = None
  override def left:        RedBlackTree[Nothing] = RBTNil
  override def right:       RedBlackTree[Nothing] = RBTNil
  override def parentLabel: Option[Nothing]       = None
}

case class RBTNode[+T](
  color:       RBColor,
  k:           T,
  left:        RedBlackTree[T],
  right:       RedBlackTree[T],
  parentLabel: Option[T]
) extends RedBlackTree[T] {
  override def key: Option[T] = Some(k)
}

object TestApp extends App {
  val testTree = RBTNode(
    Black,
    8,
    RBTNode(
      Red,
      4,
      RBTNode(
        Black,
        2,
        RBTNode(
          Red,
          1,
          RBTNil,
          RBTNil,
          Some(2)
        ),
        RBTNode(
          Red,
          3,
          RBTNil,
          RBTNil,
          Some(2)
        ),
        Some(4)
      ),
      RBTNode(
        Black,
        6,
        RBTNode(
          Red,
          5,
          RBTNil,
          RBTNil,
          Some(6)
        ),
        RBTNode(
          Red,
          7,
          RBTNil,
          RBTNil,
          Some(6)
        ),
        Some(4)
      ),
      Some(8)
    ),
    RBTNode(
      Red,
      12,
      RBTNode(
        Black,
        10,
        RBTNode(
          Red,
          9,
          RBTNil,
          RBTNil,
          Some(10)
        ),
        RBTNode(
          Red,
          11,
          RBTNil,
          RBTNil,
          Some(10)
        ),
        Some(12)
      ),
      RBTNode(
        Black,
        14,
        RBTNode(
          Red,
          13,
          RBTNil,
          RBTNil,
          Some(14)
        ),
        RBTNode(
          Red,
          15,
          RBTNil,
          RBTNil,
          Some(14)
        ),
        Some(12)
      ),
      Some(8)
    ),
    None
  )

  val validate = testTree.validate
  println(validate)

  val result = RedBlackTree.leftRotate(testTree, 4)
  println(result)
}
