package structures

import cats.syntax.option._

import scala.annotation.tailrec
import scala.collection.immutable

sealed trait BinarySearchTree[+T] {
  def key:    Option[T]
  def left:   Option[BinarySearchTree[T]]
  def right:  Option[BinarySearchTree[T]]
  def parent: Option[BinarySearchTree[T]]
  def root:   Option[BinarySearchTree[T]]
}

case object Nil extends BinarySearchTree[Nothing] {
  override def key:    Option[Nothing]                   = None
  override def left:   Option[BinarySearchTree[Nothing]] = None
  override def right:  Option[BinarySearchTree[Nothing]] = None
  override def parent: Option[BinarySearchTree[Nothing]] = None
  override def root:   Option[BinarySearchTree[Nothing]] = None
}

case class Node[+T](
  k:  T,
  l:  BinarySearchTree[T],
  r:  BinarySearchTree[T],
  p:  BinarySearchTree[T],
  rt: BinarySearchTree[T]
) extends BinarySearchTree[T] {
  override def key:    Option[T]                   = k.some
  override def left:   Option[BinarySearchTree[T]] = l.some
  override def right:  Option[BinarySearchTree[T]] = r.some
  override def parent: Option[BinarySearchTree[T]] = p.some
  override def root:   Option[BinarySearchTree[T]] = rt.some

  def lk = l.key.map(_.toString).getOrElse("*")
  def rk = r.key.map(_.toString).getOrElse("*")

  override def toString: String =
    s"""
       |       ${k}
       |     /    \\
       |    ${lk}      ${rk}
       |""".stripMargin
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
      case Node(k, _, _, _, _) if ordering.equiv(t, k) =>
        x
      case Node(k, l, _, _, _) if ordering.lt(t, k) =>
        treeSearch(l, t)
      case Node(k, _, r, _, _) if ordering.gt(t, k) =>
        treeSearch(r, t)
    }

  @tailrec
  def treeMin[T](x: BinarySearchTree[T]): BinarySearchTree[T] =
    x.left match {
      case Some(value) =>
        treeMin(value)
      case None =>
        x
    }

  @tailrec
  def treeMax[T](x: BinarySearchTree[T]): BinarySearchTree[T] =
    x.right match {
      case Some(value) =>
        treeMax(value)
      case None =>
        x
    }

  def treeSuccessor[T](x: BinarySearchTree[T])(
    implicit ordering:    Ordering[Option[T]]
  ): BinarySearchTree[T] =
    x.right match {
      case Some(value) =>
        treeMin(value)
      case None =>
        @tailrec
        def recur(xx: BinarySearchTree[T]): BinarySearchTree[T] =
          xx.parent match {
            case Some(value)
                if value.right.isDefined && ordering
                  .equiv(value.right.get.key, xx.key) =>
              recur(xx.parent.get)
            case None =>
              xx
          }
        x.parent.map(recur).getOrElse(Nil)
    }

  def treePredecessor[T](x: BinarySearchTree[T])(
    implicit ordering:      Ordering[Option[T]]
  ): BinarySearchTree[T] =
    treeSuccessor(x) // TODO

  def treeInsert[T](x: BinarySearchTree[T], v: T)(
    implicit ordering: Ordering[T]
  ): BinarySearchTree[T] = {
    @tailrec
    def recur(
      xx:  BinarySearchTree[T],
      acc: BinarySearchTree[T]
    ): BinarySearchTree[T] = xx match {
      case Nil =>
        acc
      case Node(k, l, _, _, _) if ordering.lt(v, k) =>
        recur(l, xx)
      case Node(_, _, r, _, _) =>
        recur(r, xx)
    }
    val parent = recur(x, x)
    val z      = Node(v, Nil, Nil, parent, Nil)
    parent match {
      case Nil =>
        z.copy(rt = z)
      case pp @ Node(k, _, _, _, _) if ordering.lt(v, k) =>
        pp.copy(l = z)
      case pp: Node[T] =>
        pp.copy(r = z)
    }
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

  val res = treeInsert(Nil, 15)
  println(res)
  val res2 = treeInsert(res, 6)
  println(res2)
  val res3 = treeInsert(res2, 18)
  println(res3)

  val res4 = treeInsert(treeInsert(treeInsert(treeInsert(Nil, 7), 15), 6), 18)
  println(res4)

  val seq = Seq(15, 6, 3, 7, 2, 4, 13, 9, 18, 17, 20)

  val result = treeInserts(Nil, seq: _*)
  println(result)

}
