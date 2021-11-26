package structures

import structures.BinarySearchTree.{BNil, Node}

import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

sealed trait BinarySearchTree[+T] {
  def key:         Option[T]
  def left:        BinarySearchTree[T]
  def right:       BinarySearchTree[T]
  def parentLabel: Option[T]
  def size:        Int

  def map[R](f: T => R): BinarySearchTree[R] = this match {
    case BNil =>
      BNil
    case Node(k, left, right, parentLabel) =>
      Node(f(k), left.map(f), right.map(f), parentLabel.map(f))
  }

  def fold[R](r: R)(f: (R, T) => R): R = {
    @tailrec
    def recur(rr: R, acc: List[BinarySearchTree[T]]): R = acc match {
      case Nil =>
        rr
      case ::(head, tl) =>
        head match {
          case BNil =>
            recur(rr, tl)
          case Node(k, left, right, _) =>
            recur(f(rr, k), left :: right :: tl)
        }
    }

    recur(r, List(this))
  }

  def fold2[R](r: R)(f: (R, T) => R): R = {
    // ! not tailrec
    def recur(rr: R, bb: BinarySearchTree[T]): R = bb match {
      case BNil =>
        rr
      case Node(k, left, right, _) =>
        recur(f(recur(rr, left), k), right)
    }

    recur(r, this)
  }
}

object BinarySearchTree {
  case object BNil extends BinarySearchTree[Nothing] {
    override def key: Option[Nothing] = None

    override def left:        BinarySearchTree[Nothing] = BNil
    override def right:       BinarySearchTree[Nothing] = BNil
    override def parentLabel: Option[Nothing]           = None
    override def size:        Int                       = 0
  }

  case class Node[+T](
    k:           T,
    left:        BinarySearchTree[T],
    right:       BinarySearchTree[T],
    parentLabel: Option[T]
  ) extends BinarySearchTree[T] {
    override def key:  Option[T] = Some(k)
    override def size: Int       = 1 + left.size + right.size
  }

  def preOrder[T](b: BinarySearchTree[T])(runNode: Node[T] => Unit): Unit =
    b match {
      case BNil =>
        ()
      case n @ Node(_, left, right, _) =>
        runNode(n)
        preOrder(left)(runNode)
        preOrder(right)(runNode)
    }

  def inOrder[T](b: BinarySearchTree[T])(runNode: Node[T] => Unit): Unit =
    b match {
      case BNil =>
        ()
      case n @ Node(_, left, right, _) =>
        inOrder(left)(runNode)
        runNode(n)
        inOrder(right)(runNode)
    }

  def inOrderTraversal[T](
    b:   BinarySearchTree[T],
    acc: ListBuffer[T]
  ): ListBuffer[T] =
    b match {
      case BNil =>
        acc
      case n @ Node(_, left, right, _) =>
        inOrderTraversal(left, acc)
        acc.append(n.k)
        inOrderTraversal(right, acc)
    }

  def postOrder[T](b: BinarySearchTree[T])(runNode: Node[T] => Unit): Unit =
    b match {
      case BNil =>
        ()
      case n @ Node(_, left, right, _) =>
        postOrder(left)(runNode)
        postOrder(right)(runNode)
        runNode(n)
    }

  def inorderTreeWalk[T](x: BinarySearchTree[T]): List[T] =
    inOrderTraversal(x, ListBuffer.empty).toList

  def isValid[T](x:    BinarySearchTree[T])(
    implicit ordering: Ordering[T]
  ): Boolean = {
    val isValid = ArrayBuffer(true)
    inOrderValidate(x, ArrayBuffer[BinarySearchTree[T]](BNil), isValid)
    isValid.head
  }

  def inOrderValidate[T](
    root:    BinarySearchTree[T],
    prev:    ArrayBuffer[BinarySearchTree[T]],
    isValid: ArrayBuffer[Boolean]
  )(
    implicit ordering: Ordering[T]
  ): Unit =
    root match {
      case BNil =>
        ()
      case Node(k, left, right, _) =>
        inOrderValidate(left, prev, isValid)
        if (prev.head.key.nonEmpty && ordering.gteq(prev.head.key.get, k)) {
          isValid.update(0, false)
        }
        prev.update(0, root)
        inOrderValidate(right, prev, isValid)
    }

  // O(log n)
  @tailrec
  def treeSearch[T](x: BinarySearchTree[T], t: T)(
    implicit ordering: Ordering[T]
  ): BinarySearchTree[T] =
    x match {
      case BNil =>
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
      case BNil =>
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
      case BNil =>
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
    parentLabel.map(BinarySearchTree.treeSearch(root, _)).getOrElse(BNil)

  def treeSuccessor[T](x: BinarySearchTree[T], root: BinarySearchTree[T])(
    implicit opt:         Ordering[Option[T]],
    tOrd:                 Ordering[T]
  ): BinarySearchTree[T] =
    x.right match {
      case value: Node[T] =>
        treeMin(value)
      case BNil =>
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
      case BNil =>
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
    v:           BinarySearchTree[T],
    parentLabel: Option[T] = None
  )(
    implicit ordering: Ordering[T]
  ): BinarySearchTree[T] =
    (x, v) match {
      case (BNil, BNil) =>
        BNil
      case (BNil, n @ Node(_, _, _, _)) =>
        n.copy(parentLabel = parentLabel)
      case (n @ Node(_, _, _, _), BNil) =>
        n.copy(parentLabel = parentLabel)
      case (pp @ Node(pk, pl, _, _), n @ Node(nk, _, _, _))
          if ordering.lt(nk, pk) =>
        pp.copy(left = treeInsert(pl, n, pp.key), parentLabel = parentLabel)
      case (pp @ Node(_, _, r, _), n @ Node(_, _, _, _)) =>
        pp.copy(right = treeInsert(r, n, pp.key), parentLabel = parentLabel)
    }

  def treeInserts[T](x: BinarySearchTree[T], vv: T*)(
    implicit ordering:  Ordering[T]
  ): BinarySearchTree[T] = vv.toSeq.foldLeft(x) {
    case (xx, v) =>
      treeInsert(xx, Node(v, BNil, BNil, None))
  }

  def treeDelete[T](x: BinarySearchTree[T], v: T)(
    implicit ordering: Ordering[T]
  ): BinarySearchTree[T] = {
//    @tailrec
    def recur(xx: BinarySearchTree[T]): BinarySearchTree[T] = xx match {
      case BNil =>
        xx
      case Node(zk, zl, zr, parentLabel) if ordering.equiv(zk, v) =>
        (zl, zr) match {
          case (BNil, BNil) =>
            BNil
          case (BNil, node @ Node(_, _, _, _)) =>
            node.copy(parentLabel = parentLabel)
          case (node @ Node(_, _, _, _), BNil) =>
            node.copy(parentLabel = parentLabel)
          case (l @ Node(_, _, _, _), r @ Node(rk, rl, rr, _)) =>
            rl match {
              case BNil =>
                Node(rk, l.copy(parentLabel = Some(rk)), rr, parentLabel)
              case Node(yk, yl, yr, _) =>
                val tree = Node(
                  yk,
                  l.copy(parentLabel = Some(yk)),
                  r.copy(parentLabel = Some(yk)),
                  parentLabel
                )
                val t2 = treeInsert(tree, yl, parentLabel)
                treeInsert(t2, yr, parentLabel)
            }

        }
      case Node(zk, zl, zr, parentLabel) if ordering.lt(zk, v) =>
        Node(zk, zl, recur(zr), parentLabel)
      case Node(zk, zl, zr, parentLabel) if ordering.gt(zk, v) =>
        Node(zk, recur(zl), zr, parentLabel)
    }

    recur(x)
  }
}

object TestTree extends App {
  import BinarySearchTree._

  def test(): Unit = {

    /**
      *                        15
      *                     /     \
      *                    6      18
      *                   / \    /  \
      *                  3   7  17   20
      *                 / \   \
      *                2   4  13
      *                      /
      *                     9
      */
    val seq    = Seq(15, 6, 3, 7, 2, 4, 13, 9, 18, 17, 20)
    val result = treeInserts(BNil, seq: _*)

    assert(inorderTreeWalk(result).forall(seq.contains))

    assert(treeMin(result).key.get == seq.min)
    assert(treeMax(result).key.get == seq.max)

    seq.foreach(i => assert(treeSearch(result, i).key.get == i))

    assert(parent(result, result.left.parentLabel).key.get == 15)
    assert(parent(result, result.left.left.parentLabel).key.get == 6)
    assert(parent(result, result.parentLabel).key.isEmpty)

    assert(treeSuccessor(result, result).key.get == 17)
    assert(treeSuccessor(result.left.right.right, result).key.get == 15)
    assert(treePredecessor(result, result).key.get == 13)
    assert(treePredecessor(result.left.right, result).key.get == 6)
    assert(treePredecessor(result.right.left, result).key.get == 15)

    assert(seq.sum == result.fold(0)(_ + _))
    assert(seq.product == result.fold(1)(_ * _))
    assert(seq.sum == result.fold2(0)(_ + _))
    assert(seq.product == result.fold2(1)(_ * _))

    assert(result.map(_ + 1).fold(0)(_ + _) == seq.map(_ + 1).sum)

    assert(treeDelete(result, 3).fold(0)(_ + _) == (seq.sum - 3))

    val resW15 = treeDelete(result, 15)
    assert(treeSearch(resW15, 15).key.isEmpty)
    println(resW15)
  }

  test()

}
