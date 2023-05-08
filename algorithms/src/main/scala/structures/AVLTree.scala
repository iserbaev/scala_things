package structures

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** An immutable AVL Tree implementation formerly used by mutable.TreeSet
  *
  * @author
  *   Lucien Pereira
  */
sealed trait AVLTree[+A] extends Iterable[A] with Serializable {
  import AVLTree._

  def balance: Int

  def depth: Int

  override def foreach[U](f: A => U): Unit = {
    val i = iterator
    while (i.hasNext) {
      val elem = i.next()
      f(elem)
    }
  }

  def iterator: Iterator[A] =
    Iterator.empty

  def contains[B >: A](value: B, ordering: Ordering[B]): Boolean =
    false

  def find[B >: A](value: B, ordering: Ordering[B]): AVLTree[B] =
    Leaf

  /** Returns a new tree containing the given element. Throws an IllegalArgumentException
    * if element is already present.
    */
  def insert[B >: A](value: B, ordering: Ordering[B]): AVLTree[B] = {
    val _ = ordering
    Node(value, Leaf, Leaf)
  }

  /** Return a new tree which not contains given element. */
  def remove[B >: A](value: B, ordering: Ordering[B]): AVLTree[A] =
    throw new NoSuchElementException(String.valueOf(value))

  /** Return a tuple containing the smallest element of the provided tree and a new tree
    * from which this element has been extracted.
    */
  def removeMin[B >: A]: (B, AVLTree[B]) =
    sys.error("Should not happen.")

  /** Return a tuple containing the biggest element of the provided tree and a new tree
    * from which this element has been extracted.
    */
  def removeMax[B >: A]: (B, AVLTree[B]) =
    sys.error("Should not happen.")

  def rebalance[B >: A]: AVLTree[B] =
    this

  def leftRotation[B >: A]: Node[B] =
    sys.error("Should not happen.")

  def rightRotation[B >: A]: Node[B] =
    sys.error("Should not happen.")

  def doubleLeftRotation[B >: A]: Node[B] =
    sys.error("Should not happen.")

  def doubleRightRotation[B >: A]: Node[B] =
    sys.error("Should not happen.")
}

object AVLTree {
  case object Leaf extends AVLTree[Nothing] {
    override val balance: Int = 0

    override val depth: Int = -1
  }

  case class Node[A](data: A, left: AVLTree[A], right: AVLTree[A]) extends AVLTree[A] {
    override val balance: Int =
      right.depth - left.depth

    override val depth: Int =
      math.max(left.depth, right.depth) + 1

    override def iterator: Iterator[A] =
      new AVLIterator(this)

    override def contains[B >: A](value: B, ordering: Ordering[B]): Boolean = {
      val ord = ordering.compare(value, data)
      if (0 == ord)
        true
      else if (ord < 0)
        left.contains(value, ordering)
      else
        right.contains(value, ordering)
    }

    override def find[B >: A](value: B, ordering: Ordering[B]): AVLTree[B] = {
      val ord = ordering.compare(value, data)
      if (0 == ord)
        this
      else if (ord < 0)
        left.find(value, ordering)
      else
        right.find(value, ordering)
    }

    /** Returns a new tree containing the given element. Thows an IllegalArgumentException
      * if element is already present.
      */
    override def insert[B >: A](value: B, ordering: Ordering[B]): AVLTree[B] = {
      val ord = ordering.compare(value, data)
      // if (0 == ord)
      //   throw new IllegalArgumentException()
      if (ord <= 0)
        Node(data, left.insert(value, ordering), right).rebalance
      else
        Node(data, left, right.insert(value, ordering)).rebalance
    }

    /** Return a new tree which not contains given element. */
    override def remove[B >: A](value: B, ordering: Ordering[B]): AVLTree[A] = {
      val ord = ordering.compare(value, data)
      if (ord == 0) {
        if (Leaf == left) {
          if (Leaf == right) {
            Leaf
          } else {
            val (min, newRight) = right.removeMin
            Node(min, left, newRight).rebalance
          }
        } else {
          val (max, newLeft) = left.removeMax
          Node(max, newLeft, right).rebalance
        }
      } else if (ord < 0) {
        Node(data, left.remove(value, ordering), right).rebalance
      } else {
        Node(data, left, right.remove(value, ordering)).rebalance
      }
    }

    /** Return a tuple containing the smallest element of the provided tree and a new tree
      * from which this element has been extracted.
      */
    override def removeMin[B >: A]: (B, AVLTree[B]) =
      if (Leaf == left)
        (data, right)
      else {
        val (min, newLeft) = left.removeMin
        (min, Node(data, newLeft, right).rebalance)
      }

    /** Return a tuple containing the biggest element of the provided tree and a new tree
      * from which this element has been extracted.
      */
    override def removeMax[B >: A]: (B, AVLTree[B]) =
      if (Leaf == right)
        (data, left)
      else {
        val (max, newRight) = right.removeMax
        (max, Node(data, left, newRight).rebalance)
      }

    override def rebalance[B >: A]: AVLTree[B] =
      if (-2 == balance) {
        if (1 == left.balance)
          doubleRightRotation
        else
          rightRotation
      } else if (2 == balance) {
        if (-1 == right.balance)
          doubleLeftRotation
        else
          leftRotation
      } else {
        this
      }

    override def leftRotation[B >: A] =
      if (Leaf != right) {
        val r: Node[A] = right.asInstanceOf[Node[A]]
        Node(r.data, Node(data, left, r.left), r.right)
      } else sys.error("Should not happen.")

    override def rightRotation[B >: A] =
      if (Leaf != left) {
        val l: Node[A] = left.asInstanceOf[Node[A]]
        Node(l.data, l.left, Node(data, l.right, right))
      } else sys.error("Should not happen.")

    override def doubleLeftRotation[B >: A] =
      if (Leaf != right) {
        val r: Node[A] = right.asInstanceOf[Node[A]]
        // Let's save an instanceOf by 'inlining' the left rotation
        val rightRotated = r.rightRotation
        Node(
          rightRotated.data,
          Node(data, left, rightRotated.left),
          rightRotated.right
        )
      } else sys.error("Should not happen.")

    override def doubleRightRotation[B >: A] =
      if (Leaf != left) {
        val l: Node[A] = left.asInstanceOf[Node[A]]
        // Let's save an instanceOf by 'inlining' the right rotation
        val leftRotated = l.leftRotation
        Node(
          leftRotated.data,
          leftRotated.left,
          Node(data, leftRotated.right, right)
        )
      } else sys.error("Should not happen.")
  }

  class AVLIterator[A](root: Node[A]) extends Iterator[A] {
    private val stack = mutable.Stack[Node[A]](root)
    diveLeft()

    private def diveLeft(): Unit =
      if (Leaf != stack.head.left) {
        val left: Node[A] = stack.head.left.asInstanceOf[Node[A]]
        stack.push(left)
        diveLeft()
      }

    private def engageRight(): Unit =
      if (Leaf != stack.head.right) {
        val right: Node[A] = stack.head.right.asInstanceOf[Node[A]]
        stack.pop()
        stack.push(right)
        diveLeft()
      } else {
        stack.pop()
        ()
      }

    override def hasNext: Boolean = !stack.isEmpty

    override def next(): A =
      if (stack.isEmpty)
        throw new NoSuchElementException()
      else {
        val result = stack.head.data
        // Let's maintain stack for the next invocation
        engageRight()
        result
      }
  }

  def preOrder[T](b: AVLTree[T])(runNode: AVLTree[T] => Unit): Unit =
    b match {
      case Leaf =>
        ()
      case n @ Node(_, left, right) =>
        runNode(n)
        preOrder(left)(runNode)
        preOrder(right)(runNode)
    }

  def inOrder[T](b: AVLTree[T])(runNode: AVLTree[T] => Unit): Unit =
    b match {
      case Leaf =>
        ()
      case n @ Node(_, left, right) =>
        inOrder(left)(runNode)
        runNode(n)
        inOrder(right)(runNode)
    }

  def inOrderTraversal[T](
      b: AVLTree[T],
      acc: ListBuffer[T]
  ): ListBuffer[T] =
    b match {
      case Leaf =>
        acc
      case n @ Node(_, left, right) =>
        inOrderTraversal(left, acc)
        acc.append(n.data)
        inOrderTraversal(right, acc)
    }

  def postOrder[T](b: AVLTree[T])(runNode: AVLTree[T] => Unit): Unit =
    b match {
      case Leaf =>
        ()
      case n @ Node(_, left, right) =>
        postOrder(left)(runNode)
        postOrder(right)(runNode)
        runNode(n)
    }
}
