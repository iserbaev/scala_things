sealed abstract class BTree[+T] {
  def value:   T
  def left:    BTree[T]
  def right:   BTree[T]
  def isEmpty: Boolean

  /**
    * Easy problems
    */
  def isLeaf:        Boolean
  def collectLeaves: List[BTree[T]]
  def leafCount:     Int

  /**
    * Medium difficulty problems
    */
  // the number of nodes in the tree
  def size: Int

  // nodes at a given level
  def collectNodes(level: Int): List[BTree[T]]

  // mirror a tree
  /*
        _____1_____                     _____1_____
       /           \                   /           \
     __2__       __6__       ->      __6__       __2__
    /     \     /     \             /     \     /     \
    3     4     7     8             8     7     4     3
           \                                   /
            5                                 5
   */
  def mirror: BTree[T]

  // compare the shape of two trees
  /*
        _____1_____                     _____8_____
       /           \                   /           \
     __2__       __6__       ~~      __9__       __2__
    /     \     /     \             /     \     /     \
    3     4     7     8             1     3     2     7
           \                               \
            5                               4
   */
  def sameShapeAs[S >: T](that: BTree[S]): Boolean

  // tree is symmetrical with respect to the root node
  /*
        _____1_____
       /           \
     __2__       __6__
    /     \     /     \
    3     4     7     8
   */
  def isSymmetrical: Boolean
}

case object BNil extends BTree[Nothing] {
  override def value = throw new NoSuchElementException

  override def left = throw new NoSuchElementException

  override def right = throw new NoSuchElementException

  override def isEmpty = true

  /**
    * Easy problems
    */
  override def isLeaf = false

  override def collectLeaves = List.empty[BTree[Nothing]]

  override def leafCount = 0

  /**
    * Medium difficulty problems
    */
  override def size = 0

  override def collectNodes(level: Int) = List.empty[BTree[Nothing]]

  override def mirror = BNil

  override def sameShapeAs[S >: Nothing](that: BTree[S]) = that.isEmpty

  override def isSymmetrical = true
}

case class BNode[+T](value: T, left: BTree[T], right: BTree[T])
    extends BTree[T] {
  override def isEmpty = false

  /**
    * Easy problems
    */
  override def isLeaf = left.isEmpty && right.isEmpty

  override def collectLeaves = {
    @scala.annotation.tailrec
    def clt(
      t:    BTree[T],
      pred: List[BTree[T]],
      acc:  List[BTree[T]]
    ): List[BTree[T]] =
      if (t.isLeaf) {
        pred match {
          case Nil          => t :: acc
          case ::(head, tl) => clt(head, tl, t :: acc)
        }
      } else {
        (t.left, t.right) match {
          case (l, r) if l.isLeaf => clt(r, pred, l :: acc)
          case (l, r) if r.isLeaf => clt(l, pred, r :: acc)
          case (l, r) =>
            clt(r, l :: pred, acc)
        }
      }
    clt(this, List.empty, List.empty)
  }

  override def leafCount =
    (if (isLeaf) 1 else 0) + left.leafCount + right.leafCount

  /**
    * Medium difficulty problems
    */
  override def size = 1 + left.size + right.size

  override def collectNodes(level: Int) =
    if (level == 0) {
      if (isLeaf) List.empty else List(this)
    } else left.collectNodes(level - 1) ++ right.collectNodes(level - 1)

  override def mirror: BTree[T] = BNode(value, right.mirror, left.mirror)

  override def sameShapeAs[S >: T](that: BTree[S]): Boolean = {
    @scala.annotation.tailrec
    def recur(thisRem: List[BTree[S]], thatRem: List[BTree[S]]): Boolean =
      if (thisRem.isEmpty) thatRem.isEmpty
      else if (thatRem.isEmpty) thisRem.isEmpty
      else {
        (thisRem.head, thatRem.head) match {
          case (a, b) if a.isEmpty =>
            b.isEmpty && recur(thisRem.tail, thatRem.tail)
          case (a, b) if a.isLeaf =>
            b.isLeaf && recur(thisRem.tail, thatRem.tail)
          case (a, b) =>
            recur(
              a.left :: a.right :: thisRem.tail,
              b.left :: b.right :: thatRem.tail
            )
        }
      }

    recur(List(this), List(that))
  }

  override def isSymmetrical = sameShapeAs(this.mirror)
}
