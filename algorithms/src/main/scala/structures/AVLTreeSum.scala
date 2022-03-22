package structures

// реализация из лекций
sealed trait AVLTreeSum {
  def left:      AVLTreeSum
  def right:     AVLTreeSum
  def parentKey: Option[Int]
  def level:     Int
  def depth:     Int
  def size:      Int
  def sum:       Int

  def insert(k: Int): Unit
  def find(k:   Int): AVLTreeSum
  def remove(k: Int): AVLTreeSum
  def min:  Int
  def max:  Int
  def next: Int
  def prev: Int

  def sumInSeqInclusive(l: Int, r: Int): Int
}

object AVLTreeSum {
  case class AVLNode(
    left:      AVLTreeSum,
    right:     AVLTreeSum,
    key:       Int,
    level:     Int,
    parentKey: Option[Int]
  ) extends AVLTreeSum {
    val depth: Int = 1 + math.max(left.depth, right.depth)
    val size:  Int = 1 + left.size + right.size

    val sum: Int = ???

    override def insert(k: Int): Unit = ???

    override def find(k: Int): AVLTreeSum = ???

    override def remove(k: Int): AVLTreeSum = ???

    val min: Int = math.min(key, left.min)
    val max: Int = math.max(key, right.max)

    override def next: Int = right match {
      case _: AVLNode => right.min
      case _: AVLNil  => ???
    }
    override def prev: Int = ???

    override def sumInSeqInclusive(l: Int, r: Int): Int = ???
  }

  case class AVLNil(parentKey: Option[Int]) extends AVLTreeSum {
    val left:  AVLTreeSum = AVLNil(None)
    val right: AVLTreeSum = AVLNil(None)

    val depth: Int = -1
    val size:  Int = 0

    val sum: Int = ???

    override def insert(k: Int): Unit = ???

    override def find(k: Int): AVLTreeSum = ???

    override def remove(k: Int): AVLTreeSum = ???

    val min: Int = Int.MaxValue
    val max: Int = Int.MinValue

    override def next: Int = ???

    override def prev: Int = ???

    override def sumInSeqInclusive(l: Int, r: Int): Int = ???
  }

  def orderStatistics(v: AVLTreeSum, k: Int): AVLTreeSum = ???
  def updateSize(v:      AVLTreeSum): Unit = ???
  def mergeWithRoot(
    v1:   AVLTreeSum,
    v2:   AVLTreeSum,
    root: AVLTreeSum
  ): AVLTreeSum = ???
  def merge(v1:  AVLTreeSum, v2: AVLTreeSum): AVLTreeSum               = ???
  def split(v:   AVLTreeSum, k:  Int):        (AVLTreeSum, AVLTreeSum) = ???
  def reOrder(v: AVLTreeSum, i:  Int):        AVLTreeSum               = ???
}
