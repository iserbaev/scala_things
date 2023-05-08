package tasks

case class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int         = _x
}
object SolutionP {
  def isPalindrome(head: ListNode): Boolean = head.next match {
    case empty if empty == null =>
      true
    case last if last.next == null =>
      head.x == last.x
    case intermediate if intermediate.next != null && intermediate.next.next == null =>
      head.x == intermediate.next.x
    case _ =>
      throw new IllegalStateException("should not happen")
  }

  def isP(node: ListNode, checkLast: Int => Boolean): Boolean =
    node.next match {
      case empty if empty == null =>
        checkLast(node.x)
      case last if last.next == null =>
        checkLast(last.x)
      case intermediate if intermediate.next != null && intermediate.next.next == null =>
        checkLast(intermediate.next.x) && (node.x == intermediate.x)
      case _ =>
        throw new IllegalStateException("should not happen")
    }
}

object TestP extends App {
  import SolutionP._

  assert(isPalindrome(ListNode(1)))
  assert(isPalindrome(ListNode(1, ListNode(1))))
  assert(isPalindrome(ListNode(1, ListNode(2, ListNode(1)))))

  assert(!isPalindrome(ListNode(1, ListNode(2))))
  assert(!isPalindrome(ListNode(1, ListNode(2, ListNode(2)))))
}
