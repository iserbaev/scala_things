package leetcode.top150

object StringSubsequence extends App {
  def isSubsequence(s: String, t: String): Boolean = s.isEmpty ||
    (s.length == 1 && t.contains(s)) ||
    ((s.nonEmpty && t.nonEmpty) && {
      val (isSubsequence, sIdx, _, _) = t.foldLeft((false, 0, Option.empty[Int], 0)) {
        case ((isSubsequence, sIdx, tIdxPrev, tIdx), tv) =>
          if (sIdx < s.length && s(sIdx) == tv) {

            (tIdxPrev.exists(_ < tIdx), sIdx + 1, Some(tIdx), tIdx + 1)
          } else {
            (isSubsequence, sIdx, tIdxPrev, tIdx + 1)
          }
      }

      val res = isSubsequence && sIdx == s.length
      println(res)
      res
    })

  isSubsequence("acb", "ahbgdc"): Unit
  isSubsequence("abc", "ahbgdc"): Unit
}
