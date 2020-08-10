package tasks

import java.util.UUID

import scala.annotation.tailrec
import scala.collection.SortedMap

object Kri extends App {
  val a = Option(1)
  val b = List(1, 2, 3)

//  val c = for {
//    y <- a
//    x <- b
//  } yield (x + y) // fail

//  val c = for {
//    y <- b
//    x <- a
//  } yield (x + y) // ok
//  b.flatMap(x => a.map(y => x + y))

  case class MyList[A](head: A, tail: Option[MyList[A]]) {

    // O(N)
    def reverse: MyList[A] = {
      @tailrec
      def recur(l: MyList[A], acc: Option[MyList[A]]): MyList[A] =
        l.tail match {
          case None         => MyList(l.head, acc)
          case Some(myList) => recur(myList, Option(MyList(l.head, acc)))
        }

      recur(this, None)
    }
  }

  trait SessionManager {
    // O(1), 10 min ttl, K rps = 600 * K live sessions
    def createSession(): UUID

    // O(1), updates ttl, K rps = 600 * K
    def validateSession(session: UUID): Boolean

    // O(K)
    def cleanUp(): Unit
  }
//  case class SessionValue(start: Long, updates: Option[Long] = None)
//  class MYSessionManager extends SessionManager {
//    val ttl: SortedMap[Long, Set[UUID]]
//    val sh: Array[(Long, Map[UUID, SessionValue])] =
//      new Array[(Long, Map[UUID, SessionValue])](10)
//    val sessions: Map[UUID, SessionValue]
//
//  }

}
