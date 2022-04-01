package graphs

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

sealed trait Color
object Color {
  case object White extends Color
  case object Grey extends Color
  case object Black extends Color
}

/** Представление вершины графа
  * @param v значение
  * @param color цвет
  * @param distance расстояние от корня до текущей вершины
  * @param parent значение родителя
  * @param open метка открытия вершины
  * @param close метка завершения работы с вершиной
  */
case class Meta[V](
  v:        V,
  color:    Color,
  distance: Int,
  parent:   Option[V],
  open:     Option[Int],
  close:    Option[Int]
) {
  def setColor(c:    Color): Meta[V] = copy(color    = c)
  def setDistance(i: Int):   Meta[V] = copy(distance = i)
  def setParent(v:   V):     Meta[V] = copy(parent   = Some(v))
  def setOpen(t:     Int):   Meta[V] = copy(open     = Some(t))
  def setClose(t:    Int):   Meta[V] = copy(close    = Some(t))
}
object Meta {
  def apply[V](value: V): Meta[V] =
    new Meta[V](value, Color.White, -1, None, None, None)
}

/** Adjacency list representation (список смежности)
  * Для каждой вершины u Adj[u] состоит из всех вершин смежных с u в графе G
  */
case class Adj[V](g: Map[V, List[V]], repr: Map[V, Meta[V]]) {
  def map(f: Meta[V] => Meta[V]): Adj[V] =
    Adj(g, repr.map { case (k, r) => k -> f(r) })

  def update(v: V)(f: Meta[V] => Meta[V]): Adj[V] =
    Adj(g, repr = repr.updated(v, f(repr(v))))

  def adjacent(u: V, v: V): Boolean =
    g(u).contains(v)

  def values: List[V] = repr.keys.toList
}
object Adj {
  import Color._

  def apply[V](edgesMappings: Map[String, List[V]]): Adj[V] = {

    val (gMap, reprMap) = edgesMappings.values.foldLeft(
      (mutable.Map.empty[V, List[V]], mutable.Map.empty[V, Meta[V]])
    ) {
      case ((gi, repri), pair) =>
        if (pair.nonEmpty) {
          require(pair.length == 2, s"pair.length != 2, fact =  ${pair.length}")
          val (v1, v2) = pair.head -> pair(1)
          gi.update(v1, v2 :: gi.getOrElse(v1, List.empty))
          gi.update(v2, v1 :: gi.getOrElse(v2, List.empty))
          repri.update(v1, Meta(v1))
          repri.update(v2, Meta(v2))
        }

        gi -> repri
    }

    new Adj(gMap.toMap, reprMap.toMap)
  }

  def bfs[V](g: Adj[V], s: Meta[V]): Adj[V] = {
    val clean =
      g.map(vrepr => vrepr.copy(color = White, distance = 0, parent = None))
    val head = s.copy(color = Grey, distance = 0, parent = None)

    val queue = mutable.Queue.empty[Meta[V]]
    queue.enqueue(head)

    var adj = clean
    while (queue.nonEmpty) {
      val u = queue.dequeue()

      adj = adj
        .map { v =>
          if (v.color == White && adj.adjacent(u.v, v.v)) {
            val visited =
              v.setColor(Grey).setDistance(u.distance + 1).setParent(u.v)
            queue.enqueue(visited)
            visited
          } else v
        }
        .update(u.v)(_.setColor(Black))

    }

    adj
  }

  def dfs[V](adj: Adj[V]): Adj[V] = {
    val clean =
      adj.map(vrepr => vrepr.copy(color = White, parent = None))

    val time = new AtomicInteger(0)

    val result = clean.repr.values.foldLeft(clean) {
      case (a, u) =>
        if (u.color == White) {
          dfsVisit(a, u, time)
        } else a
    }

    result
  }

  private def dfsVisit[V](
    adj:  Adj[V],
    u:    Meta[V],
    time: AtomicInteger
  ): Adj[V] = {
    val head    = u.setOpen(time.incrementAndGet()).setColor(Grey)
    val updated = adj.update(head.v)(_ => head)

    val recur = updated.repr.values.foldLeft(updated) {
      case (a, v) =>
        if (v.color == White && a.adjacent(head.v, v.v)) {
          dfsVisit(a, v, time)
        } else a
    }

    recur.update(head.v)(
      _ => head.setColor(Black).setClose(time.incrementAndGet())
    )
  }
}

object TestAdj extends App {
  val adj = Adj(
    Map(
      "a" -> List(1, 5),
      "b" -> List(1, 2),
      "c" -> List(2, 4),
      "d" -> List(4, 5),
      "e" -> List(2, 3)
//      "f" -> List(3, 4),
//      "g" -> List(6, 7),
//      "h" -> List()
    )
  )

  println(adj)

  println(Adj.bfs(adj, adj.repr.head._2))

  println(Adj.dfs(adj))

  // TODO check for 3 trees
}
