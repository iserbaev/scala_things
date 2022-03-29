package graphs

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
  * @param d расстояние от корня до текущей вершины
  * @param parent индекс предшественника
  */
case class Repr[V](v: V, color: Color, d: Int, parent: Option[V]) {
  def setColor(c:    Color): Repr[V] = copy(color  = c)
  def setDistance(i: Int):   Repr[V] = copy(d      = i)
  def setParent(v:   V):     Repr[V] = copy(parent = Some(v))
}
object Repr {
  def apply[V](value: V): Repr[V] =
    new Repr[V](value, Color.White, -1, None)
}

/** Adjacency list representation (список смежности)
  * Для каждой вершины u Adj[u] состоит из всех вершин смежных с u в графе G
  */
case class Adj[V](g: Map[V, List[V]], repr: Map[V, Repr[V]]) {
  def map(f: Repr[V] => Repr[V]): Adj[V] =
    Adj(g, repr.map { case (k, r) => k -> f(r) })

  def update(v: V)(f: Repr[V] => Repr[V]): Adj[V] =
    Adj(g, repr = repr.updated(v, f(repr(v))))

  def adjacent(u: V, v: V): Boolean =
    g(u).contains(v)
}
object Adj {
  import Color._

  def apply[V](edgesMappings: Map[String, List[V]]): Adj[V] = {

    val (gMap, reprSet) = edgesMappings.values.foldLeft(
      (mutable.Map.empty[V, List[V]], mutable.Map.empty[V, Repr[V]])
    ) {
      case ((gi, repri), pair) =>
        val (v1, v2) = pair.head -> pair(1)
        gi.update(v1, v2 :: gi.getOrElse(v1, List.empty))
        gi.update(v2, v1 :: gi.getOrElse(v2, List.empty))
        repri.update(v1, Repr(v1))
        repri.update(v2, Repr(v2))

        gi -> repri
    }

    new Adj(gMap.toMap, reprSet.toMap)
  }

  def bfs[V](g: Adj[V], s: Repr[V]): Adj[V] = {
    val clean =
      g.map(vrepr => vrepr.copy(color = White, d = 0, parent = None))
    val head = s.copy(color = Grey, d = 0, parent = None)

    val queue = mutable.Queue.empty[Repr[V]]
    queue.enqueue(head)

    var adj = clean
    while (queue.nonEmpty) {
      val u = queue.dequeue()

      adj = adj
        .map { v =>
          if (v.color == White && adj.adjacent(u.v, v.v)) {
            val visited =
              v.setColor(Grey).setDistance(u.d + 1).setParent(u.v)
            queue.enqueue(visited)
            visited
          } else v
        }
        .update(u.v)(_.setColor(Black))

    }

    adj
  }
}

object TestAdj extends App {
  val adj = Adj(
    Map(
      "a" -> List(1, 5),
      "b" -> List(1, 2),
      "c" -> List(2, 4),
      "d" -> List(4, 5),
      "e" -> List(2, 3),
      "f" -> List(3, 4)
    )
  )

  println(adj)

  println(Adj.bfs(adj, adj.repr.head._2))
}
