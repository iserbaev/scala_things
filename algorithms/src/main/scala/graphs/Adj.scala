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
case class Meta[V](v: V, color: Color, d: Int, parent: Option[V]) {
  def setColor(c:    Color): Meta[V] = copy(color  = c)
  def setDistance(i: Int):   Meta[V] = copy(d      = i)
  def setParent(v:   V):     Meta[V] = copy(parent = Some(v))
}
object Meta {
  def apply[V](value: V): Meta[V] =
    new Meta[V](value, Color.White, -1, None)
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
      g.map(vrepr => vrepr.copy(color = White, d = 0, parent = None))
    val head = s.copy(color = Grey, d = 0, parent = None)

    val queue = mutable.Queue.empty[Meta[V]]
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
      "f" -> List(3, 4),
      "g" -> List(6, 7),
      "h" -> List()
    )
  )

  println(adj)

  println(Adj.bfs(adj, adj.repr.head._2))

  // TODO check for 3 trees
}
