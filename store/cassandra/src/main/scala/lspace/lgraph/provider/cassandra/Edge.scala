package lspace.lgraph.provider.cassandra

case class Edge(
  id: Long,
  iri: Option[String],
  iriEdge: Option[(Long, Long)],
  iris: Set[String],
  irisEdges: List[(Long, Long)],
  fromId: Long,
  fromType: Int, // 0 = node, 1 = edge, 2 = value
  key: String,
  skey: Set[String],
  toId: Long,
  toType: Int // 0 = node, 1 = edge, 2 = value
)
