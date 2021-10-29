package lspace.lgraph.provider.cassandra

case class Value(
  id: Long,
  iri: Option[String],
  iriEdge: Option[(Long, Long)],
  iris: Set[String],
  irisEdges: List[(Long, Long)],
  label: String,
  context0: String,
  value: String
)
