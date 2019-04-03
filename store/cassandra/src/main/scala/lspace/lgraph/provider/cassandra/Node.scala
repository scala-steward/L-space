package lspace.lgraph.provider.cassandra

case class Node(id: Long,
                iri: Option[String],
                iriEdge: Option[(Long, Long)],
                iris: Set[String],
                irisEdges: List[(Long, Long)],
                labels: List[String])
