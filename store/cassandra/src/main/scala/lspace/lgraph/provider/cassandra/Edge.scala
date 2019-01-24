package lspace.lgraph.provider.cassandra

case class Edge(id: Long,
                iri: Long,
                iris: Set[Long],
                fromId: Long,
                fromType: Int, //0 = node, 1 = edge, 2 = value
                key: Long,
                toId: Long,
                toType: Int, //0 = node, 1 = edge, 2 = value
                props: Map[Long, List[Long]] = Map())
