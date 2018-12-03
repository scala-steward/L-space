package lspace.lgraph.provider.cassandra

case class Value(id: Long, iri: Long, iris: Set[Long], label: Long, value: String, props: Map[Long, List[Long]] = Map())
