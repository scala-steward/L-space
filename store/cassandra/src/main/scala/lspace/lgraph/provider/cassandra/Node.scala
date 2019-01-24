package lspace.lgraph.provider.cassandra

case class Node(id: Long, iri: Long, iris: Set[Long], labels: List[Long], props: Map[Long, List[Long]] = Map())
