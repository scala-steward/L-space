package lspace.lgraph.provider.cassandra

import com.outworkers.phantom.dsl.{CassandraConnection, Database}

abstract class CassandraGraph(override val connector: CassandraConnection) extends Database[CassandraGraph](connector) {
  object states extends States with Connector
}
