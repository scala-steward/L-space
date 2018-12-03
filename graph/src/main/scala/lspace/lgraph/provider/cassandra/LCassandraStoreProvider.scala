package lspace.lgraph.provider.cassandra

import com.outworkers.phantom.connectors.KeySpaceBuilder
import com.outworkers.phantom.dsl._
import lspace.lgraph.store.{StoreManager, StoreProvider}
import lspace.lgraph.{GraphManager, LGraph}

import scala.util.Try

abstract class CassandraGraph(override val connector: CassandraConnection) extends Database[CassandraGraph](connector) {
  object states extends States with Connector
}

abstract class CassandraGraphTables(override val connector: CassandraConnection)
    extends Database[CassandraGraphTables](connector) {
  object nodes                  extends Nodes with Connector
  object nodesByIri             extends NodesByIri with Connector
  object nodesByIris            extends NodesByIris with Connector
  object edges                  extends Edges with Connector
  object edgesByIri             extends EdgesByIri with Connector
  object edgesByFrom            extends EdgesByFrom with Connector
  object edgesByFromAndKey      extends EdgesByFromAndKey with Connector
  object edgesByTo              extends EdgesByTo with Connector
  object edgesByToAndKey        extends EdgesByToAndKey with Connector
  object edgesByFromAndTo       extends EdgesByFromAndTo with Connector
  object edgesByFromAndKeyAndTo extends EdgesByFromAndKeyAndTo with Connector
  object values                 extends Values with Connector
  object valuesByValue          extends ValuesByValue with Connector
  object valuesByIri            extends ValuesByIri with Connector
}

object LCassandraStoreProvider {
  def apply(iri: String, keyspaceBuilder: KeySpaceBuilder): LCassandraStoreProvider =
    new LCassandraStoreProvider(iri, keyspaceBuilder)
}
class LCassandraStoreProvider(val iri: String, keyspaceBuilder: KeySpaceBuilder) extends StoreProvider {

  val space: LKeySpace = LKeySpace(iri)

  private def getOrCreateKeySpace(iri: String) =
    KeySpace(iri.replace('.', '_').replace('-', '_'))
      .ifNotExists()
      .`with`(
        replication eqs SimpleStrategy.replication_factor(1)
      )

  object graph              extends CassandraGraph(keyspaceBuilder.keySpace(getOrCreateKeySpace(space.graph)))
  object graphTables        extends CassandraGraphTables(keyspaceBuilder.keySpace(getOrCreateKeySpace(space.data)))
  object nsGraphTables      extends CassandraGraphTables(keyspaceBuilder.keySpace(getOrCreateKeySpace(space.ns)))
  object nsIndexGraphTables extends CassandraGraphTables(keyspaceBuilder.keySpace(getOrCreateKeySpace(space.nsIndex)))
  object indexGraphTables   extends CassandraGraphTables(keyspaceBuilder.keySpace(getOrCreateKeySpace(space.index)))
  object indexIndexGraphTables
      extends CassandraGraphTables(keyspaceBuilder.keySpace(getOrCreateKeySpace(space.indexIndex)))

  def deleteAll(): Unit = {
    Try { graph.truncate() }
    Try { graphTables.truncate() }
    Try { nsGraphTables.truncate() }
    Try { nsIndexGraphTables.truncate() }
    Try { indexGraphTables.truncate() }
    Try { indexIndexGraphTables.truncate() }
  }

  def stateManager[G <: LGraph](graph: G): GraphManager[G]   = new CassandraGraphManager(graph, this.graph)
  def dataManager[G <: LGraph](graph: G): StoreManager[G]    = new CassandraStoreManager(graph, graphTables)
  def nsManager[G <: LGraph](graph: G): StoreManager[G]      = new CassandraStoreManager(graph, nsGraphTables)
  def nsIndexManager[G <: LGraph](graph: G): StoreManager[G] = new CassandraStoreManager(graph, nsIndexGraphTables)
  def indexManager[G <: LGraph](graph: G): StoreManager[G]   = new CassandraStoreManager(graph, indexGraphTables)
  def indexIndexManager[G <: LGraph](graph: G): StoreManager[G] =
    new CassandraStoreManager(graph, indexIndexGraphTables)
}
