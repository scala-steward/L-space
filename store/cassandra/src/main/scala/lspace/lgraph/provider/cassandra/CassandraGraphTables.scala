package lspace.lgraph.provider.cassandra

import com.outworkers.phantom.dsl.{CassandraConnection, Database}

abstract class CassandraGraphTables(override val connector: CassandraConnection)
    extends Database[CassandraGraphTables](connector) {
  object nodes extends Nodes with Connector
//  object nodesByIri             extends NodesByIri with Connector
//  object nodesByIris            extends NodesByIris with Connector
  object edges extends Edges with Connector
//  object edgesByIri             extends EdgesByIri with Connector
  object edgesByFrom            extends EdgesByFrom with Connector
  object edgesByFromAndKey      extends EdgesByFromAndKey with Connector
  object edgesByTo              extends EdgesByTo with Connector
  object edgesByToAndKey        extends EdgesByToAndKey with Connector
  object edgesByFromAndTo       extends EdgesByFromAndTo with Connector
  object edgesByFromAndKeyAndTo extends EdgesByFromAndKeyAndTo with Connector
  object values                 extends Values with Connector
  object valuesByValue          extends ValuesByValue with Connector
//  object valuesByIri            extends ValuesByIri with Connector
}
