package lspace.lgraph.provider.cassandra

import com.datastax.driver.core.PagingState

import scala.concurrent.Future
import com.outworkers.phantom.dsl._

abstract class Edges extends Table[Edges, Edge] {
  object id       extends LongColumn with PartitionKey
  object iri      extends LongColumn
  object iris     extends SetColumn[Long]
  object fromId   extends LongColumn
  object fromType extends IntColumn
  object key      extends LongColumn
  object toId     extends LongColumn
  object toType   extends IntColumn
  object props    extends MapColumn[Long, List[Long]]

  def findById(id: Long) = {
    select.where(_.id eqs id).fetchRecord()
  }

  def findByIds(ids: List[Long], pagingState: Option[PagingState] = None) = {
    select.where(_.id in ids).paginateRecord(pagingState)
  }

  def delete(id: Long) = super.delete.where(_.id eqs id)
//  def delete(ids: List[Long]) = delete.where(_.id in ids)
}

abstract class EdgesByIri extends Table[EdgesByIri, Edge] {
  object id       extends LongColumn with PrimaryKey
  object iri      extends LongColumn with PartitionKey
  object iris     extends SetColumn[Long]
  object fromId   extends LongColumn
  object fromType extends IntColumn
  object key      extends LongColumn
  object toId     extends LongColumn
  object toType   extends IntColumn
  object props    extends MapColumn[Long, List[Long]]

  def findByIri(iri: Long, pagingState: Option[PagingState] = None) = {
    select.where(_.iri eqs iri).paginateRecord(pagingState)
  }

  def findByIris(iris: List[Long], pagingState: Option[PagingState] = None) = {
    select.where(_.iri in iris).paginateRecord(pagingState)
  }

  def delete(id: Long, iri: Long) = super.delete.where(_.id eqs id).and(_.iri eqs iri)
//  def delete(iris: List[Long]) = delete.where(_.iri in iris)
}

abstract class EdgesByFrom extends Table[EdgesByFrom, Edge] {
  object id       extends LongColumn with PrimaryKey
  object iri      extends LongColumn
  object iris     extends SetColumn[Long]
  object fromId   extends LongColumn with PartitionKey
  object fromType extends IntColumn
  object key      extends LongColumn
  object toId     extends LongColumn
  object toType   extends IntColumn
  object props    extends MapColumn[Long, List[Long]]

  def findByFrom(id: Long, pagingState: Option[PagingState] = None) = {
    select.where(_.fromId eqs id).paginateRecord(pagingState)
  }
  def findByFroms(ids: List[Long], pagingState: Option[PagingState] = None) = {
    select.where(_.fromId in ids).paginateRecord(pagingState)
  }

  def delete(id: Long, fromId: Long) = super.delete.where(_.id eqs id).and(_.fromId eqs fromId)
//  def delete(ids: List[Long]) = delete.where(_.fromId in ids)
}

abstract class EdgesByFromAndKey extends Table[EdgesByFromAndKey, Edge] {
  object id       extends LongColumn with PrimaryKey
  object iri      extends LongColumn
  object iris     extends SetColumn[Long]
  object fromId   extends LongColumn with PartitionKey
  object fromType extends IntColumn
  object key      extends LongColumn with PartitionKey
  object toId     extends LongColumn
  object toType   extends IntColumn
  object props    extends MapColumn[Long, List[Long]]

  def findByFromAndKey(from: Long, key: Long, pagingState: Option[PagingState] = None) = {
    select.where(_.fromId eqs from).and(_.key eqs key).paginateRecord(pagingState)
  }

  def delete(id: Long, fromId: Long, key: Long) =
    super.delete.where(_.id eqs id).and(_.fromId eqs fromId).and(_.key eqs key)
}

abstract class EdgesByTo extends Table[EdgesByTo, Edge] {
  object id       extends LongColumn with PrimaryKey
  object iri      extends LongColumn
  object iris     extends SetColumn[Long]
  object fromId   extends LongColumn
  object fromType extends IntColumn
  object key      extends LongColumn
  object toId     extends LongColumn with PartitionKey
  object toType   extends IntColumn
  object props    extends MapColumn[Long, List[Long]]

  def findByTo(to: Long, pagingState: Option[PagingState] = None) = {
    select.where(_.toId eqs to).paginateRecord(pagingState)
  }

  def delete(id: Long, toId: Long) = super.delete.where(_.id eqs id).and(_.toId eqs toId)
//  def delete(ids: List[Long]) = delete.where(_.toId in ids)
}

abstract class EdgesByToAndKey extends Table[EdgesByToAndKey, Edge] {
  object id       extends LongColumn with PrimaryKey
  object iri      extends LongColumn
  object iris     extends SetColumn[Long]
  object fromId   extends LongColumn
  object fromType extends IntColumn
  object key      extends LongColumn with PartitionKey
  object toId     extends LongColumn with PartitionKey
  object toType   extends IntColumn
  object props    extends MapColumn[Long, List[Long]]

  def findByToAndKey(to: Long, key: Long, pagingState: Option[PagingState] = None) = {
    select.where(_.toId eqs to).and(_.key eqs key).paginateRecord(pagingState)
  }

  def delete(id: Long, toId: Long, key: Long) = super.delete.where(_.id eqs id).and(_.toId eqs toId).and(_.key eqs key)
}

abstract class EdgesByFromAndTo extends Table[EdgesByFromAndTo, Edge] {
  object id       extends LongColumn with PrimaryKey
  object iri      extends LongColumn
  object iris     extends SetColumn[Long]
  object fromId   extends LongColumn with PartitionKey
  object fromType extends IntColumn
  object key      extends LongColumn
  object toId     extends LongColumn with PartitionKey
  object toType   extends IntColumn
  object props    extends MapColumn[Long, List[Long]]

  def findByFromAndKeyAndTo(from: Long, to: Long, pagingState: Option[PagingState] = None) = {
    select.where(_.fromId eqs from).and(_.toId eqs to).paginateRecord(pagingState)
  }

  def delete(id: Long, fromId: Long, toId: Long) =
    super.delete.where(_.id eqs id).and(_.fromId eqs fromId).and(_.toId eqs toId)
}

abstract class EdgesByFromAndKeyAndTo extends Table[EdgesByFromAndKeyAndTo, Edge] {
  object id       extends LongColumn with PrimaryKey
  object iri      extends LongColumn
  object iris     extends SetColumn[Long]
  object fromId   extends LongColumn with PartitionKey
  object fromType extends IntColumn
  object key      extends LongColumn with PartitionKey
  object toId     extends LongColumn with PartitionKey
  object toType   extends IntColumn
  object props    extends MapColumn[Long, List[Long]]

  def findByFromAndKeyAndTo(from: Long, key: Long, to: Long, pagingState: Option[PagingState] = None) = {
    select.where(_.fromId eqs from).and(_.key eqs key).and(_.toId eqs to).paginateRecord(pagingState)
  }

  def delete(id: Long, fromId: Long, key: Long, toId: Long) =
    super.delete.where(_.id eqs id).and(_.fromId eqs fromId).and(_.key eqs key).and(_.toId eqs toId)
}
