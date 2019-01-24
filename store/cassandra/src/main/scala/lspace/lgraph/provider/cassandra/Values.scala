package lspace.lgraph.provider.cassandra

import com.datastax.driver.core.PagingState
import com.outworkers.phantom.dsl._

import scala.concurrent.Future

abstract class Values extends Table[Values, Value] {
  object id    extends LongColumn with PartitionKey
  object iri   extends LongColumn
  object iris  extends SetColumn[Long]
  object label extends LongColumn
  object value extends StringColumn
  object props extends MapColumn[Long, List[Long]]

  def findById(id: Long) = {
    select.where(_.id eqs id).fetchRecord()
  }

  def findByIds(ids: List[Long], pagingState: Option[PagingState] = None) = {
    select.where(_.id in ids).paginateRecord(pagingState)
  }

  def delete(id: Long) = super.delete.where(_.id eqs id)
//  def delete(ids: List[Long]) = delete.where(_.id in ids)
}

abstract class ValuesByValue extends Table[ValuesByValue, Value] {
  object id    extends LongColumn with PrimaryKey
  object iri   extends LongColumn
  object iris  extends SetColumn[Long]
  object label extends LongColumn
  object value extends StringColumn with PartitionKey
  object props extends MapColumn[Long, List[Long]]

  def findByValue(value: String, pagingState: Option[PagingState] = None) = {
    select.where(_.value eqs value).paginateRecord(pagingState)
  }

  def findByValues(values: List[String], pagingState: Option[PagingState] = None) = {
    select.where(_.value in values).paginateRecord(pagingState)
  }

  def delete(id: Long, value: String) = super.delete.where(_.id eqs id).and(_.value eqs value)
//  def delete(values: List[String]) = delete.where(_.value in values)
}

abstract class ValuesByIri extends Table[ValuesByIri, Value] {
  object id    extends LongColumn with PrimaryKey
  object iri   extends LongColumn with PartitionKey
  object iris  extends SetColumn[Long]
  object label extends LongColumn
  object value extends StringColumn
  object props extends MapColumn[Long, List[Long]]

  def findByIri(iri: Long, pagingState: Option[PagingState] = None) = {
    select.where(_.iri eqs iri).paginateRecord(pagingState)
  }

  def findByIris(iris: List[Long], pagingState: Option[PagingState] = None) = {
    select.where(_.iri in iris).paginateRecord(pagingState)
  }

  def delete(id: Long, iri: Long) = super.delete.where(_.id eqs id).and(_.iri eqs iri)
//  def delete(iris: List[Long]) = delete.where(_.iri in iris)
}
