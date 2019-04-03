package lspace.lgraph.provider.cassandra

import com.datastax.driver.core.PagingState
import com.outworkers.phantom.dsl._

import scala.concurrent.Future

abstract class Values extends Table[Values, Value] {
  object id        extends LongColumn with PartitionKey
  object iri       extends OptionalStringColumn
  object iriEdge   extends OptionalCol[(Long, Long)]
  object iris      extends SetColumn[String]
  object irisEdges extends ListColumn[(Long, Long)]
  object label     extends StringColumn
  object context0  extends StringColumn
  object value     extends StringColumn

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
  object id        extends LongColumn with PrimaryKey
  object iri       extends OptionalStringColumn
  object iriEdge   extends OptionalCol[(Long, Long)]
  object iris      extends SetColumn[String]
  object irisEdges extends ListColumn[(Long, Long)]
  object label     extends StringColumn
  object context0  extends StringColumn
  object value     extends StringColumn with PartitionKey

  def findByValue(value: String, pagingState: Option[PagingState] = None) = {
    select.where(_.value eqs value).paginateRecord(pagingState)
  }

  def findByValues(values: List[String], pagingState: Option[PagingState] = None) = {
    select.where(_.value in values).paginateRecord(pagingState)
  }

  def delete(id: Long, value: String) = super.delete.where(_.id eqs id).and(_.value eqs value)
//  def delete(values: List[String]) = delete.where(_.value in values)
}
//
//abstract class ValuesByIri extends Table[ValuesByIri, Value] {
//  object id        extends LongColumn with PrimaryKey
//  object iri       extends OptionalStringColumn with PartitionKey
//  object iriEdge   extends OptionalCol[(Long, Long)]
//  object iris      extends SetColumn[String]
//  object irisEdges extends ListColumn[(Long, Long)]
//  object label     extends StringColumn
//  object context0  extends StringColumn
//  object value     extends StringColumn
//
//  def findByIri(iri: String, pagingState: Option[PagingState] = None) = {
//    select.where(_.iri eqs Some(iri)).paginateRecord(pagingState)
//  }
//
//  def findByIris(iris: List[String], pagingState: Option[PagingState] = None) = {
//    select.where(_.iri in iris.map(Some(_))).paginateRecord(pagingState)
//  }
//
//  def delete(id: Long) = super.delete.where(_.id eqs id)
////  def delete(iris: List[Long]) = delete.where(_.iri in iris)
//}
