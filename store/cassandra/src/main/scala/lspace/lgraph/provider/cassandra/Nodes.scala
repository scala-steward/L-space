package lspace.lgraph.provider.cassandra

import com.datastax.driver.core.PagingState
import com.outworkers.phantom.dsl._

import scala.concurrent.Future

abstract class Nodes extends Table[Nodes, Node] {
  object id        extends LongColumn with PartitionKey
  object iri       extends OptionalStringColumn
  object iriEdge   extends OptionalCol[(Long, Long)]
  object iris      extends SetColumn[String]
  object irisEdges extends ListColumn[(Long, Long)]
  object labels    extends ListColumn[String]

  def findById(id: Long) =
    select.where(_.id.eqs(id)).fetchRecord()

  def findByIds(ids: List[Long], pagingState: Option[PagingState] = None) =
    select.where(_.id in ids).paginateRecord(pagingState)

  def delete(id: Long) = super.delete.where(_.id.eqs(id))
//  def delete(ids: List[Long]) = delete.where(_.id in ids)
}

//abstract class NodesByIri extends Table[NodesByIri, Node] {
//  object id        extends LongColumn with PrimaryKey
//  object iri       extends OptionalStringColumn with PartitionKey
//  object iriEdge   extends OptionalCol[(Long, Long)]
//  object iris      extends SetColumn[String]
//  object irisEdges extends ListColumn[(Long, Long)]
//  object labels    extends ListColumn[String]
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
//
//abstract class NodesByIris extends Table[NodesByIris, Node] {
//  object id        extends LongColumn with PrimaryKey
//  object iri       extends OptionalStringColumn
//  object iriEdge   extends OptionalCol[(Long, Long)]
//  object iris      extends SetColumn[String] with PartitionKey
//  object irisEdges extends ListColumn[(Long, Long)]
//  object labels    extends ListColumn[String]
//
//  def findByIri(iri: String, pagingState: Option[PagingState] = None) = {
//    select.where(_.iris contains iri).paginateRecord(pagingState)
//  }
//
//  def findByIris(iris: Set[String], pagingState: Option[PagingState] = None) =
//    select.where(_.iris eqs iris).paginateRecord(pagingState)
////  {
////    iris.tail
////      .foldLeft(select.where(_.iris contains iris.head)) { case (query, iri) => query.and(_.iris contains iri) }
////      .paginateRecord(pagingState)
////  }
//
//  def delete(id: Long) = super.delete.where(_.id eqs id)
//
////    iris.foldLeft(super.delete.where(_.id eqs id)) {
////    case (q, iri) => q.and(_.iris eqs Set(iri))
////  }
////  def delete(iris: List[Long]) = iris.map(delete(_))
//}
