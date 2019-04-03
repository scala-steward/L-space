package lspace.provider.transaction

import java.util.concurrent.ConcurrentHashMap

import lspace.datatype.DataType
import lspace.structure.util.ClassTypeable
import lspace.structure.{ClassType, Value, Values}
import monix.eval.Task
import monix.reactive.Observable

import scala.collection.JavaConverters._
import scala.collection.mutable

abstract class TValues[G <: Transaction](override val graph: G) extends Values(graph) {
  import graph._

  val added: mutable.HashSet[GValue[_]]                    = mutable.HashSet[GValue[_]]()
  val deleted: mutable.OpenHashMap[Long, parent.GValue[_]] = mutable.OpenHashMap[Long, parent.GValue[_]]()

  override def apply(): Observable[Value[_]] = {
    val tvalues = super.apply()
    val idSet: scala.collection.concurrent.Map[Long, Value[_]] =
      new ConcurrentHashMap[Long, Value[_]]().asScala
    tvalues.map { value =>
      idSet += value.id -> value; value
    } ++ parent.values().filter(n => !idSet.contains(n.id))
  }

  override def hasIri(iris: List[String]): Observable[Value[_]] = {
    val fromTransaction = super.hasIri(iris)
    val fromParent = parent.values
      .hasIri(iris)
      .asInstanceOf[Observable[parent.GValue[Any]]]
      .mapEval(_TValue(_).task)
      .filter(n => !deleted.contains(n.id))
    val idSet: scala.collection.concurrent.Map[Long, Value[_]] =
      new ConcurrentHashMap[Long, Value[_]]().asScala
    fromTransaction.map { value =>
      idSet += value.id -> value; value
    } ++ fromParent.filter(n => !idSet.contains(n.id))
  }

  override def byValue[T, TOut, CTOut <: ClassType[_]](value: T)(
      implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut]): Observable[Value[T]] =
    byValue(List(value -> clsTpbl.ct.asInstanceOf[DataType[T]]))
  override def byValue[T](valueSet: List[(T, DataType[T])]): Observable[Value[T]] = {
    val fromTransaction = super.byValue(valueSet)
    val fromParent = parent.values
      .byValue(valueSet)
      .asInstanceOf[Observable[parent.GValue[T]]]
      .mapEval(_TValue(_).task)
      .filter(n => !deleted.contains(n.id))
    val idSet: scala.collection.concurrent.Map[Long, Value[_]] =
      new ConcurrentHashMap[Long, Value[_]]().asScala
    fromTransaction.map { value =>
      idSet += value.id -> value; value
    } ++ fromParent.filter(n => !idSet.contains(n.id))
  }

  override def hasId(id: Long): Task[Option[Value[_]]] = {
    if (deleted.contains(id)) Task.now(None)
    else
      for {
        r <- super
          .hasId(id)
        r1 <- if (r.nonEmpty) Task.now(r)
        else
          for {
            v <- parent.values
              .hasId(id)
            v1 <- if (v.nonEmpty) {
              _TValue(v.get.asInstanceOf[parent.GValue[Any]]).map(Some(_)).task
            } else Task.now(v)
          } yield v1
      } yield r1
  }
}
