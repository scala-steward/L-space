package lspace.structure

import java.util.concurrent.ConcurrentHashMap

import lspace.datatype.DataType
import lspace.structure.util.{ClassTypeable, UpsertHelper}
import monix.eval.Task
import monix.reactive.Observable

import scala.collection.JavaConverters._
import scala.collection.concurrent
import scala.collection.immutable.ListSet

abstract class Values(val graph: Graph) extends RApi[Value[_]] {
  import graph._

  def apply(): Observable[Value[Any]] = valueStore.all()
  def count(): Task[Long]             = valueStore.count()

  def hasId(id: Long): Task[Option[Value[Any]]] = valueStore.hasId(id)
  def cached = new {
    def hasId(id: Long): Option[Value[Any]] =
      valueStore.cached.hasId(id)
    def dereferenceValue(t: Any): Any = t
    def count: Long                   = valueStore.cached.count
  }
  def hasIri(iris: List[String]): Observable[Value[Any]] = {
    //    println(s"get nodes $iris")
    val validIris = iris.distinct.filter(_.nonEmpty)
    if (validIris.nonEmpty)
      Observable.fromIterable(validIris).flatMap { iri =>
        //        println(s"get $iri")
        valueStore
          .hasIri(iri)
          .asInstanceOf[Observable[Value[_]]] //        println(r.map(_.iri))
      } else Observable[Value[_]]()
  }

  def byValue[T, TOut, CTOut <: ClassType[_]](value: T)(
      implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut]): Observable[Value[T]] =
    valueStore.byValue(value, clsTpbl.ct.asInstanceOf[DataType[T]]).asInstanceOf[Observable[Value[T]]]
  def byValue[T](valueSet: List[(T, DataType[T])]): Observable[Value[T]] =
    //      val valueList = valueSet.distinct.filter(_ != null)
    //      if (valueList.nonEmpty) values().filter(v => valueSet.map(_._2).contains(v.value)).toList
    //      //      or(
    //      //      _.has(this.id, p.Within(iriList)),
    //      //      _.has(this.ids, p.Within(iriList))).toSet
    //      else List[Value[_]]()
    Observable.fromIterable(valueSet).flatMap { value =>
      valueStore.byValue(value._1, value._2).asInstanceOf[Observable[Value[T]]]
    } //distinct

  def dereferenceValue(t: Any): Task[Any] = t match {
    case v: Vector[_] =>
      Task.parSequence[Any, Vector](v.map(dereferenceValue)) //without type parameters it cannot be inferred
    case v: ListSet[_] => Task.parSequence[Any, ListSet](v.map(dereferenceValue))
    case v: List[_]    => Task.parSequence[Any, List](v.map(dereferenceValue))
    case v: Set[_]     => Task.parSequence[Any, Set](v.map(dereferenceValue))
    case v: Map[_, _] =>
      Task
        .parSequence(v.toList.map {
          case (key, value) =>
            for {
              a <- dereferenceValue(key)
              b <- dereferenceValue(value)
            } yield (a, b)
        })
        .map(_.toMap)
    case (v1, v2) =>
      for {
        a <- dereferenceValue(v1)
        b <- dereferenceValue(v2)
      } yield (a, b)
    case (v1, v2, v3) =>
      for {
        a <- dereferenceValue(v1)
        b <- dereferenceValue(v2)
        c <- dereferenceValue(v3)
      } yield (a, b, c)
    case (v1, v2, v3, v4) =>
      for {
        a <- dereferenceValue(v1)
        b <- dereferenceValue(v2)
        c <- dereferenceValue(v3)
        d <- dereferenceValue(v4)
      } yield (a, b, c, d)
    case (v1, v2, v3, v4, v5) =>
      for {
        a <- dereferenceValue(v1)
        b <- dereferenceValue(v2)
        c <- dereferenceValue(v3)
        d <- dereferenceValue(v4)
        e <- dereferenceValue(v5)
      } yield (a, b, c, d, e)
    case v: Ontology    => nodes.upsert(v.iri, Ontology.ontology) //ns.ontologies.store(v))
    case v: Property    => nodes.upsert(v.iri, Property.ontology) //(ns.properties.store(v))
    case v: DataType[_] => nodes.upsert(v.iri, DataType.ontology) //ns.datatypes.store(v))
    case v: Node        => nodes.upsert(v)
    case v: Edge[_, _]  => edges.upsert(v)
    case v: Value[_]    => values.upsert(v)
    case _              => Task.now(t) //TODO: check if t is a supported datatype?
  }

  final def create[T, TOut, CTOut <: ClassType[_]](value: T)(
      implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut]): Task[Value[T]] = //add implicit DataType[T]
    byValue(value).headOptionL.flatMap(_.map(_.asInstanceOf[_Value[T]]).map(Task.now).getOrElse {
      for {
        dereferencedValue <- dereferenceValue(value).map(_.asInstanceOf[T])
        b <- byValue(dereferencedValue)(clsTpbl).headOptionL
          .flatMap(_.map(_.asInstanceOf[_Value[T]]).map(Task.now).getOrElse {
            for {
              id    <- idProvider.next
              value <- createValue(id, dereferencedValue, DataType.detect(dereferencedValue))
            } yield value
          })
      } yield b
    })
  final protected[lspace] def create[T](value: T, dt: DataType[T]): Task[Value[T]] = { //add implicit DataType[T]
    val detectedDT = DataType.detect(value)
    val finalDT    = if (detectedDT.`@extends`(dt)) detectedDT else dt
    for {
      dereferencedValue <- dereferenceValue(value).map(_.asInstanceOf[T])
      b <- byValue(dereferencedValue -> finalDT :: Nil).headOptionL
        .flatMap(_.map(_.asInstanceOf[_Value[T]]).map(Task.now).getOrElse {
          for {
            id    <- idProvider.next
            value <- createValue(id, dereferencedValue, finalDT)
          } yield value
        })
    } yield { b }
  }

  private val upsertingTasks: concurrent.Map[Any, Task[Value[Any]]] =
    new ConcurrentHashMap[Any, Task[Value[Any]]]().asScala

  def upsert[V, TOut, CTOut <: ClassType[Any]](value: V)(
      implicit clsTpbl: ClassTypeable.Aux[V, TOut, CTOut]): Task[Value[V]] =
    upsertingTasks
      .getOrElseUpdate(
        value,
        byValue(value).toListL
          .flatMap {
            case Nil         => create(value, DataType.detect(value))
            case List(value) => Task.now(value)
            case values =>
              mergeValues(values.toSet)
          }
          .doOnFinish(_ => Task(upsertingTasks.remove(value)))
          .memoize
      )
      .asInstanceOf[Task[Value[V]]]

  final protected[lspace] def upsert[V](value: V, dt: DataType[V]): Task[Value[V]] =
    //      val values = byValue(List(value -> dt))
    //      values.headOptionL.flatMap(_.map(Task.now).getOrElse(create(value, dt)))
//    val dt = DataType.detect(value)
    upsertingTasks
      .getOrElseUpdate(
        value,
        byValue(List(value -> dt)).toListL
          .flatMap {
            case Nil         => create(value, dt)
            case List(value) => Task.now(value)
            case values =>
              mergeValues(values.toSet)
          }
          .doOnFinish(_ => Task(upsertingTasks.remove(value)))
          .memoize
      )
      .asInstanceOf[Task[Value[V]]]
  //      val _value: Value[V] = if (values.isEmpty) {
  //        create(value, dt)
  ////      } else if (values.size > 1) {
  ////        GraphUtils.mergeValues(values.toSet)
  //      } else values.head
  //      _value
  final def upsert[V](value: Value[V])(implicit helper: UpsertHelper = UpsertHelper()): Task[Value[V]] =
    if (value.graph != this) {
      helper.createValue(value.id,
      upsert(value.value, value.label))
    } else Task.now(value)

  /**
    * adds a value to the graph including all edges and (meta) edges on edges as long as edges have edges
    * @param value
    * @tparam V
    * @return
    */
  def post[V](value: Value[V]): Task[Value[V]] =
    if (value.graph != this) {
      hasIri(value.iri).toListL
        .flatMap {
          case List() => create(value.value, value.label)
          case List(storedValue: Value[_]) if storedValue.value == value.value =>
            Task.now(storedValue.asInstanceOf[Value[V]])
          case List(_: Value[_], _*) =>
            Task.raiseError(new Exception("multiple values with the same iri, what should we do?! Dedup?"))
        }
        .flatMap { newValue =>
          for { _ <- addMeta(value, newValue) } yield newValue
        }
    } else Task.now(value)

  final def delete(value: Value[Any]): Task[Unit] = value match {
    case value: _Value[_] => deleteValue(value.asInstanceOf[_Value[_]])
    case _                => Task.unit //LOG???
  }

  /**
    * adds a value
    * @param value
    * @tparam V
    * @return
    */
  final def +[V](value: Value[V]): Task[Value[V]] = upsert(value)

  /**
    * deletes a value
    * @param value
    */
  final def -(value: Value[Any]): Task[Unit] = delete(value)

  /**
    * adds a value and meta-properties (value.outE())
    * @param value
    * @tparam V
    * @return
    */
  final def ++[V](value: Value[V]): Task[Value[V]] = post(value)
}
