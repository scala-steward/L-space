package lspace.lgraph.store

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

import lspace.lgraph.LGraph
import lspace.librarian.datatype._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.Property.default.{`@id`, `@ids`}
import lspace.librarian.structure.{ClassType, DataType, Value}
import lspace.librarian.structure.store.ValueStore
import lspace.types.vector.Point

import scala.collection.mutable
import scala.concurrent.duration._

object LValueStore {
  def apply[G <: LGraph](iri: String, graph: G): LValueStore[G] = new LValueStore(iri, graph)
}

class LValueStore[G <: LGraph](val iri: String, val graph: G) extends LStore[G] with ValueStore[G] {

  protected lazy val intCache: mutable.OpenHashMap[Int, Set[T]] =
    mutable.OpenHashMap[Int, Set[T]]()
  protected lazy val doubleCache: mutable.OpenHashMap[Double, Set[T]] =
    mutable.OpenHashMap[Double, Set[T]]()
  protected lazy val longCache: mutable.OpenHashMap[Long, Set[T]] =
    mutable.OpenHashMap[Long, Set[T]]()
  protected lazy val stringCache: mutable.OpenHashMap[String, Set[T]] =
    mutable.OpenHashMap[String, Set[T]]()
  protected lazy val booleanCache: mutable.OpenHashMap[Boolean, Set[T]] =
    mutable.OpenHashMap[Boolean, Set[T]]()
  protected lazy val datetimeCache: mutable.OpenHashMap[Instant, Set[T]] =
    mutable.OpenHashMap[Instant, Set[T]]()
  protected lazy val localdatetimeCache: mutable.OpenHashMap[LocalDateTime, Set[T]] =
    mutable.OpenHashMap[LocalDateTime, Set[T]]()
  protected lazy val dateCache: mutable.OpenHashMap[LocalDate, Set[T]] =
    mutable.OpenHashMap[LocalDate, Set[T]]()
  protected lazy val timeCache: mutable.OpenHashMap[LocalTime, Set[T]] =
    mutable.OpenHashMap[LocalTime, Set[T]]()
  protected lazy val geopointCache: mutable.OpenHashMap[Point, Set[T]] =
    mutable.OpenHashMap[Point, Set[T]]()
  protected lazy val mapCache: mutable.OpenHashMap[Map[_, _], Set[T]] =
    mutable.OpenHashMap[Map[_, _], Set[T]]()
  protected lazy val listCache: mutable.OpenHashMap[List[_], Set[T]] =
    mutable.OpenHashMap[List[_], Set[T]]()
  protected lazy val vectorCache: mutable.OpenHashMap[Vector[_], Set[T]] =
    mutable.OpenHashMap[Vector[_], Set[T]]()

  override def byId(id: Long): Option[T] = super.byId(id).orElse(graph.storeManager.valueById(id).headOption)

  def byId(ids: List[Long]): Stream[T] = {
    val byCache          = ids.map(id => id -> byId(id))
    val (noCache, cache) = byCache.partition(_._2.isEmpty)
    (cache.flatMap(_._2) toStream) ++ (if (noCache.nonEmpty) graph.storeManager.valuesById(noCache.map(_._1))
                                       else Stream())
  }

  override def byIri(iri: String): Stream[T] = super.byIri(iri) ++ graph.storeManager.valueByIri(iri) distinct

  def byValue[V, VOut, CVOut <: DataType[VOut]](value: V)(
      implicit clsTpbl: ClassTypeable.Aux[V, VOut, CVOut]): Stream[T] =
    byValue(value, clsTpbl.ct.asInstanceOf[DataType[V]])
  def byValue[V](value: V, dt: DataType[V]): Stream[T] =
    value match {
      case value: Int =>
        intCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt).asInstanceOf[Stream[T]])
      case value: Double =>
        doubleCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt).asInstanceOf[Stream[T]])
      case value: Long =>
        longCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt).asInstanceOf[Stream[T]])
      case value: String =>
        stringCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt).asInstanceOf[Stream[T]])
      case value: Boolean =>
        booleanCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt).asInstanceOf[Stream[T]])
      case value: Instant =>
        datetimeCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt).asInstanceOf[Stream[T]])
      case value: LocalDateTime =>
        localdatetimeCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt).asInstanceOf[Stream[T]])
      case value: LocalDate =>
        dateCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt).asInstanceOf[Stream[T]])
      case value: LocalTime =>
        timeCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt).asInstanceOf[Stream[T]])
      case value: Point =>
        geopointCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt).asInstanceOf[Stream[T]])
      case value: Map[_, _] =>
        mapCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt).asInstanceOf[Stream[T]])
      case value: List[_] =>
        listCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt).asInstanceOf[Stream[T]])
      case value: Vector[_] =>
        vectorCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt).asInstanceOf[Stream[T]])
      case _ =>
        throw new Exception(s"unsupported valuestore-type, cannot find store for datatype-class ${value.getClass}")
    }

  override def store(value: T): Unit = {
    super.store(value)
    value.label match {
      case dt: IntType[_] =>
        intCache += value.value
          .asInstanceOf[Int] -> (intCache.getOrElse(value.value.asInstanceOf[Int], Set[T]()) + value)
      case dt: DoubleType[_] =>
        doubleCache += value.value
          .asInstanceOf[Double] -> (doubleCache.getOrElse(value.value.asInstanceOf[Double], Set[T]()) + value)
      case dt: LongType[_] =>
        longCache += value.value
          .asInstanceOf[Long] -> (longCache.getOrElse(value.value.asInstanceOf[Long], Set[T]()) + value)
      case dt: TextType[_] =>
        stringCache += value.value
          .asInstanceOf[String] -> (stringCache.getOrElse(value.value.asInstanceOf[String], Set[T]()) + value)
      case dt: BoolType[Boolean] =>
        booleanCache += value.value
          .asInstanceOf[Boolean] -> (booleanCache.getOrElse(value.value.asInstanceOf[Boolean], Set[T]()) + value)
      case dt: DateTimeType[_] if dt.iri == DateTimeType.datetimeType.iri =>
        datetimeCache += value.value
          .asInstanceOf[Instant] -> (datetimeCache
          .getOrElse(value.value.asInstanceOf[Instant], Set[T]()) + value)
      case dt: DateTimeType[_] if dt.iri == LocalDateTimeType.localdatetimeType.iri =>
        localdatetimeCache += value.value
          .asInstanceOf[LocalDateTime] -> (localdatetimeCache.getOrElse(value.value.asInstanceOf[LocalDateTime],
                                                                        Set[T]()) + value)
      case dt: LocalDateType[_] =>
        dateCache += value.value
          .asInstanceOf[LocalDate] -> (dateCache.getOrElse(value.value.asInstanceOf[LocalDate], Set[T]()) + value)
      case dt: LocalTimeType[_] =>
        timeCache += value.value
          .asInstanceOf[LocalTime] -> (timeCache.getOrElse(value.value.asInstanceOf[LocalTime], Set[T]()) + value)
      case dt: GeopointType[_] =>
        geopointCache += value.value
          .asInstanceOf[Point] -> (geopointCache.getOrElse(value.value.asInstanceOf[Point], Set[T]()) + value)
      case dt: MapType[_, _] =>
        mapCache += value.value
          .asInstanceOf[Map[_, _]] -> (mapCache.getOrElse(value.value.asInstanceOf[Map[_, _]], Set[T]()) + value)
      case dt: ListType[_] =>
        listCache += value.value
          .asInstanceOf[List[_]] -> (listCache.getOrElse(value.value.asInstanceOf[List[_]], Set[T]()) + value)
      case dt: VectorType[_] =>
        vectorCache += value.value
          .asInstanceOf[Vector[_]] -> (vectorCache.getOrElse(value.value.asInstanceOf[Vector[_]], Set[T]()) + value)
      case _ =>
        throw new Exception(s"unsupported valuestore-type, @type to valuestore on is ${value.label.iri}")
    }
    graph.storeManager
      .storeValues(List(value))
      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
    //      .runToFuture(monix.execution.Scheduler.global)
  }

  override def store(values: List[T]): Unit = {
    values.foreach(super.store)
    graph.storeManager
      .storeValues(values)
      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
    //      .runToFuture(monix.execution.Scheduler.global)
  }

  override def delete(value: T): Unit = {
    super.delete(value)
    value.label match {
      case dt: IntType[_] =>
        val values = intCache.getOrElse(value.value.asInstanceOf[Int], Set[T]())
        if (values.filterNot(_ == value).isEmpty) intCache -= value.value.asInstanceOf[Int]
        else intCache += value.value.asInstanceOf[Int] -> (values - value)
      case dt: DoubleType[_] =>
        val values = doubleCache.getOrElse(value.value.asInstanceOf[Double], Set[T]())
        if (values.filterNot(_ == value).isEmpty) doubleCache -= value.value.asInstanceOf[Double]
        else doubleCache += value.value.asInstanceOf[Double] -> (values - value)
      case dt: LongType[_] =>
        val values = longCache.getOrElse(value.value.asInstanceOf[Long], Set[T]())
        if (values.filterNot(_ == value).isEmpty) longCache -= value.value.asInstanceOf[Long]
        else longCache += value.value.asInstanceOf[Long] -> (values - value)
      case dt: TextType[_] =>
        val values = stringCache.getOrElse(value.value.asInstanceOf[String], Set[T]())
        if (values.filterNot(_ == value).isEmpty) stringCache -= value.value.asInstanceOf[String]
        else stringCache += value.value.asInstanceOf[String] -> (values - value)
      case dt: BoolType[Boolean] =>
        val values = booleanCache.getOrElse(value.value.asInstanceOf[Boolean], Set[T]())
        if (values.filterNot(_ == value).isEmpty) booleanCache -= value.value.asInstanceOf[Boolean]
        else booleanCache += value.value.asInstanceOf[Boolean] -> (values - value)
      case dt: DateTimeType[_] if dt.iri == DateTimeType.datetimeType.iri =>
        val values = datetimeCache.getOrElse(value.value.asInstanceOf[Instant], Set[T]())
        if (values.filterNot(_ == value).isEmpty) datetimeCache -= value.value.asInstanceOf[Instant]
        else datetimeCache += value.value.asInstanceOf[Instant] -> (values - value)
      case dt: DateTimeType[_] if dt.iri == LocalDateTimeType.localdatetimeType.iri =>
        val values = localdatetimeCache.getOrElse(value.value.asInstanceOf[LocalDateTime], Set[T]())
        if (values.filterNot(_ == value).isEmpty) localdatetimeCache -= value.value.asInstanceOf[LocalDateTime]
        else localdatetimeCache += value.value.asInstanceOf[LocalDateTime] -> (values - value)
      case dt: LocalDateType[_] =>
        val values = dateCache.getOrElse(value.value.asInstanceOf[LocalDate], Set[T]())
        if (values.filterNot(_ == value).isEmpty) dateCache -= value.value.asInstanceOf[LocalDate]
        else dateCache += value.value.asInstanceOf[LocalDate] -> (values - value)
      case dt: LocalTimeType[_] =>
        val values = timeCache.getOrElse(value.value.asInstanceOf[LocalTime], Set[T]())
        if (values.filterNot(_ == value).isEmpty) timeCache -= value.value.asInstanceOf[LocalTime]
        else timeCache += value.value.asInstanceOf[LocalTime] -> (values - value)
      case dt: GeopointType[_] =>
        val values = geopointCache.getOrElse(value.value.asInstanceOf[Point], Set[T]())
        if (values.filterNot(_ == value).isEmpty) geopointCache -= value.value.asInstanceOf[Point]
        else geopointCache += value.value.asInstanceOf[Point] -> (values - value)
      case dt: MapType[_, _] =>
        val values = mapCache.getOrElse(value.value.asInstanceOf[Map[_, _]], Set[T]())
        if (values.filterNot(_ == value).isEmpty) mapCache -= value.value.asInstanceOf[Map[_, _]]
        else mapCache += value.value.asInstanceOf[Map[_, _]] -> (values - value)
      case dt: ListType[_] =>
        val values = listCache.getOrElse(value.value.asInstanceOf[List[_]], Set[T]())
        if (values.filterNot(_ == value).isEmpty) listCache -= value.value.asInstanceOf[List[_]]
        else listCache += value.value.asInstanceOf[List[_]] -> (values - value)
      case dt: VectorType[_] =>
        val values = vectorCache.getOrElse(value.value.asInstanceOf[Vector[_]], Set[T]())
        if (values.filterNot(_ == value).isEmpty) vectorCache -= value.value.asInstanceOf[Vector[_]]
        else vectorCache += value.value.asInstanceOf[Vector[_]] -> (values - value)
      case _ =>
        throw new Exception(s"unsupported valuestore-type, @type to valuestore on is ${value.label.iri}")
    }
    graph.storeManager
      .deleteValues(List(value))
      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
    //      .runToFuture(monix.execution.Scheduler.global)
  }

  override def delete(values: List[T]): Unit = {
    values.foreach(super.delete)
    graph.storeManager
      .deleteValues(values)
      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
    //      .runToFuture(monix.execution.Scheduler.global)
  }

  def all(): Stream[graph._Value[_]] = graph.storeManager.values
  def count(): Long                  = graph.storeManager.valueCount()
}
