package lspace.librarian.provider.mem.store

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.concurrent.ConcurrentHashMap

import lspace.librarian.datatype._
import lspace.librarian.provider.mem.MemGraph
import lspace.librarian.structure.Property.default.{`@id`, `@ids`}
import lspace.librarian.structure.{ClassType, DataType, Value}
import lspace.librarian.structure.store.ValueStore
import lspace.types.vector.Point

import scala.collection.immutable.ListSet
import scala.collection._
import scala.collection.JavaConverters._

object MemValueStore {
  def apply[G <: MemGraph](iri: String, graph: G): MemValueStore[G] = new MemValueStore(iri, graph)
}

class MemValueStore[G <: MemGraph](val iri: String, val graph: G) extends MemStore[G] with ValueStore[G] {

  protected lazy val intCache: concurrent.Map[Int, Set[graph.GValue[Int]]] =
    new ConcurrentHashMap[Int, Set[graph.GValue[Int]]]().asScala
  protected lazy val doubleCache: concurrent.Map[Double, Set[graph.GValue[Double]]] =
    new ConcurrentHashMap[Double, Set[graph.GValue[Double]]]().asScala
  protected lazy val longCache: concurrent.Map[Long, Set[graph.GValue[Long]]] =
    new ConcurrentHashMap[Long, Set[graph.GValue[Long]]]().asScala
  protected lazy val stringCache: concurrent.Map[String, Set[graph.GValue[String]]] =
    new ConcurrentHashMap[String, Set[graph.GValue[String]]]().asScala
  protected lazy val booleanCache: concurrent.Map[Boolean, Set[graph.GValue[Boolean]]] =
    new ConcurrentHashMap[Boolean, Set[graph.GValue[Boolean]]]().asScala
  protected lazy val datetimeCache: concurrent.Map[Instant, Set[graph.GValue[Instant]]] =
    new ConcurrentHashMap[Instant, Set[graph.GValue[Instant]]]().asScala
  protected lazy val localdatetimeCache: concurrent.Map[LocalDateTime, Set[graph.GValue[LocalDateTime]]] =
    new ConcurrentHashMap[LocalDateTime, Set[graph.GValue[LocalDateTime]]]().asScala
  protected lazy val dateCache: concurrent.Map[LocalDate, Set[graph.GValue[LocalDate]]] =
    new ConcurrentHashMap[LocalDate, Set[graph.GValue[LocalDate]]]().asScala
  protected lazy val timeCache: concurrent.Map[LocalTime, Set[graph.GValue[LocalTime]]] =
    new ConcurrentHashMap[LocalTime, Set[graph.GValue[LocalTime]]]().asScala
  protected lazy val geopointCache: concurrent.Map[Point, Set[graph.GValue[Point]]] =
    new ConcurrentHashMap[Point, Set[graph.GValue[Point]]]().asScala
  protected lazy val mapCache: concurrent.Map[Map[Any, Any], Set[graph.GValue[Map[Any, Any]]]] =
    new ConcurrentHashMap[Map[Any, Any], Set[graph.GValue[Map[Any, Any]]]]().asScala
  protected lazy val listsetCache: concurrent.Map[ListSet[Any], Set[graph.GValue[ListSet[Any]]]] =
    new ConcurrentHashMap[ListSet[Any], Set[graph.GValue[ListSet[Any]]]]().asScala
  protected lazy val setCache: concurrent.Map[Set[Any], Set[graph.GValue[Set[Any]]]] =
    new ConcurrentHashMap[Set[Any], Set[graph.GValue[Set[Any]]]]().asScala
  protected lazy val listCache: concurrent.Map[List[Any], Set[graph.GValue[List[Any]]]] =
    new ConcurrentHashMap[List[Any], Set[graph.GValue[List[Any]]]]().asScala
  protected lazy val vectorCache: concurrent.Map[Vector[Any], Set[graph.GValue[Vector[Any]]]] =
    new ConcurrentHashMap[Vector[Any], Set[graph.GValue[Vector[Any]]]]().asScala

  def byValue[V](value: V, dt: DataType[V]): Stream[graph.GValue[V]] = {
    value match {
      case value: Int           => intCache.get(value).toStream.flatMap(_.toList)
      case value: Double        => doubleCache.get(value).toStream.flatMap(_.toList)
      case value: Long          => longCache.get(value).toStream.flatMap(_.toList)
      case value: String        => stringCache.get(value).toStream.flatMap(_.toList)
      case value: Boolean       => booleanCache.get(value).toStream.flatMap(_.toList)
      case value: Instant       => datetimeCache.get(value).toStream.flatMap(_.toList)
      case value: LocalDateTime => localdatetimeCache.get(value).toStream.flatMap(_.toList)
      case value: LocalDate     => dateCache.get(value).toStream.flatMap(_.toList)
      case value: LocalTime     => timeCache.get(value).toStream.flatMap(_.toList)
      case value: Point         => geopointCache.get(value).toStream.flatMap(_.toList)
      case value: Map[Any, Any] => mapCache.get(value).toStream.flatMap(_.toList)
      case value: ListSet[Any]  => listsetCache.get(value).toStream.flatMap(_.toList)
      case value: Set[Any]      => setCache.get(value).toStream.flatMap(_.toList)
      case value: List[Any]     => listCache.get(value).toStream.flatMap(_.toList)
      case value: Vector[Any]   => vectorCache.get(value).toStream.flatMap(_.toList)
      case _ =>
        throw new Exception(s"unsupported valuestore-type, cannot find store for datatype-class ${value.getClass}")
    }
  }.asInstanceOf[Stream[graph.GValue[V]]]

  override def store(value: T): Unit = {
    super.store(value)
    val label = if (value.label.iri.nonEmpty) value.label else ClassType.valueToOntologyResource(value.value)
    label match {
      case dt: IntType[_] =>
        intCache += value.value
          .asInstanceOf[Int] -> (intCache.getOrElse(value.value.asInstanceOf[Int], Set()) + value
          .asInstanceOf[graph.GValue[Int]])
      case dt: DoubleType[_] =>
        doubleCache += value.value
          .asInstanceOf[Double] -> (doubleCache.getOrElse(value.value.asInstanceOf[Double], Set()) + value
          .asInstanceOf[graph.GValue[Double]])
      case dt: LongType[_] =>
        longCache += value.value
          .asInstanceOf[Long] -> (longCache.getOrElse(value.value.asInstanceOf[Long], Set()) + value
          .asInstanceOf[graph.GValue[Long]])
      case dt: TextType[_] =>
        stringCache += value.value
          .asInstanceOf[String] -> (stringCache.getOrElse(value.value.asInstanceOf[String], Set()) + value
          .asInstanceOf[graph.GValue[String]])
      case dt: BoolType[Boolean] =>
        booleanCache += value.value
          .asInstanceOf[Boolean] -> (booleanCache.getOrElse(value.value.asInstanceOf[Boolean], Set()) + value
          .asInstanceOf[graph.GValue[Boolean]])
      case dt: DateTimeType[_] if dt.iri == DateTimeType.datetimeType.iri =>
        datetimeCache += value.value
          .asInstanceOf[Instant] -> (datetimeCache
          .getOrElse(value.value.asInstanceOf[Instant], Set()) + value.asInstanceOf[graph.GValue[Instant]])
      case dt: DateTimeType[_] if dt.iri == LocalDateTimeType.localdatetimeType.iri =>
        localdatetimeCache += value.value
          .asInstanceOf[LocalDateTime] -> (localdatetimeCache
          .getOrElse(value.value.asInstanceOf[LocalDateTime], Set()) + value.asInstanceOf[graph.GValue[LocalDateTime]])
      case dt: LocalDateType[_] =>
        dateCache += value.value
          .asInstanceOf[LocalDate] -> (dateCache.getOrElse(value.value.asInstanceOf[LocalDate], Set()) + value
          .asInstanceOf[graph.GValue[LocalDate]])
      case dt: LocalTimeType[_] =>
        timeCache += value.value
          .asInstanceOf[LocalTime] -> (timeCache.getOrElse(value.value.asInstanceOf[LocalTime], Set()) + value
          .asInstanceOf[graph.GValue[LocalTime]])
      case dt: GeopointType[_] =>
        geopointCache += value.value
          .asInstanceOf[Point] -> (geopointCache.getOrElse(value.value.asInstanceOf[Point], Set()) + value
          .asInstanceOf[graph.GValue[Point]])
      case dt: MapType[_, _] =>
        mapCache += value.value
          .asInstanceOf[Map[Any, Any]] -> (mapCache.getOrElse(value.value.asInstanceOf[Map[Any, Any]], Set()) + value
          .asInstanceOf[graph.GValue[Map[Any, Any]]])
      case dt: ListSetType[_] =>
        listsetCache += value.value
          .asInstanceOf[ListSet[Any]] -> (listsetCache.getOrElse(value.value.asInstanceOf[ListSet[Any]], Set()) + value
          .asInstanceOf[graph.GValue[ListSet[Any]]])
      case dt: SetType[_] =>
        setCache += value.value
          .asInstanceOf[Set[Any]] -> (setCache.getOrElse(value.value.asInstanceOf[Set[Any]], Set()) + value
          .asInstanceOf[graph.GValue[Set[Any]]])
      case dt: ListType[_] =>
        listCache += value.value
          .asInstanceOf[List[Any]] -> (listCache.getOrElse(value.value.asInstanceOf[List[Any]], Set()) + value
          .asInstanceOf[graph.GValue[List[Any]]])
      case dt: VectorType[_] =>
        vectorCache += value.value
          .asInstanceOf[Vector[Any]] -> (vectorCache.getOrElse(value.value.asInstanceOf[Vector[Any]], Set()) + value
          .asInstanceOf[graph.GValue[Vector[Any]]])
      case _ =>
        throw new Exception(
          s"unsupported valuestore-type, @type to valuestore on is ${value.label.iri} and value ${value.value}")
    }
  }

  override def store(resources: List[T]): Unit = {
    resources.foreach(store)
  }

  def byIri(iri: String): Stream[T2] =
    graph.`@idStore`.byValue(iri, DataType.default.`@string`)
      .flatMap(_.in(`@id`, `@ids`).filter(_.isInstanceOf[Value[_]]))
      .asInstanceOf[Stream[T2]]
      .distinct

  override def delete(value: T): Unit = {
    super.delete(value)
    val label = if (value.label.iri.nonEmpty) value.label else ClassType.valueToOntologyResource(value.value)
    label match {
      case dt: IntType[_] =>
        val values = intCache.getOrElse(value.value.asInstanceOf[Int], Set())
        if (values.exists(_ == value)) intCache -= value.value.asInstanceOf[Int]
        else intCache += value.value.asInstanceOf[Int] -> (values - value.asInstanceOf[graph.GValue[Int]])
      case dt: DoubleType[_] =>
        val values = doubleCache.getOrElse(value.value.asInstanceOf[Double], Set())
        if (values.exists(_ == value)) doubleCache -= value.value.asInstanceOf[Double]
        else doubleCache += value.value.asInstanceOf[Double] -> (values - value.asInstanceOf[graph.GValue[Double]])
      case dt: LongType[_] =>
        val values = longCache.getOrElse(value.value.asInstanceOf[Long], Set())
        if (values.exists(_ == value)) longCache -= value.value.asInstanceOf[Long]
        else longCache += value.value.asInstanceOf[Long] -> (values - value.asInstanceOf[graph.GValue[Long]])
      case dt: TextType[_] =>
        val values = stringCache.getOrElse(value.value.asInstanceOf[String], Set())
        if (values.exists(_ == value)) stringCache -= value.value.asInstanceOf[String]
        else stringCache += value.value.asInstanceOf[String] -> (values - value.asInstanceOf[graph.GValue[String]])
      case dt: BoolType[Boolean] =>
        val values = booleanCache.getOrElse(value.value.asInstanceOf[Boolean], Set())
        if (values.exists(_ == value)) booleanCache -= value.value.asInstanceOf[Boolean]
        else booleanCache += value.value.asInstanceOf[Boolean] -> (values - value.asInstanceOf[graph.GValue[Boolean]])
      case dt: DateTimeType[_] if dt.iri == DateTimeType.datetimeType.iri =>
        val values = datetimeCache.getOrElse(value.value.asInstanceOf[Instant], Set())
        if (values.exists(_ == value)) datetimeCache -= value.value.asInstanceOf[Instant]
        else datetimeCache += value.value.asInstanceOf[Instant] -> (values - value.asInstanceOf[graph.GValue[Instant]])
      case dt: DateTimeType[_] if dt.iri == LocalDateTimeType.localdatetimeType.iri =>
        val values = localdatetimeCache.getOrElse(value.value.asInstanceOf[LocalDateTime], Set())
        if (values.exists(_ == value)) localdatetimeCache -= value.value.asInstanceOf[LocalDateTime]
        else
          localdatetimeCache += value.value
            .asInstanceOf[LocalDateTime] -> (values - value.asInstanceOf[graph.GValue[LocalDateTime]])
      case dt: LocalDateType[_] =>
        val values = dateCache.getOrElse(value.value.asInstanceOf[LocalDate], Set())
        if (values.exists(_ == value)) dateCache -= value.value.asInstanceOf[LocalDate]
        else dateCache += value.value.asInstanceOf[LocalDate] -> (values - value.asInstanceOf[graph.GValue[LocalDate]])
      case dt: LocalTimeType[_] =>
        val values = timeCache.getOrElse(value.value.asInstanceOf[LocalTime], Set())
        if (values.exists(_ == value)) timeCache -= value.value.asInstanceOf[LocalTime]
        else timeCache += value.value.asInstanceOf[LocalTime] -> (values - value.asInstanceOf[graph.GValue[LocalTime]])
      case dt: GeopointType[_] =>
        val values = geopointCache.getOrElse(value.value.asInstanceOf[Point], Set())
        if (values.exists(_ == value)) geopointCache -= value.value.asInstanceOf[Point]
        else geopointCache += value.value.asInstanceOf[Point] -> (values - value.asInstanceOf[graph.GValue[Point]])
      case dt: MapType[_, _] =>
        val values = mapCache.getOrElse(value.value.asInstanceOf[Map[Any, Any]], Set())
        if (values.exists(_ == value)) mapCache -= value.value.asInstanceOf[Map[Any, Any]]
        else
          mapCache += value.value
            .asInstanceOf[Map[Any, Any]] -> (values - value.asInstanceOf[graph.GValue[Map[Any, Any]]])
      case dt: ListSetType[_] =>
        val values = listsetCache.getOrElse(value.value.asInstanceOf[ListSet[Any]], Set())
        if (values.exists(_ == value)) listsetCache -= value.value.asInstanceOf[ListSet[Any]]
        else
          listsetCache += value.value
            .asInstanceOf[ListSet[Any]] -> (values - value.asInstanceOf[graph.GValue[ListSet[Any]]])
      case dt: SetType[_] =>
        val values = setCache.getOrElse(value.value.asInstanceOf[Set[Any]], Set())
        if (values.exists(_ == value)) setCache -= value.value.asInstanceOf[Set[Any]]
        else setCache += value.value.asInstanceOf[Set[Any]] -> (values - value.asInstanceOf[graph.GValue[Set[Any]]])
      case dt: ListType[_] =>
        val values = listCache.getOrElse(value.value.asInstanceOf[List[Any]], Set())
        if (values.exists(_ == value)) listCache -= value.value.asInstanceOf[List[Any]]
        else listCache += value.value.asInstanceOf[List[Any]] -> (values - value.asInstanceOf[graph.GValue[List[Any]]])
      case dt: VectorType[_] =>
        val values = vectorCache.getOrElse(value.value.asInstanceOf[Vector[Any]], Set())
        if (values.exists(_ == value)) vectorCache -= value.value.asInstanceOf[Vector[Any]]
        else
          vectorCache += value.value
            .asInstanceOf[Vector[Any]] -> (values - value.asInstanceOf[graph.GValue[Vector[Any]]])
      case _ =>
        throw new Exception(s"unsupported valuestore-type, @type to valuestore on is ${value.label.iri}")
    }
  }
}
