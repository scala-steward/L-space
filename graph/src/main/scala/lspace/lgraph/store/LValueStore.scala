package lspace.lgraph.store

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.concurrent.ConcurrentHashMap

import lspace.lgraph.LGraph
import lspace.datatype._
import lspace.structure.ClassType
import lspace.structure.util.ClassTypeable
import lspace.structure.store.ValueStore
import lspace.types.vector.Point

import scala.collection.immutable.ListSet
import scala.collection.concurrent
import scala.collection.JavaConverters._
import scala.concurrent.duration._

object LValueStore {
  def apply[G <: LGraph](iri: String, graph: G): LValueStore[G] = new LValueStore(iri, graph)
}

class LValueStore[G <: LGraph](val iri: String, val graph: G) extends LStore[G] with ValueStore[G] {

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

  override def hasId(id: Long): Option[T2] = cachedById(id).orElse(graph.storeManager.valueById(id).headOption)

  def hasId(ids: List[Long]): Stream[T2] = {
    val (deleted, tryable) = ids.partition(isDeleted)
    val byCache            = tryable.map(id => id -> cachedById(id))
    val (noCache, cache)   = byCache.partition(_._2.isEmpty)
    (cache.flatMap(_._2) toStream) ++ (if (noCache.nonEmpty) graph.storeManager.valuesById(noCache.map(_._1))
                                       else Stream())
  }

  override def hasIri(iri: String): Stream[T2] =
    cachedByIri(iri).filterNot(v => isDeleted(v.id)) ++ graph.storeManager.valueByIri(iri) distinct

  def byValue[V, VOut, CVOut <: DataType[VOut]](value: V)(
      implicit clsTpbl: ClassTypeable.Aux[V, VOut, CVOut]): Stream[graph.GValue[V]] =
    byValue(value, clsTpbl.ct.asInstanceOf[DataType[V]])
  def byValue[V](value: V, dt: DataType[V]): Stream[graph.GValue[V]] =
    (value match {
      case value: Int =>
        intCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Stream[graph.GValue[V]]]
      case value: Double =>
        doubleCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Stream[graph.GValue[V]]]
      case value: Long =>
        longCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Stream[graph.GValue[V]]]
      case value: String =>
        stringCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Stream[graph.GValue[V]]]
      case value: Boolean =>
        booleanCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Stream[graph.GValue[V]]]
      case value: Instant =>
        datetimeCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Stream[graph.GValue[V]]]
      case value: LocalDateTime =>
        localdatetimeCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Stream[graph.GValue[V]]]
      case value: LocalDate =>
        dateCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Stream[graph.GValue[V]]]
      case value: LocalTime =>
        timeCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Stream[graph.GValue[V]]]
      case value: Point =>
        geopointCache
          .get(value)
          .map(_.toStream)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Stream[graph.GValue[V]]]
      case value: Map[Any, Any] =>
        mapCache
          .get(value)
          .map(_.toStream.filter(_.label == dt))
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Stream[graph.GValue[V]]]
      case value: ListSet[Any] =>
        listsetCache
          .get(value)
          .map(_.toStream.filter(_.label == dt))
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Stream[graph.GValue[V]]]
      case value: Set[Any] =>
        setCache
          .get(value)
          .map(_.toStream.filter(_.label == dt))
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Stream[graph.GValue[V]]]
      case value: List[Any] =>
        listCache
          .get(value)
          .map(_.toStream.filter(_.label == dt))
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Stream[graph.GValue[V]]]
      case value: Vector[Any] =>
        vectorCache
          .get(value)
          .map(_.toStream.filter(_.label == dt))
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Stream[graph.GValue[V]]]
      case _ =>
        throw new Exception(s"unsupported valuestore-type, cannot find store for datatype-class ${value.getClass}")
    }).filterNot(v => isDeleted(v.id))

  override def store(value: T): Unit = {
    super.store(value)
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

  override def cache(value: T): Unit = {
    super.cache(value)
    cacheByValue(value)
  }

  private[this] val intCacheLock           = new Object
  private[this] val doubleCacheLock        = new Object
  private[this] val longCacheLock          = new Object
  private[this] val textCacheLock          = new Object
  private[this] val boolCacheLock          = new Object
  private[this] val datetimeCacheLock      = new Object
  private[this] val localdatetimeCacheLock = new Object
  private[this] val localdateCacheLock     = new Object
  private[this] val localtimeCacheLock     = new Object
  private[this] val geopointCacheLock      = new Object
  private[this] val mapCacheLock           = new Object
  private[this] val listsetCacheLock       = new Object
  private[this] val setCacheLock           = new Object
  private[this] val listCacheLock          = new Object
  private[this] val vectorCacheLock        = new Object

  def cacheByValue(value: T): Unit = {
    val label = if (value.label.iri.nonEmpty) value.label else ClassType.valueToOntologyResource(value.value)
    label match {
      case dt: IntType[_] =>
        intCacheLock.synchronized {
          intCache += value.value
            .asInstanceOf[Int] -> (intCache.getOrElse(value.value.asInstanceOf[Int], Set()) + value
            .asInstanceOf[graph.GValue[Int]])
        }
      case dt: DoubleType[_] =>
        doubleCacheLock.synchronized {
          doubleCache += value.value
            .asInstanceOf[Double] -> (doubleCache.getOrElse(value.value.asInstanceOf[Double], Set()) + value
            .asInstanceOf[graph.GValue[Double]])
        }
      case dt: LongType[_] =>
        longCacheLock.synchronized {
          longCache += value.value
            .asInstanceOf[Long] -> (longCache.getOrElse(value.value.asInstanceOf[Long], Set()) + value
            .asInstanceOf[graph.GValue[Long]])
        }
      case dt: TextType[_] =>
        textCacheLock.synchronized {
          stringCache += value.value
            .asInstanceOf[String] -> (stringCache.getOrElse(value.value.asInstanceOf[String], Set()) + value
            .asInstanceOf[graph.GValue[String]])
        }
      case dt: BoolType[Boolean] =>
        boolCacheLock.synchronized {
          booleanCache += value.value
            .asInstanceOf[Boolean] -> (booleanCache.getOrElse(value.value.asInstanceOf[Boolean], Set()) + value
            .asInstanceOf[graph.GValue[Boolean]])
        }
      case dt: DateTimeType[_] if dt.iri == DateTimeType.datatype.iri =>
        datetimeCacheLock.synchronized {
          datetimeCache += value.value
            .asInstanceOf[Instant] -> (datetimeCache
            .getOrElse(value.value.asInstanceOf[Instant], Set()) + value.asInstanceOf[graph.GValue[Instant]])
        }
      case dt: DateTimeType[_] if dt.iri == LocalDateTimeType.datatype.iri =>
        localdatetimeCacheLock.synchronized {
          localdatetimeCache += value.value
            .asInstanceOf[LocalDateTime] -> (localdatetimeCache
            .getOrElse(value.value.asInstanceOf[LocalDateTime], Set()) + value
            .asInstanceOf[graph.GValue[LocalDateTime]])
        }
      case dt: LocalDateType[_] =>
        localdateCacheLock.synchronized {
          dateCache += value.value
            .asInstanceOf[LocalDate] -> (dateCache.getOrElse(value.value.asInstanceOf[LocalDate], Set()) + value
            .asInstanceOf[graph.GValue[LocalDate]])
        }
      case dt: LocalTimeType[_] =>
        localtimeCacheLock.synchronized {
          timeCache += value.value
            .asInstanceOf[LocalTime] -> (timeCache.getOrElse(value.value.asInstanceOf[LocalTime], Set()) + value
            .asInstanceOf[graph.GValue[LocalTime]])
        }
      case dt: GeopointType[_] =>
        geopointCacheLock.synchronized {
          geopointCache += value.value
            .asInstanceOf[Point] -> (geopointCache.getOrElse(value.value.asInstanceOf[Point], Set()) + value
            .asInstanceOf[graph.GValue[Point]])
        }
      case dt: MapType[_, _] =>
        mapCacheLock.synchronized {
          mapCache += value.value
            .asInstanceOf[Map[Any, Any]] -> (mapCache.getOrElse(value.value.asInstanceOf[Map[Any, Any]], Set()) + value
            .asInstanceOf[graph.GValue[Map[Any, Any]]])
        }
      case dt: ListSetType[_] =>
        listsetCacheLock.synchronized {
          listsetCache += value.value
            .asInstanceOf[ListSet[Any]] -> (listsetCache.getOrElse(value.value.asInstanceOf[ListSet[Any]], Set()) + value
            .asInstanceOf[graph.GValue[ListSet[Any]]])
        }
      case dt: SetType[_] =>
        setCacheLock.synchronized {
          setCache += value.value
            .asInstanceOf[Set[Any]] -> (setCache.getOrElse(value.value.asInstanceOf[Set[Any]], Set()) + value
            .asInstanceOf[graph.GValue[Set[Any]]])
        }
      case dt: ListType[_] =>
        listCacheLock.synchronized {
          listCache += value.value
            .asInstanceOf[List[Any]] -> (listCache.getOrElse(value.value.asInstanceOf[List[Any]], Set()) + value
            .asInstanceOf[graph.GValue[List[Any]]])
        }
      case dt: VectorType[_] =>
        vectorCacheLock.synchronized {
          vectorCache += value.value
            .asInstanceOf[Vector[Any]] -> (vectorCache.getOrElse(value.value.asInstanceOf[Vector[Any]], Set()) + value
            .asInstanceOf[graph.GValue[Vector[Any]]])
        }
      case _ =>
        throw new Exception(s"unsupported valuestore-type, @type to valuestore on is ${value.label.iri}")
    }
  }

  override def delete(value: T): Unit = {
    _deleted += value.id -> Instant.now()
    super.delete(value)
    graph.storeManager
      .deleteValues(List(value))
      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)
  }

  override def uncache(value: T): Unit = {
    super.uncache(value)
    uncacheByValue(value)
  }

  def uncacheByValue(value: T): Unit = {
    val label = if (value.label.iri.nonEmpty) value.label else ClassType.valueToOntologyResource(value.value)
    label match {
      case dt: IntType[_] =>
        intCacheLock.synchronized {
          val values = intCache.getOrElse(value.value.asInstanceOf[Int], Set())
          if (values.exists(_ == value)) intCache -= value.value.asInstanceOf[Int]
          else intCache += value.value.asInstanceOf[Int] -> (values - value.asInstanceOf[graph.GValue[Int]])
        }
      case dt: DoubleType[_] =>
        doubleCacheLock.synchronized {
          val values = doubleCache.getOrElse(value.value.asInstanceOf[Double], Set())
          if (values.exists(_ == value)) doubleCache -= value.value.asInstanceOf[Double]
          else doubleCache += value.value.asInstanceOf[Double] -> (values - value.asInstanceOf[graph.GValue[Double]])
        }
      case dt: LongType[_] =>
        longCacheLock.synchronized {
          val values = longCache.getOrElse(value.value.asInstanceOf[Long], Set())
          if (values.exists(_ == value)) longCache -= value.value.asInstanceOf[Long]
          else longCache += value.value.asInstanceOf[Long] -> (values - value.asInstanceOf[graph.GValue[Long]])
        }
      case dt: TextType[_] =>
        textCacheLock.synchronized {
          val values = stringCache.getOrElse(value.value.asInstanceOf[String], Set())
          if (values.exists(_ == value)) stringCache -= value.value.asInstanceOf[String]
          else stringCache += value.value.asInstanceOf[String] -> (values - value.asInstanceOf[graph.GValue[String]])
        }
      case dt: BoolType[Boolean] =>
        boolCacheLock.synchronized {
          val values = booleanCache.getOrElse(value.value.asInstanceOf[Boolean], Set())
          if (values.exists(_ == value)) booleanCache -= value.value.asInstanceOf[Boolean]
          else booleanCache += value.value.asInstanceOf[Boolean] -> (values - value.asInstanceOf[graph.GValue[Boolean]])
        }
      case dt: DateTimeType[_] if dt.iri == DateTimeType.datatype.iri =>
        datetimeCacheLock.synchronized {
          val values = datetimeCache.getOrElse(value.value.asInstanceOf[Instant], Set())
          if (values.exists(_ == value)) datetimeCache -= value.value.asInstanceOf[Instant]
          else
            datetimeCache += value.value.asInstanceOf[Instant] -> (values - value.asInstanceOf[graph.GValue[Instant]])
        }
      case dt: DateTimeType[_] if dt.iri == LocalDateTimeType.datatype.iri =>
        localdatetimeCacheLock.synchronized {
          val values = localdatetimeCache.getOrElse(value.value.asInstanceOf[LocalDateTime], Set())
          if (values.exists(_ == value)) localdatetimeCache -= value.value.asInstanceOf[LocalDateTime]
          else
            localdatetimeCache += value.value
              .asInstanceOf[LocalDateTime] -> (values - value.asInstanceOf[graph.GValue[LocalDateTime]])
        }
      case dt: LocalDateType[_] =>
        localdateCacheLock.synchronized {
          val values = dateCache.getOrElse(value.value.asInstanceOf[LocalDate], Set())
          if (values.exists(_ == value)) dateCache -= value.value.asInstanceOf[LocalDate]
          else
            dateCache += value.value.asInstanceOf[LocalDate] -> (values - value.asInstanceOf[graph.GValue[LocalDate]])
        }
      case dt: LocalTimeType[_] =>
        localtimeCacheLock.synchronized {
          val values = timeCache.getOrElse(value.value.asInstanceOf[LocalTime], Set())
          if (values.exists(_ == value)) timeCache -= value.value.asInstanceOf[LocalTime]
          else
            timeCache += value.value.asInstanceOf[LocalTime] -> (values - value.asInstanceOf[graph.GValue[LocalTime]])
        }
      case dt: GeopointType[_] =>
        geopointCacheLock.synchronized {
          val values = geopointCache.getOrElse(value.value.asInstanceOf[Point], Set())
          if (values.exists(_ == value)) geopointCache -= value.value.asInstanceOf[Point]
          else geopointCache += value.value.asInstanceOf[Point] -> (values - value.asInstanceOf[graph.GValue[Point]])
        }
      case dt: MapType[_, _] =>
        mapCacheLock.synchronized {
          val values = mapCache.getOrElse(value.value.asInstanceOf[Map[Any, Any]], Set())
          if (values.exists(_ == value)) mapCache -= value.value.asInstanceOf[Map[Any, Any]]
          else
            mapCache += value.value
              .asInstanceOf[Map[Any, Any]] -> (values - value.asInstanceOf[graph.GValue[Map[Any, Any]]])
        }
      case dt: ListSetType[_] =>
        listsetCacheLock.synchronized {
          val values = listsetCache.getOrElse(value.value.asInstanceOf[ListSet[Any]], Set())
          if (values.exists(_ == value)) listsetCache -= value.value.asInstanceOf[ListSet[Any]]
          else
            listsetCache += value.value
              .asInstanceOf[ListSet[Any]] -> (values - value.asInstanceOf[graph.GValue[ListSet[Any]]])
        }
      case dt: SetType[_] =>
        setCacheLock.synchronized {
          val values = setCache.getOrElse(value.value.asInstanceOf[Set[Any]], Set())
          if (values.exists(_ == value)) setCache -= value.value.asInstanceOf[Set[Any]]
          else setCache += value.value.asInstanceOf[Set[Any]] -> (values - value.asInstanceOf[graph.GValue[Set[Any]]])
        }
      case dt: ListType[_] =>
        listCacheLock.synchronized {
          val values = listCache.getOrElse(value.value.asInstanceOf[List[Any]], Set())
          if (values.exists(_ == value)) listCache -= value.value.asInstanceOf[List[Any]]
          else
            listCache += value.value.asInstanceOf[List[Any]] -> (values - value.asInstanceOf[graph.GValue[List[Any]]])
        }
      case dt: VectorType[_] =>
        vectorCacheLock.synchronized {
          val values = vectorCache.getOrElse(value.value.asInstanceOf[Vector[Any]], Set())
          if (values.exists(_ == value)) vectorCache -= value.value.asInstanceOf[Vector[Any]]
          else
            vectorCache += value.value
              .asInstanceOf[Vector[Any]] -> (values - value.asInstanceOf[graph.GValue[Vector[Any]]])
        }
      case _ =>
        throw new Exception(s"unsupported valuestore-type, @type to valuestore on is ${value.label.iri}")
    }
  }

  override def delete(values: List[T]): Unit = {
    val delTime = Instant.now()
    values.foreach(value => _deleted += value.id -> delTime)
    values.foreach(super.delete)
    graph.storeManager
      .deleteValues(values)
      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)
  }

  def all(): Stream[T2] =
    _cache.toStream.map(_._2).filterNot(v => isDeleted(v.id)) ++ graph.storeManager.values distinct
  def count(): Long = graph.storeManager.valueCount()
}
