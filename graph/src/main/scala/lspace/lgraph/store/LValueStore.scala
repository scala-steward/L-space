package lspace.lgraph.store

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.concurrent.ConcurrentHashMap

import lspace.Label
import lspace.lgraph.LGraph
import lspace.datatype._
import lspace.structure.{ClassType, Value}
import lspace.structure.util.ClassTypeable
import lspace.structure.store.ValueStore
import lspace.types.vector.Point
import monix.eval.Task
import monix.reactive.Observable
import squants.time.Time

import scala.collection.immutable.ListSet
import scala.collection.concurrent
import scala.collection.JavaConverters._
import scala.concurrent.duration._

object LValueStore {
  def apply[G <: LGraph](iri: String, graph: G): LValueStore[G] = new LValueStore(iri, graph)
}

class LValueStore[G <: LGraph](val iri: String, val graph: G) extends LStore[G] with ValueStore[G] {

  protected lazy val intCache: concurrent.Map[Int, Set[graph._Value[Int]]] =
    new ConcurrentHashMap[Int, Set[graph._Value[Int]]]().asScala
  protected lazy val doubleCache: concurrent.Map[Double, Set[graph._Value[Double]]] =
    new ConcurrentHashMap[Double, Set[graph._Value[Double]]]().asScala
  protected lazy val longCache: concurrent.Map[Long, Set[graph._Value[Long]]] =
    new ConcurrentHashMap[Long, Set[graph._Value[Long]]]().asScala
  protected lazy val stringCache: concurrent.Map[String, Set[graph._Value[String]]] =
    new ConcurrentHashMap[String, Set[graph._Value[String]]]().asScala
  protected lazy val booleanCache: concurrent.Map[Boolean, Set[graph._Value[Boolean]]] =
    new ConcurrentHashMap[Boolean, Set[graph._Value[Boolean]]]().asScala
  protected lazy val datetimeCache: concurrent.Map[Instant, Set[graph._Value[Instant]]] =
    new ConcurrentHashMap[Instant, Set[graph._Value[Instant]]]().asScala
  protected lazy val localdatetimeCache: concurrent.Map[LocalDateTime, Set[graph._Value[LocalDateTime]]] =
    new ConcurrentHashMap[LocalDateTime, Set[graph._Value[LocalDateTime]]]().asScala
  protected lazy val dateCache: concurrent.Map[LocalDate, Set[graph._Value[LocalDate]]] =
    new ConcurrentHashMap[LocalDate, Set[graph._Value[LocalDate]]]().asScala
  protected lazy val timeCache: concurrent.Map[LocalTime, Set[graph._Value[LocalTime]]] =
    new ConcurrentHashMap[LocalTime, Set[graph._Value[LocalTime]]]().asScala
  protected lazy val durationCache: concurrent.Map[Time, Set[graph._Value[Time]]] =
    new ConcurrentHashMap[Time, Set[graph._Value[Time]]]().asScala
  protected lazy val geopointCache: concurrent.Map[Point, Set[graph._Value[Point]]] =
    new ConcurrentHashMap[Point, Set[graph._Value[Point]]]().asScala
  protected lazy val mapCache: concurrent.Map[Map[Any, Any], Set[graph._Value[Map[Any, Any]]]] =
    new ConcurrentHashMap[Map[Any, Any], Set[graph._Value[Map[Any, Any]]]]().asScala
  protected lazy val listsetCache: concurrent.Map[ListSet[Any], Set[graph._Value[ListSet[Any]]]] =
    new ConcurrentHashMap[ListSet[Any], Set[graph._Value[ListSet[Any]]]]().asScala
  protected lazy val setCache: concurrent.Map[Set[Any], Set[graph._Value[Set[Any]]]] =
    new ConcurrentHashMap[Set[Any], Set[graph._Value[Set[Any]]]]().asScala
  protected lazy val listCache: concurrent.Map[List[Any], Set[graph._Value[List[Any]]]] =
    new ConcurrentHashMap[List[Any], Set[graph._Value[List[Any]]]]().asScala
  protected lazy val vectorCache: concurrent.Map[Vector[Any], Set[graph._Value[Vector[Any]]]] =
    new ConcurrentHashMap[Vector[Any], Set[graph._Value[Vector[Any]]]]().asScala

  override def hasId(id: Long): Task[Option[T2]] =
    Task.defer {
      if (isDeleted(id)) Task.now(None)
      else
        cachedById(id) match {
          case Some(e) => Task.now(Some(e))
          case None    => graph.storeManager.valueById(id).map(_.map(_.asInstanceOf[T2]))
        }
    }

  def hasId(ids: List[Long]): Observable[T2] = {
    val (deleted, tryable) = ids.partition(isDeleted)
    val byCache            = tryable.map(id => id -> cachedById(id))
    val (noCache, cache)   = byCache.partition(_._2.isEmpty)
    Observable.fromIterable(cache.flatMap(_._2)) ++ (if (noCache.nonEmpty)
                                                       graph.storeManager
                                                         .valuesById(noCache.map(_._1))
                                                         .asInstanceOf[Observable[T2]]
                                                         .executeOn(LStore.ec)
                                                     else Observable.empty[T2])
  }

  override def hasIri(iri: String): Observable[T2] = {
    val cachedResult = cachedByIri(iri).filterNot(e => isDeleted(e.id))
    Observable.fromIterable(cachedResult) ++
      graph.storeManager
        .valueByValue(iri, Label.D.`@string`)
        .map(_.id)
        .flatMap(graph.storeManager.edgesByToIdAndKey(_, Label.P.`@id`).asInstanceOf[Observable[graph.GEdge[Any, Any]]])
        .map(_.from)
        .collect { case value: Value[_] => value }
        .asInstanceOf[Observable[T2]]
        .filter(v => !cachedResult.toSet.contains(v) && !isDeleted(v.id))
        .executeOn(LStore.ec)
  }

  def hasIri(iri: Set[String]): Observable[T2] = {
    val cachedResult = iri.flatMap(iri => cachedByIri(iri).filterNot(e => isDeleted(e.id)))
    Observable.fromIterable(cachedResult) ++
      graph.storeManager
        .valuesByValue(iri.toList.map(_ -> Label.D.`@string`))
        .map(_.id)
        .flatMap(graph.storeManager.edgesByToIdAndKey(_, Label.P.`@id`).asInstanceOf[Observable[graph.GEdge[Any, Any]]])
        .map(_.from)
        .collect { case value: Value[_] => value }
        .asInstanceOf[Observable[T2]]
        .filter(v => !cachedResult.contains(v) && !isDeleted(v.id))
        .executeOn(LStore.ec)
  }

  def byValue[V, VOut, CVOut <: DataType[VOut]](value: V)(
      implicit clsTpbl: ClassTypeable.Aux[V, VOut, CVOut]): Observable[graph._Value[V]] =
    byValue(value, clsTpbl.ct.asInstanceOf[DataType[V]])
  def byValue[V](value: V, dt: DataType[V]): Observable[graph._Value[V]] =
    (value match {
      case value: Int =>
        intCache
          .get(value)
          .map(Observable.fromIterable)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Observable[graph._Value[V]]]
      case value: Double =>
        doubleCache
          .get(value)
          .map(Observable.fromIterable)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Observable[graph._Value[V]]]
      case value: Long =>
        longCache
          .get(value)
          .map(Observable.fromIterable)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Observable[graph._Value[V]]]
      case value: String =>
        stringCache
          .get(value)
          .map(Observable.fromIterable)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Observable[graph._Value[V]]]
      case value: Boolean =>
        booleanCache
          .get(value)
          .map(Observable.fromIterable)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Observable[graph._Value[V]]]
      case value: Instant =>
        datetimeCache
          .get(value)
          .map(Observable.fromIterable)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Observable[graph._Value[V]]]
      case value: LocalDateTime =>
        localdatetimeCache
          .get(value)
          .map(Observable.fromIterable)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Observable[graph._Value[V]]]
      case value: LocalDate =>
        dateCache
          .get(value)
          .map(Observable.fromIterable)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Observable[graph._Value[V]]]
      case value: LocalTime =>
        timeCache
          .get(value)
          .map(Observable.fromIterable)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Observable[graph._Value[V]]]
      case value: Time =>
        durationCache
          .get(value)
          .map(Observable.fromIterable)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Observable[graph._Value[V]]]
      case value: Point =>
        geopointCache
          .get(value)
          .map(Observable.fromIterable)
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Observable[graph._Value[V]]]
      case value: Map[Any, Any] @unchecked =>
        mapCache
          .get(value)
          .map(Observable.fromIterable(_).filter(_.label == dt))
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Observable[graph._Value[V]]]
      case value: ListSet[Any] @unchecked =>
        listsetCache
          .get(value)
          .map(Observable.fromIterable(_).filter(_.label == dt))
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Observable[graph._Value[V]]]
      case value: Set[Any] @unchecked =>
        setCache
          .get(value)
          .map(Observable.fromIterable(_).filter(_.label == dt))
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Observable[graph._Value[V]]]
      case value: List[Any] @unchecked =>
        listCache
          .get(value)
          .map(Observable.fromIterable(_).filter(_.label == dt))
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Observable[graph._Value[V]]]
      case value: Vector[Any] @unchecked =>
        vectorCache
          .get(value)
          .map(Observable.fromIterable(_).filter(_.label == dt))
          .getOrElse(graph.storeManager.valueByValue(value, dt))
          .asInstanceOf[Observable[graph._Value[V]]]
      case _ =>
        throw new Exception(s"unsupported valuestore-type, cannot find store for datatype-class ${value.getClass}")
    }).filter(v => !isDeleted(v.id))

  override def store(value: T): Task[Unit] = {
    for {
      _ <- super.store(value)
      _ <- graph.storeManager
        .storeValues(List(value))
        .executeOn(LStore.ec)
        .forkAndForget
    } yield ()
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)
  }

  override def store(values: List[T]): Task[Unit] = {
    for {
      _ <- Task.gatherUnordered(values.map(super.store))
      _ <- graph.storeManager
        .storeValues(values)
        .executeOn(LStore.ec)
        .forkAndForget
    } yield ()
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
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
  private[this] val durationCacheLock      = new Object
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
            .asInstanceOf[graph._Value[Int]])
        }
      case dt: DoubleType[_] =>
        doubleCacheLock.synchronized {
          doubleCache += value.value
            .asInstanceOf[Double] -> (doubleCache.getOrElse(value.value.asInstanceOf[Double], Set()) + value
            .asInstanceOf[graph._Value[Double]])
        }
      case dt: LongType[_] =>
        longCacheLock.synchronized {
          longCache += value.value
            .asInstanceOf[Long] -> (longCache.getOrElse(value.value.asInstanceOf[Long], Set()) + value
            .asInstanceOf[graph._Value[Long]])
        }
      case dt: TextType[_] =>
        textCacheLock.synchronized {
          stringCache += value.value
            .asInstanceOf[String] -> (stringCache.getOrElse(value.value.asInstanceOf[String], Set()) + value
            .asInstanceOf[graph._Value[String]])
        }
      case dt: BoolType[_] =>
        boolCacheLock.synchronized {
          booleanCache += value.value
            .asInstanceOf[Boolean] -> (booleanCache.getOrElse(value.value.asInstanceOf[Boolean], Set()) + value
            .asInstanceOf[graph._Value[Boolean]])
        }
      case dt: DateTimeType[_] if dt.iri == DateTimeType.datatype.iri =>
        datetimeCacheLock.synchronized {
          datetimeCache += value.value
            .asInstanceOf[Instant] -> (datetimeCache
            .getOrElse(value.value.asInstanceOf[Instant], Set()) + value.asInstanceOf[graph._Value[Instant]])
        }
      case dt: DateTimeType[_] if dt.iri == LocalDateTimeType.datatype.iri =>
        localdatetimeCacheLock.synchronized {
          localdatetimeCache += value.value
            .asInstanceOf[LocalDateTime] -> (localdatetimeCache
            .getOrElse(value.value.asInstanceOf[LocalDateTime], Set()) + value
            .asInstanceOf[graph._Value[LocalDateTime]])
        }
      case dt: LocalDateType[_] =>
        localdateCacheLock.synchronized {
          dateCache += value.value
            .asInstanceOf[LocalDate] -> (dateCache.getOrElse(value.value.asInstanceOf[LocalDate], Set()) + value
            .asInstanceOf[graph._Value[LocalDate]])
        }
      case dt: LocalTimeType[_] =>
        localtimeCacheLock.synchronized {
          timeCache += value.value
            .asInstanceOf[LocalTime] -> (timeCache.getOrElse(value.value.asInstanceOf[LocalTime], Set()) + value
            .asInstanceOf[graph._Value[LocalTime]])
        }
      case dt: DurationType =>
        durationCacheLock.synchronized {
          durationCache += value.value
            .asInstanceOf[Time] -> (durationCache.getOrElse(value.value.asInstanceOf[Time], Set()) + value
            .asInstanceOf[graph._Value[Time]])
        }
      case dt: GeopointType[_] =>
        geopointCacheLock.synchronized {
          geopointCache += value.value
            .asInstanceOf[Point] -> (geopointCache.getOrElse(value.value.asInstanceOf[Point], Set()) + value
            .asInstanceOf[graph._Value[Point]])
        }
      case dt: MapType[_, _] =>
        mapCacheLock.synchronized {
          mapCache += value.value
            .asInstanceOf[Map[Any, Any]] -> (mapCache.getOrElse(value.value.asInstanceOf[Map[Any, Any]], Set()) + value
            .asInstanceOf[graph._Value[Map[Any, Any]]])
        }
      case dt: ListSetType[_] =>
        listsetCacheLock.synchronized {
          listsetCache += value.value
            .asInstanceOf[ListSet[Any]] -> (listsetCache.getOrElse(value.value.asInstanceOf[ListSet[Any]], Set()) + value
            .asInstanceOf[graph._Value[ListSet[Any]]])
        }
      case dt: SetType[_] =>
        setCacheLock.synchronized {
          setCache += value.value
            .asInstanceOf[Set[Any]] -> (setCache.getOrElse(value.value.asInstanceOf[Set[Any]], Set()) + value
            .asInstanceOf[graph._Value[Set[Any]]])
        }
      case dt: ListType[_] =>
        listCacheLock.synchronized {
          listCache += value.value
            .asInstanceOf[List[Any]] -> (listCache.getOrElse(value.value.asInstanceOf[List[Any]], Set()) + value
            .asInstanceOf[graph._Value[List[Any]]])
        }
      case dt: VectorType[_] =>
        vectorCacheLock.synchronized {
          vectorCache += value.value
            .asInstanceOf[Vector[Any]] -> (vectorCache.getOrElse(value.value.asInstanceOf[Vector[Any]], Set()) + value
            .asInstanceOf[graph._Value[Vector[Any]]])
        }
      case _ =>
        throw new Exception(s"unsupported valuestore-type, @type to valuestore on is ${value.label.iri}")
    }
  }

  override def delete(value: T): Task[Unit] = Task.defer {
    _deleted += value.id -> Instant.now()
    for {
      _ <- super.delete(value)
      _ <- graph.storeManager
        .deleteValues(List(value))
        .executeOn(LStore.ec)
        .forkAndForget
    } yield ()
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
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
          else intCache += value.value.asInstanceOf[Int] -> (values - value.asInstanceOf[graph._Value[Int]])
        }
      case dt: DoubleType[_] =>
        doubleCacheLock.synchronized {
          val values = doubleCache.getOrElse(value.value.asInstanceOf[Double], Set())
          if (values.exists(_ == value)) doubleCache -= value.value.asInstanceOf[Double]
          else doubleCache += value.value.asInstanceOf[Double] -> (values - value.asInstanceOf[graph._Value[Double]])
        }
      case dt: LongType[_] =>
        longCacheLock.synchronized {
          val values = longCache.getOrElse(value.value.asInstanceOf[Long], Set())
          if (values.exists(_ == value)) longCache -= value.value.asInstanceOf[Long]
          else longCache += value.value.asInstanceOf[Long] -> (values - value.asInstanceOf[graph._Value[Long]])
        }
      case dt: TextType[_] =>
        textCacheLock.synchronized {
          val values = stringCache.getOrElse(value.value.asInstanceOf[String], Set())
          if (values.exists(_ == value)) stringCache -= value.value.asInstanceOf[String]
          else stringCache += value.value.asInstanceOf[String] -> (values - value.asInstanceOf[graph._Value[String]])
        }
      case dt: BoolType[_] =>
        boolCacheLock.synchronized {
          val values = booleanCache.getOrElse(value.value.asInstanceOf[Boolean], Set())
          if (values.exists(_ == value)) booleanCache -= value.value.asInstanceOf[Boolean]
          else booleanCache += value.value.asInstanceOf[Boolean] -> (values - value.asInstanceOf[graph._Value[Boolean]])
        }
      case dt: DateTimeType[_] if dt.iri == DateTimeType.datatype.iri =>
        datetimeCacheLock.synchronized {
          val values = datetimeCache.getOrElse(value.value.asInstanceOf[Instant], Set())
          if (values.exists(_ == value)) datetimeCache -= value.value.asInstanceOf[Instant]
          else
            datetimeCache += value.value.asInstanceOf[Instant] -> (values - value.asInstanceOf[graph._Value[Instant]])
        }
      case dt: DateTimeType[_] if dt.iri == LocalDateTimeType.datatype.iri =>
        localdatetimeCacheLock.synchronized {
          val values = localdatetimeCache.getOrElse(value.value.asInstanceOf[LocalDateTime], Set())
          if (values.exists(_ == value)) localdatetimeCache -= value.value.asInstanceOf[LocalDateTime]
          else
            localdatetimeCache += value.value
              .asInstanceOf[LocalDateTime] -> (values - value.asInstanceOf[graph._Value[LocalDateTime]])
        }
      case dt: LocalDateType[_] =>
        localdateCacheLock.synchronized {
          val values = dateCache.getOrElse(value.value.asInstanceOf[LocalDate], Set())
          if (values.exists(_ == value)) dateCache -= value.value.asInstanceOf[LocalDate]
          else
            dateCache += value.value.asInstanceOf[LocalDate] -> (values - value.asInstanceOf[graph._Value[LocalDate]])
        }
      case dt: LocalTimeType[_] =>
        localtimeCacheLock.synchronized {
          val values = timeCache.getOrElse(value.value.asInstanceOf[LocalTime], Set())
          if (values.exists(_ == value)) timeCache -= value.value.asInstanceOf[LocalTime]
          else
            timeCache += value.value.asInstanceOf[LocalTime] -> (values - value.asInstanceOf[graph._Value[LocalTime]])
        }
      case dt: DurationType =>
        durationCacheLock.synchronized {
          val values = durationCache.getOrElse(value.value.asInstanceOf[Time], Set())
          if (values.exists(_ == value)) durationCache -= value.value.asInstanceOf[Time]
          else
            durationCache += value.value.asInstanceOf[Time] -> (values - value.asInstanceOf[graph._Value[Time]])
        }
      case dt: GeopointType[_] =>
        geopointCacheLock.synchronized {
          val values = geopointCache.getOrElse(value.value.asInstanceOf[Point], Set())
          if (values.exists(_ == value)) geopointCache -= value.value.asInstanceOf[Point]
          else geopointCache += value.value.asInstanceOf[Point] -> (values - value.asInstanceOf[graph._Value[Point]])
        }
      case dt: MapType[_, _] =>
        mapCacheLock.synchronized {
          val values = mapCache.getOrElse(value.value.asInstanceOf[Map[Any, Any]], Set())
          if (values.exists(_ == value)) mapCache -= value.value.asInstanceOf[Map[Any, Any]]
          else
            mapCache += value.value
              .asInstanceOf[Map[Any, Any]] -> (values - value.asInstanceOf[graph._Value[Map[Any, Any]]])
        }
      case dt: ListSetType[_] =>
        listsetCacheLock.synchronized {
          val values = listsetCache.getOrElse(value.value.asInstanceOf[ListSet[Any]], Set())
          if (values.exists(_ == value)) listsetCache -= value.value.asInstanceOf[ListSet[Any]]
          else
            listsetCache += value.value
              .asInstanceOf[ListSet[Any]] -> (values - value.asInstanceOf[graph._Value[ListSet[Any]]])
        }
      case dt: SetType[_] =>
        setCacheLock.synchronized {
          val values = setCache.getOrElse(value.value.asInstanceOf[Set[Any]], Set())
          if (values.exists(_ == value)) setCache -= value.value.asInstanceOf[Set[Any]]
          else setCache += value.value.asInstanceOf[Set[Any]] -> (values - value.asInstanceOf[graph._Value[Set[Any]]])
        }
      case dt: ListType[_] =>
        listCacheLock.synchronized {
          val values = listCache.getOrElse(value.value.asInstanceOf[List[Any]], Set())
          if (values.exists(_ == value)) listCache -= value.value.asInstanceOf[List[Any]]
          else
            listCache += value.value.asInstanceOf[List[Any]] -> (values - value.asInstanceOf[graph._Value[List[Any]]])
        }
      case dt: VectorType[_] =>
        vectorCacheLock.synchronized {
          val values = vectorCache.getOrElse(value.value.asInstanceOf[Vector[Any]], Set())
          if (values.exists(_ == value)) vectorCache -= value.value.asInstanceOf[Vector[Any]]
          else
            vectorCache += value.value
              .asInstanceOf[Vector[Any]] -> (values - value.asInstanceOf[graph._Value[Vector[Any]]])
        }
      case _ =>
        throw new Exception(s"unsupported valuestore-type, @type to valuestore on is ${value.label.iri}")
    }
  }

  override def delete(values: List[T]): Task[Unit] = Task.defer {
    val delTime = Instant.now()
    values.foreach(value => _deleted += value.id -> delTime)
    for {
      _ <- Task.gatherUnordered(values.map(super.delete))
      _ <- graph.storeManager
        .deleteValues(values)
        .executeOn(LStore.ec)
        .forkAndForget
    } yield ()
//      .runSyncUnsafe(15 seconds)(monix.execution.Scheduler.global, monix.execution.schedulers.CanBlock.permit)
//      .runToFuture(monix.execution.Scheduler.global)
  }

  override def purge: Task[Unit] =
    for {
      _ <- super.purge
      _ <- Task {
        intCache.clear()
        doubleCache.clear()
        longCache.clear()
        stringCache.clear()
        booleanCache.clear()
        datetimeCache.clear()
        localdatetimeCache.clear()
        dateCache.clear()
        timeCache.clear()
        durationCache.clear()
        geopointCache.clear()
        mapCache.clear()
        listsetCache.clear()
        setCache.clear()
        listCache.clear()
        vectorCache.clear()
      }
    } yield ()

  def all(): Observable[T2] = {
    val cached = _cache.values
    Observable.fromIterable(cached).filter(n => !isDeleted(n.id)) ++ graph.storeManager.values
      .filter(!cached.toSet.contains(_))
      .executeOn(LStore.ec)
  }
  def count(): Task[Long] = graph.storeManager.valueCount()
}
