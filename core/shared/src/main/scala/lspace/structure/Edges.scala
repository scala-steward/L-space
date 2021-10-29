package lspace.structure

import lspace.structure.util.UpsertHelper
import monix.eval.Task
import monix.reactive.Observable

abstract class Edges(val graph: Graph) extends RApi[Edge[Any, Any]] {
  import graph._

  def apply(): Observable[Edge[Any, Any]] = edgeStore.all()
  def count(): Task[Long]                 = edgeStore.count()

  def hasId(id: Long): Task[Option[Edge[Any, Any]]] = edgeStore.hasId(id)
  override def hasIri(iris: List[String]): Observable[Edge[Any, Any]] = {
    val validIris = iris.filter(_.nonEmpty)
    if (validIris.nonEmpty)
      Observable
        .fromIterable(validIris)
        .flatMap(iri =>
          edgeStore
            .hasIri(iri)
            .asInstanceOf[Observable[Edge[_, _]]]
        ) // distinct
    else Observable[Edge[_, _]]()
  }

  def cached: Cached = new Cached {
    def hasId(id: Long): Option[Edge[Any, Any]] =
      edgeStore.cached.hasId(id)
    def dereferenceValue(t: Any): Any = t
    def count: Long                   = edgeStore.cached.count
  }

  /** creates and stores an edge
    * @param from
    * @param key
    * @param to
    * @tparam S
    * @tparam E
    * @return
    */
  final def create[S, E](from: Resource[S], key: Property, to: Resource[E])(implicit
    helper: UpsertHelper = UpsertHelper()
  ): Task[Edge[S, E]] = {
    val _from = from match {
      case from: _Node       => Task.now(from.asInstanceOf[_Resource[S]])
      case from: _Edge[_, _] => Task.now(from.asInstanceOf[_Resource[S]])
      case from: _Value[_]   => Task.now(from.asInstanceOf[_Resource[S]])
      case _ =>
        resources
          .hasIri(from.iri)
          .headOptionL
          .flatMap(
            _.map(Task.now)
              .getOrElse(resources.upsert(from))
              .asInstanceOf[Task[_Resource[S]]]
          )
    }
    val _to = to match {
      case to: _Node       => Task.now(to.asInstanceOf[_Resource[E]])
      case to: _Edge[_, _] => Task.now(to.asInstanceOf[_Resource[E]])
      case to: _Value[_]   => Task.now(to.asInstanceOf[_Resource[E]])
      case _ =>
        resources
          .hasIri(to.iri)
          .headOptionL
          .flatMap(
            _.map(Task.now)
              .getOrElse(resources.upsert(to))
              .asInstanceOf[Task[_Resource[E]]]
          )
    }
    for {
      from <- _from
      to   <- _to
      id   <- idProvider.next
      edge <- createEdge(id, from, key, to).onErrorHandle { f =>
        println(f.getMessage); throw f
      }
    } yield edge
  }

  def upsert[S, E](edge: Edge[S, E])(implicit helper: UpsertHelper = UpsertHelper()): Task[Edge[S, E]] =
    if (edge.graph != this) {
      helper.createEdge(
        edge.id,
        for {
          from    <- resources.upsert(edge.from)
          to      <- resources.upsert(edge.to)
          newEdge <- from.addOut(edge.key, to)
        } yield newEdge
      )
    } else Task.now(edge)

  /** adds an edge and upserts the 'from' and 'to' resource and adds (meta) edges on edges as long as edges have edges
    * @param edge
    * @tparam S
    * @tparam E
    * @return
    */
  def post[S, E](edge: Edge[S, E])(implicit helper: UpsertHelper = UpsertHelper()): Task[Edge[S, E]] =
    if (edge.graph != this) {
      for {
        from    <- resources.upsert(edge.from)
        to      <- resources.upsert(edge.to)
        newEdge <- from.addOut(edge.key, to)
        _       <- addMeta(edge, newEdge)
      } yield newEdge
    } else Task.now(edge)

  final def delete(edge: Edge[_, _]): Task[Unit] = edge match {
    case edge: _Edge[_, _] => deleteEdge(edge.asInstanceOf[_Edge[_, _]])
    case _                 => Task.unit // LOG???
  }

  /** adds an edge by reference (from --- key --> to)
    * @param edge
    * @tparam S
    * @tparam E
    * @return
    */
  final def +[S, E](edge: Edge[S, E]): Task[Edge[S, E]] = upsert(edge)

  /** deletes an edge
    * @param edge
    */
  final def -(edge: Edge[_, _]): Task[Unit] = delete(edge)

  /** adds an edge by reference (from --- key --> to) and meta-properties (edge.outE())
    * @param edge
    * @tparam S
    * @tparam E
    * @return
    */
  final def ++[S, E](edge: Edge[S, E]): Task[Edge[S, E]] = post(edge)
}
