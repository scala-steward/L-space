package lspace.structure

import lspace.datatype.{DataType, GraphType, TextType}
import lspace.librarian.task.Guide
import lspace.librarian.traversal.util.{EndMapper, ResultMapper}
import lspace.provider.detached.DetachedGraph
import lspace.provider.mem.MemGraph
import lspace.provider.transaction.Transaction
import lspace.structure
import lspace.structure.store.{EdgeStore, NodeStore, ValueStore}
import lspace.structure.util.{ClassTypeable, GraphUtils, IdProvider, UpsertHelper}
import monix.eval.Task
import monix.reactive.Observable
import shapeless.HList

import scala.collection.mutable

object Graph {

  /** easy helper for creating simple in-memory graphs (graph can always be merged into other types of graphs, e.g. graphs which are persistent)
    * @param iri
    * @return
    */
  def apply(iri: String): Graph = MemGraph(iri)

  val graphs: mutable.HashMap[String, Graph] = mutable.HashMap[String, Graph]()
  lazy val baseKeys = Set(
    Property.default.`@id`,
    Property.default.`@ids`,
    Property.default.`@type`,
    Property.default.`@createdon`,
    Property.default.`@modifiedon`,
    Property.default.`@deletedon`,
    Property.default.`@transcendedon`
  )
//  lazy val reservedKeys = Set(
//    Property.default.`@id`,
////    Property.default.`@value`,
//    Property.default.`@ids`,
//    Property.default.`@type`,
////    Property.default.`@container`,
////    Property.default.`@label`,
////    Property.default.`@comment`,
//    Property.default.`@createdon`,
//    Property.default.`@modifiedon`,
//    Property.default.`@deletedon`,
//    Property.default.`@transcendedon`,
//    Property.default.`@properties`,
//    Property.default.`@graph`,
//    Property.default.`@range`,
//    Property.default.`@extends`
//  )

  //  graphs += "detached" -> DetachedGraph
//  implicit def dt[T <: Graph](implicit ev: T <:< Graph) = new IriType[T] {
//    val iri: String = ldcontext.types.graph
//  }
  implicit def default[T <: Graph](implicit ev: T <:< Graph): ClassTypeable.Aux[T, T, DataType[T]] =
    new ClassTypeable[T] {
      type C  = T
      type CT = DataType[T]
      def ct: CT = GraphType[T]
    }

  lspace.librarian.traversal.step.Step.steps
    .map(_.ontology)
  lspace.librarian.logic.predicate.P.predicates
    .map(_.ontology)
  lspace.librarian.traversal.step.Step.steps
    .map(_.properties)
  lspace.librarian.logic.predicate.P.predicates
    .map(_.properties)
}

trait Graph extends IriResource with GraphUtils { self =>

  trait _Resource[+T]        extends structure.Resource[T]
  abstract class _Node       extends _Resource[structure.Node] with Node
  abstract class _Edge[S, E] extends _Resource[structure.Edge[S, E]] with Edge[S, E]
  abstract class _Value[T]   extends _Resource[T] with Value[T]

  type GResource[T] <: _Resource[T]
  type GNode <: _Node
  type GEdge[S, E] <: _Edge[S, E]
  type GValue[T] <: _Value[T]

  override lazy val hashCode: Int = iri.hashCode

  lazy val thisgraph: this.type = this
  def ns: NameSpaceGraph

  protected[lspace] def idProvider: IdProvider

  /** creates new transaction
    * @return
    */
  def transaction: Transaction

  /** Store of all nodes
    * @return
    */
  protected[lspace] def nodeStore: NodeStore[this.type]

  /** Store of all edges
    * @return
    */
  protected[lspace] def edgeStore: EdgeStore[this.type]

  /** Store of all values
    * @return
    */
  protected[lspace] def valueStore: ValueStore[this.type]

  def init: Task[Unit]

  /** Resources, an aggration of nodes, edges and values
    */
  def resources: Resources               = _resources
  private lazy val _resources: Resources = new Resources(this) {}

  /** Edges Aka Links Aka Properties
    */
  def edges: Edges               = _edges
  private lazy val _edges: Edges = new Edges(this) {}

  /** Nodes Aka Vertices
    */
  def nodes: Nodes               = _nodes
  private lazy val _nodes: Nodes = new Nodes(this) {}

  private lazy val _values: Values = new Values(this) {}
  def values: Values               = _values

  protected[lspace] def newNode(id: Long): GNode
  protected[lspace] def getOrCreateNode(id: Long): Task[GNode] =
    nodeStore
      .hasId(id)
      .flatMap(_.map(Task.now).getOrElse {
        for {
          node <- Task.now(newNode(id))
          _    <- storeNode(node)
        } yield node
      })
  final def +(label: Ontology): Task[Node]         = nodes.create(label)
  protected def storeNode(node: GNode): Task[Unit] = nodeStore.store(node)

  protected[lspace] def newEdge[S, E](id: Long, from: _Resource[S], key: Property, to: _Resource[E]): GEdge[S, E]

  protected[lspace] def createEdge[S, E](
    id: Long,
    from: _Resource[S],
    key: Property,
    to: _Resource[E]
  ): Task[GEdge[S, E]] =
//    if (ns.properties.get(key.iri).isEmpty) ns.properties.store(key)
    for {
      _ <- (if (!Property.properties.default.byIri.contains(key.iri))
              ns.properties.store(key)
            else Task.unit).startAndForget
      edge <- Task.now(newEdge(id, from, key, to))
      _    <- storeEdge(edge) //.startAndForget //what if this fails?
      _ <- {
        if (edge.key == Property.default.`@id` || edge.key == Property.default.`@ids`) edge.from match {
          case node: _Node =>
            nodeStore.store(node)
          case edge: _Edge[_, _] =>
            edgeStore.store(edge /*.asInstanceOf[_Edge[_, _]]*/ )
          case value: _Value[_] =>
            valueStore.store(value.asInstanceOf[_Value[_]])
        }
        else Task.unit
      }
    } yield edge

  protected def storeEdge(edge: GEdge[_, _]): Task[Unit] = edgeStore.store(edge)

  protected[lspace] def newValue[T](id: Long, value: T, label: DataType[T]): GValue[T]
  protected[lspace] def createValue[T](id: Long, value: T, dt: DataType[T]): Task[GValue[T]] =
//    if (ns.datatypes.get(dt.iri).isEmpty) ns.datatypes.store(dt)
//    ns.datatypes.store(dt).runToFuture(monix.execution.Scheduler.global)
    for {
      _value <- Task.now(newValue(id, value, dt))
      _      <- storeValue(_value)
    } yield _value

  protected def storeValue(value: GValue[_]): Task[Unit] = valueStore.store(value)

  /** deletes the Node from the graph
    * @param node
    */
  protected[lspace] def deleteNode(node: _Node): Task[Unit] =
    for {
      _ <- deleteResource(node)
      _ <- nodeStore.delete(node)
    } yield ()

  /** deletes the Edge from the graph
    * @param edge
    */
  protected[lspace] def deleteEdge(edge: _Edge[_, _]): Task[Unit] =
    for {
      _ <- deleteResource(edge)
      _ <- edgeStore.delete(edge)
    } yield ()

  /** deletes the Value from the graph
    * @param value
    */
  protected[lspace] def deleteValue(value: _Value[_]): Task[Unit] =
    for {
      _ <- deleteResource(value)
      _ <- valueStore.delete(value)
    } yield ()

  /** TODO: rename to _deleteProperties/_deleteEdges?
    * @param resource
    */
  protected def deleteResource[T <: _Resource[_]](resource: T): Task[Unit]

  //TODO: break graph cycles
  protected[lspace] def addMeta[S <: Resource[_], T <: Resource[_]](source: S, target: T)(implicit
    helper: UpsertHelper = UpsertHelper()
  ): Task[Unit] =
    Observable
      .fromIterable(source.outE().filterNot(p => Graph.baseKeys.contains(p.key)))
      .mapEval { edge =>
        for {
          t <- helper.createEdge(edge.id, edges.create[Any, Any](target, edge.key, edge.to))
          u <- addMeta(edge, t)
        } yield u
      }
      .completedL

  def add: Graph => Task[Graph] = ++

  val ++ : Graph => Task[Graph] = (graph: Graph) => {
    implicit val helper = new UpsertHelper()
    if (graph != this) {
      if (graph == DetachedGraph)
        scribe.warn(
          s"adding the contents of DetachedGraph to ${this.iri} has zero effect as DetachedGraph does not store any objects, they float"
        )
      for {
        _ <- graph
          .nodes()
          .mapParallelUnordered(8) { node =>
//            nodes.create(node.labels: _*).map(node.id -> _)
//            (if (node.iri.nonEmpty) nodes.upsert(node.iri, node.labels: _*)
//             else nodes.create(node.labels: _*)).map(node.id -> _)
            nodes.upsert(node)
          }
          .completedL
        _ <- graph
          .values()
          .mapParallelUnordered(8) { value =>
            values.upsert(value)
          }
          .completedL
        r <- {
          for {
            _ <- graph
              .edges()
              .filter(e =>
                !((e.key == Property.default.`@id` || e.key == Property.default.`@ids`) && e.from
                  .isInstanceOf[Node] && e.to.hasLabel(TextType.datatype).isDefined)
              )
              .mapParallelUnordered(8) { edge =>
                //            if (edge.iri.nonEmpty) //TODO: find edge width
                helper.mergeEdge(edge)(self).onErrorHandle { e: Throwable =>
                  scribe.error(e.getMessage)
                }
              }
              .toListL
            _ <- helper.retryEdges(
              self
            ) //TODO: improve adding/retrying edges (chains of edges / edges-on-edges), recursive? or merge from/to ahead of time?
            _ <- helper.retryEdges(
              self
            ) //Possible strategy: only recurse when the number of edges-to-retry declines after an iteration
            _ <- helper.retryEdges(self)
            _ <- helper.retryEdges(self)
            _ <- helper.retryEdges(self)
          } yield this
        }
      } yield r
    } else Task.now(this)
  }

  import lspace.Traversal
  def *>[ST <: ClassType[_], End, ET[+Z] <: ClassType[Z], Steps <: HList, Out, OutCT <: ClassType[_], F[_], FT](
    traversal: Traversal[ST, ET[End], Steps]
  )(implicit
    tweaker: EndMapper.Aux[ET[End], Steps, Out, OutCT],
    guide: Guide[F],
    mapper: ResultMapper.Aux[F, ET[End], OutCT, FT]
  ): FT =
    mapper.apply(traversal, this)

  protected[lspace] def traverse[F[_]](
    traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList],
    guide: Guide[F]
  ): F[Any] = guide.buildTraversal[Any](traversal)(this)

  def persist: Task[Unit] = Task.unit

  def purge: Task[Unit] =
    for {
      _ <- nodeStore.purge
      _ <- edgeStore.purge
      _ <- valueStore.purge
    } yield ()

  def close(): Task[Unit] = Task.unit

  override def toString: String = s"graph:$iri"
}
