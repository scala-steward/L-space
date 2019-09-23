package lspace.structure

import java.util.concurrent.ConcurrentHashMap

import lspace.structure
import monix.eval.Task
import lspace.datatype.{DataType, GraphType, TextType}
import lspace.librarian.logic.{Assistent, DefaultAssistent}
import lspace.librarian.task.{AsyncGuide, Guide}
import lspace.provider.transaction.Transaction
import lspace.librarian.traversal.util.{OutTweaker, ResultMapper}
import lspace.provider.detached.DetachedGraph
import lspace.provider.mem.MemGraph
import lspace.structure.store.{EdgeStore, NodeStore, ValueStore}
import lspace.structure.util.{ClassTypeable, GraphUtils, IdProvider}
import monix.reactive.Observable
import shapeless.{::, HList, HNil}

import scala.collection.mutable

object Graph {

  /**
    * easy helper for creating simple in-memory graphs (graph can always be merged into other types of graphs, e.g. graphs which are persistent)
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

//  type _Resource[T] <: _Resource[T]
//  type GNode <: _Node             // with _Resource[Node]
//  type _Edge[S, E] <: _Edge[S, E] //with _Resource[Edge[S, E]]
//  type GValue[T] <: _Value[T]     // with _Resource[T]

  override lazy val hashCode: Int = iri.hashCode

  implicit lazy val assistent: Assistent     = DefaultAssistent()
  implicit lazy val guide: Guide[Observable] = AsyncGuide()

  lazy val thisgraph: this.type = this
  def ns: NameSpaceGraph

  protected[lspace] def idProvider: IdProvider

  /**
    * creates new transaction
    * @return
    */
  def transaction: Transaction

  protected[lspace] def nodeStore: NodeStore[this.type]
  protected[lspace] def edgeStore: EdgeStore[this.type]
  protected[lspace] def valueStore: ValueStore[this.type]
//  protected def `@idStore`: ValueStore[this.type]
//  protected def `@typeStore`: ValueStore[String, _Value[String]]

  def init: Task[Unit]

  private lazy val _resources: Resources = new Resources(this) {}
  def resources: Resources               = _resources

  /**
    * Edges A.K.A. Links A.K.A. Properties
    * @return
    */
//  def edges: Stream[Edge[_, _]] = edgeStore.toStream()
  private lazy val _edges: Edges = new Edges(this) {}
  def edges: Edges               = _edges

  /**
    * Nodes A.K.A. Vertices
    * @return
    */
//  def nodes: Stream[Node] = nodeStore.toStream()
  private lazy val _nodes: Nodes = new Nodes(this) {}
  def nodes: Nodes               = _nodes

  private lazy val _values: Values = new Values(this) {}
  def values: Values               = _values

  protected[lspace] def newNode(id: Long): GNode
  protected[lspace] def getOrCreateNode(id: Long): Task[GNode] = {
    nodeStore
      .hasId(id)
      .flatMap(_.map(Task.now).getOrElse {
        for {
          node <- Task.now(newNode(id))
          u    <- storeNode(node)
        } yield node
      })
  }
  final def +(label: Ontology): Task[Node]         = nodes.create(label)
  protected def storeNode(node: _Node): Task[Unit] = nodeStore.store(node)

  protected[lspace] def newEdge[S, E](id: Long, from: _Resource[S], key: Property, to: _Resource[E]): GEdge[S, E]

  protected[lspace] def createEdge[S, E](id: Long,
                                         from: _Resource[S],
                                         key: Property,
                                         to: _Resource[E]): Task[GEdge[S, E]] = {
//    if (ns.properties.get(key.iri).isEmpty) ns.properties.store(key)

    for {
      _ <- (if (Property.properties.default.byIri.get(key.iri).isEmpty)
              ns.properties.store(key)
            else Task.unit).startAndForget
      edge <- Task.now(newEdge(id, from, key, to))
      u    <- storeEdge(edge.asInstanceOf[_Edge[_, _]]) //.startAndForget //what if this fails?
      _ <- {
        if (edge.key == Property.default.`@id` || edge.key == Property.default.`@ids`) edge.from match {
          case node: _Node =>
            nodeStore.store(node)
          case edge: _Edge[_, _] =>
            edgeStore.store(edge /*.asInstanceOf[_Edge[_, _]]*/ )
          case value: _Value[_] =>
            valueStore.store(value.asInstanceOf[_Value[_]])
        } else Task.unit
      }
    } yield edge
  }

  protected def storeEdge(edge: _Edge[_, _]): Task[Unit] = edgeStore.store(edge)

  protected[lspace] def newValue[T](id: Long, value: T, label: DataType[T]): GValue[T]
  protected[lspace] def createValue[T](id: Long, value: T, dt: DataType[T]): Task[GValue[T]] = {
//    if (ns.datatypes.get(dt.iri).isEmpty) ns.datatypes.store(dt)
//    ns.datatypes.store(dt).runToFuture(monix.execution.Scheduler.global)
    for {
      _value <- Task.now(newValue(id, value, dt))
      u      <- storeValue(_value.asInstanceOf[_Value[_]])
    } yield _value
  }

  protected def storeValue(value: _Value[_]): Task[Unit] = valueStore.store(value)

  /**
    * deletes the Node from the graph
    * @param node
    */
  protected[lspace] def deleteNode(node: _Node): Task[Unit] = {
    for {
      _ <- deleteResource(node)
      _ <- nodeStore.delete(node)
    } yield ()
  }

  /**
    * deletes the Edge from the graph
    * @param edge
    */
  protected[lspace] def deleteEdge(edge: _Edge[_, _]): Task[Unit] = {
    for {
      _ <- deleteResource(edge)
      _ <- edgeStore.delete(edge)
    } yield ()
  }

  /**
    * deletes the Value from the graph
    * @param value
    */
  protected[lspace] def deleteValue(value: _Value[_]): Task[Unit] = {
    for {
      _ <- deleteResource(value)
      _ <- valueStore.delete(value)
    } yield ()
  }

  /**
    * TODO: rename to _deleteProperties/_deleteEdges?
    * @param resource
    */
  protected def deleteResource[T <: _Resource[_]](resource: T): Task[Unit]

  private[lspace] def addMeta[S <: Resource[_], T <: Resource[_]](source: S, target: T): Task[Unit] =
    Observable
      .fromIterable(source.outE().filterNot(p => Graph.baseKeys.contains(p.key)))
      .mapEval { edge =>
        for {
          t <- edges.create[Any, Any](target, edge.key, edge.to)
          u <- addMeta(edge, t)
        } yield u
      }
      .completedL

  def add: Graph => Task[Graph] = ++
  val ++ : Graph => Task[Graph] = (graph: Graph) => {
    if (graph != this) {
      if (graph == DetachedGraph)
        scribe.warn(
          s"adding the contents of DetachedGraph to ${this.iri} is futile as DetachedGraph does not store any objects, they float")
      for {
        oldIdNewNodeMap <- graph
          .nodes()
          .mapEval { node =>
//            nodes.create(node.labels: _*).map(node.id -> _)
//            (if (node.iri.nonEmpty) nodes.upsert(node.iri, node.labels: _*)
//             else nodes.create(node.labels: _*)).map(node.id -> _)
            nodes.upsert(node.iri, node.iris, node.labels: _*).map(node.id -> _)
          }
          .toListL
          .map(_.toMap)
        oldIdNewValueMap <- graph
          .values()
          .mapEval { value =>
            values.upsert(value).map(value.id -> _)
          }
          .toListL
          .map(_.toMap)
        r <- {
          import scala.collection.JavaConverters._
          val oldIdNewEdgeMap: scala.collection.concurrent.Map[Long, Edge[_, _]] =
            new ConcurrentHashMap[Long, Edge[_, _]]().asScala
          for {
            edges <- graph
              .edges()
              .filter(e =>
                !((e.key == Property.default.`@id` || e.key == Property.default.`@ids`) && e.from
                  .isInstanceOf[Node] && e.to.hasLabel(TextType.datatype).isDefined))
              .mapEval { edge =>
                //            if (edge.iri.nonEmpty) //TODO: find edge width
                for {
                  newEdge <- edges.create(
                    edge.from match {
                      case (resource: Node) =>
                        oldIdNewNodeMap(resource.id)
                      case (resource: Edge[_, _]) =>
                        oldIdNewEdgeMap(resource.id)
                      case (resource: Value[_]) =>
                        oldIdNewValueMap(resource.id)
                    },
                    edge.key,
                    edge.to match {
                      case (resource: Node) =>
                        oldIdNewNodeMap(resource.id)
                      case (resource: Edge[_, _]) =>
                        oldIdNewEdgeMap(resource.id)
                      case (resource: Value[_]) =>
                        oldIdNewValueMap(resource.id)
                    }
                  )
                } yield {
                  oldIdNewEdgeMap += (edge.id -> newEdge)
                }
              }
              .toListL
          } yield this
        }
      } yield r
    } else Task.now(this)
  }

//  def g[Out](traversalObservable: TraversalTask[Out]): Out = traversalObservable.run(this)
//  def *>[Out](traversalObservable: TraversalTask[Out]): Out = traversalObservable.run(this)
//  def map[T](traversalTask: TraversalTask[T]): T            = traversalTask.run(this)
  import lspace.librarian.traversal._
  def *>[ST <: ClassType[_], End, ET[+Z] <: ClassType[Z], Steps <: HList, Out, OutCT <: ClassType[_], F[_]](
      traversal: Traversal[ST, ET[End], Steps])(implicit
                                                tweaker: OutTweaker.Aux[ET[End], Steps, Out, OutCT],
                                                guide: Guide[F],
                                                mapper: ResultMapper[F, ET[End], OutCT]): mapper.FT =
    mapper.apply(traversal, this).asInstanceOf[mapper.FT]

  protected[lspace] def executeTraversal[F[_]](
      traversal: Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList],
      guide: Guide[F]): F[Any] = guide.buildTraversal[Any](traversal)(this)

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
