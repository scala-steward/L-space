package lspace.structure

import java.util.concurrent.ConcurrentHashMap

import lspace.structure
import monix.eval.Task
import lspace.librarian.traversal._
import lspace.structure.Property.default
import lspace.structure.util.ClassTypeable
import lspace.datatype.{DataType, GraphType, TextType}
import lspace.librarian.logic.{Assistent, DefaultAssistent}
import lspace.librarian.task.{AsyncGuide, Guide, ZeroOrOneResult}
import lspace.provider.transaction.Transaction
import lspace.provider.wrapped.WrappedResource
import lspace.librarian.traversal.Traversal
import lspace.librarian.traversal.Traversal.SegmentMapper
import lspace.provider.mem.MemGraph
import lspace.structure.store.{EdgeStore, NodeStore, ValueStore}
import lspace.structure.util.{ClassTypeable, GraphUtils, IdProvider}
import monix.execution.{Cancelable, CancelableFuture}
import monix.reactive.Observable
import shapeless.ops.hlist.{Collect, Reverse}
import shapeless.{::, HList, HNil}

import scala.collection.immutable.ListSet
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
  lazy val reservedKeys = Set(
    Property.default.`@id`,
    Property.default.`@value`,
    Property.default.`@ids`,
    Property.default.`@type`,
    Property.default.`@container`,
    Property.default.`@label`,
    Property.default.`@comment`,
    Property.default.`@createdon`,
    Property.default.`@modifiedon`,
    Property.default.`@deletedon`,
    Property.default.`@transcendedon`,
    Property.default.`@properties`,
    Property.default.`@graph`,
    Property.default.`@range`,
    Property.default.`@extends`
  )

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
  abstract class _Node       extends _Resource[structure.Node] with structure.Node
  abstract class _Edge[S, E] extends _Resource[structure.Edge[S, E]] with structure.Edge[S, E]
  abstract class _Value[T]   extends _Resource[T] with structure.Value[T]

  type GResource[T] <: _Resource[T]
  type GNode <: _Node             // with GResource[Node]
  type GEdge[S, E] <: _Edge[S, E] //with GResource[Edge[S, E]]
  type GValue[T] <: _Value[T]     // with GResource[T]

  override lazy val hashCode: Int = iri.hashCode

  implicit lazy val assistent: Assistent     = DefaultAssistent()
  implicit lazy val guide: Guide[Observable] = AsyncGuide()

  lazy val thisgraph: this.type = this
  def ns: NameSpaceGraph

  def idProvider: IdProvider

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

  def init: CancelableFuture[Unit]

  sealed trait RApi[T <: Resource[_]] {

    def apply(): Observable[T]

    def hasIri(iri: String, iris: String*): Observable[T] = hasIri(iri :: iris.toList)

    /**
      *
      * @param iris a set of uri's to get T for
      * @return
      */
    def hasIri(iris: List[String]): Observable[T]

    def hasId(id: Long): Task[Option[T]]
    def cached: {
      def hasId(id: Long): Option[T]
      def dereferenceValue(t: Any): Any
    }

  }
  trait Resources extends RApi[Resource[_]] {
    def apply(): Observable[Resource[_]] = nodes() ++ edges() ++ values()
    def count(): Task[Long]              = Task.gather(List(nodeStore.count(), edgeStore.count(), valueStore.count())).map(_.sum)

    def hasIri(iris: List[String]): Observable[Resource[_]] = {
      val validIris = iris.filter(_.nonEmpty)
      if (validIris.nonEmpty) {
        Observable
          .fromIterable(validIris)
          .flatMap(
            iri =>
              nodeStore
                .hasIri(iri)
                .asInstanceOf[Observable[Resource[_]]] ++ edgeStore.hasIri(iri).asInstanceOf[Observable[Resource[_]]])
          .asInstanceOf[Observable[Resource[_]]]
      } else Observable[Resource[_]]()
    }

    def upsert[V](resource: Resource[V]): Task[Resource[V]] = {
      upsertR(resource)
//      value match {
//        case resource: Resource[_] => upsertR(resource).asInstanceOf[Resource[V]]
//        case value                 => values.create(value).asInstanceOf[Resource[V]]
//      }
    }
    private def upsertR[V](value: Resource[V]): Task[Resource[V]] = {
      value match {
//        case resource: _Resource[V]       => resource
        case resource: WrappedResource[V] => upsertR(resource.self)
        case resource: Resource[V] =>
          resource match {
            case node: _Node =>
              Task.now(node)
            case node: Node =>
              nodes.upsert(node).asInstanceOf[Task[Resource[V]]]
            case edge: _Edge[_, _] => Task.now(edge)
            case edge: Edge[_, _] =>
              edges.upsert(edge).asInstanceOf[Task[Resource[V]]]
            case value: _Value[V] =>
              Task.now(value)
            case value: Value[V] =>
              values.upsert(value).asInstanceOf[Task[Resource[V]]]
            case _ =>
              scribe.error(s"cannot upsert value with class ${value.getClass.toString}")
              Task.raiseError(new Exception("???"))
          }
      }
    }

    def hasId(id: Long): Task[Option[Resource[_]]] =
      for {
        nodeOption              <- nodes.hasId(id)
        nodeOrEdgeOption        <- if (nodeOption.nonEmpty) Task.now(nodeOption) else edges.hasId(id)
        nodeOrEdgeOrValueOption <- if (nodeOrEdgeOption.nonEmpty) Task.now(nodeOrEdgeOption) else values.hasId(id)
      } yield nodeOrEdgeOrValueOption

    lazy val cached = new {
      def hasId(id: Long): Option[Resource[_]] =
        nodeStore.cached.hasId(id).orElse(edgeStore.cached.hasId(id)).orElse(valueStore.cached.hasId(id))

      def dereferenceValue(t: Any): Any = t match {
        case v: Vector[_]  => v.map(dereferenceValue)
        case v: ListSet[_] => v.map(dereferenceValue)
        case v: List[_]    => v.map(dereferenceValue)
        case v: Set[_]     => v.map(dereferenceValue)
        case v: Map[_, _] =>
          v.map {
            case (key, value) =>
              dereferenceValue(key) -> dereferenceValue(value)
          }
        case (v1, v2) =>
          (dereferenceValue(v1), dereferenceValue(v2))
        case (v1, v2, v3) =>
          (dereferenceValue(v1), dereferenceValue(v2), dereferenceValue(v3))
        case (v1, v2, v3, v4) =>
          (dereferenceValue(v1), dereferenceValue(v2), dereferenceValue(v3), dereferenceValue(v4))
        case (v1, v2, v3, v4, v5) =>
          (dereferenceValue(v1), dereferenceValue(v2), dereferenceValue(v3), dereferenceValue(v4), dereferenceValue(v5))
        //      case v: Ontology    => nodes.upsert(v.iri, Ontology.ontology) //ns.ontologies.store(v))
        //      case v: Property    => nodes.upsert(v.iri, Property.ontology) //(ns.properties.store(v))
        //      case v: DataType[_] => nodes.upsert(v.iri, DataType.ontology) //ns.datatypes.store(v))
        case v: _Node       => v
        case v: Node        => nodeStore.cached.hasId(v.id).getOrElse(newNode(v.id))
        case v: _Edge[_, _] => v
        case v: Edge[_, _] =>
          edgeStore.cached
            .hasId(v.id)
            .getOrElse(
              newEdge[Any, Any](v.id,
                                cache(v.from).asInstanceOf[GResource[Any]],
                                v.key,
                                cache(v.to).asInstanceOf[GResource[Any]]))
        case v: _Value[_] => v
        case v: Value[_]  => valueStore.cached.hasId(v.id).getOrElse(newValue(v.id, dereferenceValue(v.value), v.label))
        case _            => t
      }
    }

    protected[lspace] def cache[T](resource: Resource[T]): _Resource[T] = resource match {
      case node: _Node       => node.asInstanceOf[_Resource[T]]
      case node: Node        => newNode(node.id).asInstanceOf[_Resource[T]]
      case edge: _Edge[_, _] => edge.asInstanceOf[_Resource[T]]
      case edge: Edge[_, _] =>
        newEdge[Any, Any](edge.id,
                          cache(edge.from).asInstanceOf[GResource[Any]],
                          edge.key,
                          cache(edge.to).asInstanceOf[GResource[Any]]).asInstanceOf[_Resource[T]]
      case value: _Value[_] => value.asInstanceOf[_Resource[T]]
      case value: Value[_] =>
        newValue(value.id, cached.dereferenceValue(value.value), value.label).asInstanceOf[_Resource[T]]
    }
  }

  private lazy val _resources: Resources = new Resources {}
  def resources: Resources               = _resources

  trait Edges extends RApi[Edge[_, _]] {
    def apply(): Observable[Edge[_, _]] = edgeStore.all()
    def count(): Task[Long]             = edgeStore.count()

    def hasId(id: Long): Task[Option[Edge[_, _]]] = edgeStore.hasId(id)
    override def hasIri(iris: List[String]): Observable[Edge[_, _]] = {
      val validIris = iris.filter(_.nonEmpty)
      if (validIris.nonEmpty)
        Observable
          .fromIterable(validIris)
          .flatMap(
            iri =>
              edgeStore
                .hasIri(iri)
                .asInstanceOf[Observable[Edge[_, _]]]) // distinct
      else Observable[Edge[_, _]]()
    }

    def cached = new {
      def hasId(id: Long): Option[Edge[_, _]] =
        edgeStore.cached.hasId(id)
      def dereferenceValue(t: Any): Any = t
    }

    /**
      * creates and stores an edge
      * @param from
      * @param key
      * @param to
      * @tparam S
      * @tparam E
      * @return
      */
    final def create[S, E](from: Resource[S], key: Property, to: Resource[E]): Task[Edge[S, E]] = {
      val _from = from match {
        case from: _Node       => Task.now(from.asInstanceOf[GResource[S]])
        case from: _Edge[_, _] => Task.now(from.asInstanceOf[GResource[S]])
        case from: _Value[_]   => Task.now(from.asInstanceOf[GResource[S]])
        case _ =>
          resources
            .hasIri(from.iri)
            .headOptionL
            .flatMap(
              _.map(Task.now)
                .getOrElse(resources.upsert(from))
                .asInstanceOf[Task[GResource[S]]])
      }
      val _to = to match {
        case to: _Node       => Task.now(to.asInstanceOf[GResource[E]])
        case to: _Edge[_, _] => Task.now(to.asInstanceOf[GResource[E]])
        case to: _Value[_]   => Task.now(to.asInstanceOf[GResource[E]])
        case _ =>
          resources
            .hasIri(to.iri)
            .headOptionL
            .flatMap(
              _.map(Task.now)
                .getOrElse(resources.upsert(to))
                .asInstanceOf[Task[GResource[E]]])
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

    def upsert[S, E](edge: Edge[S, E]): Task[Edge[S, E]] = {
      if (edge.graph != this) {
        for {
          from    <- resources.upsert(edge.from)
          to      <- resources.upsert(edge.to)
          newEdge <- from.addOut(edge.key, to)
        } yield newEdge
      } else Task.now(edge)
    }

    /**
      * adds an edge and upserts the 'from' and 'to' resource and adds (meta) edges on edges as long as edges have edges
      * @param edge
      * @tparam S
      * @tparam E
      * @tparam T
      * @return
      */
    def post[S, E](edge: Edge[S, E]): Task[Edge[S, E]] = {
      if (edge.graph != this) {
        for {
          from    <- resources.upsert(edge.from)
          to      <- resources.upsert(edge.to)
          newEdge <- from.addOut(edge.key, to)
          u       <- addMeta(edge, newEdge)
        } yield newEdge
      } else Task.now(edge)
    }

    final def delete(edge: Edge[_, _]): Task[Unit] = edge match {
      case edge: _Edge[_, _] => deleteEdge(edge.asInstanceOf[GEdge[_, _]])
      case _                 => Task.unit //LOG???
    }

    /**
      * adds an edge by reference (from --- key --> to)
      * @param edge
      * @tparam S
      * @tparam E
      * @return
      */
    final def +[S, E](edge: Edge[S, E]): Task[Edge[S, E]] = upsert(edge)

    /**
      * deletes an edge
      * @param edge
      */
    final def -(edge: Edge[_, _]): Task[Unit] = delete(edge)

    /**
      * adds an edge by reference (from --- key --> to) and meta-properties (edge.outE())
      * @param edge
      * @tparam S
      * @tparam E
      * @return
      */
    final def ++[S, E](edge: Edge[S, E]): Task[Edge[S, E]] = post(edge)
  }

  /**
    * Edges A.K.A. Links A.K.A. Properties
    * @return
    */
//  def edges: Stream[Edge[_, _]] = edgeStore.toStream()
  private lazy val _edges: Edges = new Edges {}
  def edges: Edges               = _edges

  trait Nodes extends RApi[Node] {
    def apply(): Observable[Node] = nodeStore.all()
    def count(): Task[Long]       = nodeStore.count()

    def hasId(id: Long): Task[Option[Node]] = nodeStore.hasId(id)
    def cached = new {
      def hasId(id: Long): Option[Node] =
        nodeStore.cached.hasId(id)
      def dereferenceValue(t: Any): Any = t
    }
    def hasId(id: List[Long]): Observable[Node] = nodeStore.hasId(id)
    override def hasIri(iris: List[String]): Observable[Node] = {
      //    println(s"get nodes $iris")
      val validIris = iris.distinct.filter(_.nonEmpty)
      if (validIris.nonEmpty)
        Observable.fromIterable(validIris).flatMap { iri =>
//          println(s"get $iri")
          nodeStore
            .hasIri(iri)
            .asInstanceOf[Observable[Node]] //        println(r.map(_.iri))
        } else Observable[Node]()
    }

    def create(ontology: Ontology*): Task[Node] = {
      for {
        id <- idProvider.next
        node = newNode(id)
        u <- Task.gatherUnordered(ontology.map(node.addLabel))
      } yield node
    }

    def upsert(iri: String, ontologies: Ontology*): Task[Node] = {
      for {
        node <- upsert(iri, Set[String]())
        u    <- Task.gatherUnordered(ontologies.toList map node.addLabel)
      } yield node
    }

    /**
      *
      * @param iri an iri which should all resolve to the same resource as param uris
      * @param iris a set of iri's which should all resolve to the same resource
      * @return all vertices which identify by the uri's, expected to return (in time) only a single vertex due to eventual consistency
      */
    def upsert(iri: String, iris: Set[String]): Task[Node] = {
      hasIri(iri :: iris.toList).toListL
        .flatMap {
          case Nil =>
            for {
              node <- create()
              iriEdge <- if (iri.nonEmpty) node.addOut(default.typed.iriUrlString, iri)
              else if (iris.headOption.exists(_.nonEmpty))
                node.addOut(default.typed.iriUrlString, iris.head)
              else Task.unit
            } yield {
              node
            }
          case List(node) => Task.now(node)
          case nodes =>
            mergeNodes(nodes.toSet)
        }
        .flatMap { node =>
          node.out(lspace.Label.P.`@id`, lspace.Label.P.`@ids`)
          val newIris = ((iris + iri) diff node.iris).toList.filter(_.nonEmpty)
          for {
            iriEdges <- Task.sequence(newIris.map(node.addOut(default.`@ids`, _)))
          } yield node
        }
    }

    def upsert(node: Node): Task[Node] = {
      if (node.graph != thisgraph) { //
        for {
          edges <- node.g.outE().withGraph(node.graph).toListF
          newNode <- if (node.iri.nonEmpty) upsert(node.iri)
          else {
            for {
              newNode <- create()
              u       <- Task.gather(node.labels.map(newNode.addLabel))
              v       <- addMeta(node, newNode)
            } yield newNode
          }
        } yield newNode
      } else {
        Task.now(node)
      }
    }

    /**
      * adds a node to the graph including all edges and (meta) edges on edges as long as edges have edges
      * @param node
      * @tparam T
      * @return
      */
    def post(node: Node): Task[Node] = node match {
      case node: _Node => //match on GNode does also accept _Node instances from other Graphs???? Why?
        Task.now(node)
      case _ =>
        for {
          newNode <- if (node.iri.nonEmpty) upsert(node.iri, node.iris) else create()
          u       <- Task.gather(node.labels.map(node.addLabel))
          v       <- addMeta(node, newNode)
        } yield node
    }

    final def delete(node: Node): Task[Unit] = node match {
      case node: _Node => deleteNode(node.asInstanceOf[GNode])
      case _           => Task.unit //LOG???
    }

    final def +(label: Ontology): Task[Node] = create(label)

    /**
      * adds a node by reference (iri(s))
      * @param node
      * @return
      */
    final def +(node: Node): Task[Node] = upsert(node)

    /**
      * deletes a node
      * @param node
      */
    final def -(node: Node): Task[Unit] = delete(node)

    /**
      * adds a node by every detail
      * @param node
      * @return
      */
    final def ++(node: Node): Task[Node] = post(node)
  }

  /**
    * Nodes A.K.A. Vertices
    * @return
    */
//  def nodes: Stream[Node] = nodeStore.toStream()
  private lazy val _nodes: Nodes = new Nodes {}
  def nodes: Nodes               = _nodes

  trait Values extends RApi[Value[_]] {
    def apply(): Observable[Value[_]] = valueStore.all()
    def count(): Task[Long]           = valueStore.count()

    def hasId(id: Long): Task[Option[Value[_]]] = valueStore.hasId(id)
    def cached = new {
      def hasId(id: Long): Option[Value[_]] =
        valueStore.cached.hasId(id)
      def dereferenceValue(t: Any): Any = t
    }
    def hasIri(iris: List[String]): Observable[Value[_]] = {
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
    def byValue[T](valueSet: List[(T, DataType[T])]): Observable[Value[T]] = {
//      val valueList = valueSet.distinct.filter(_ != null)
//      if (valueList.nonEmpty) values().filter(v => valueSet.map(_._2).contains(v.value)).toList
//      //      or(
//      //      _.has(this.id, p.Within(iriList)),
//      //      _.has(this.ids, p.Within(iriList))).toSet
//      else List[Value[_]]()

      Observable.fromIterable(valueSet).flatMap { value =>
        valueStore.byValue(value._1, value._2).asInstanceOf[Observable[Value[T]]]
      } //distinct
    }

    def dereferenceValue(t: Any): Task[Any] = t match {
      case v: Vector[_] =>
        Task.gather[Any, Vector](v.map(dereferenceValue)) //without type parameters it cannot be inferred
      case v: ListSet[_] => Task.gather[Any, ListSet](v.map(dereferenceValue))
      case v: List[_]    => Task.gather[Any, List](v.map(dereferenceValue))
      case v: Set[_]     => Task.gather[Any, Set](v.map(dereferenceValue))
      case v: Map[_, _] =>
        Task
          .gather(v.toList.map {
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
      case _              => Task.now(t)
    }

    final def create[T, TOut, CTOut <: ClassType[_]](value: T)(
        implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut]): Task[Value[T]] = { //add implicit DataType[T]
      byValue(value).headOptionL.flatMap(_.map(_.asInstanceOf[GValue[T]]).map(Task.now).getOrElse {
        for {
          dereferencedValue <- dereferenceValue(value).map(_.asInstanceOf[T])
          b <- byValue(dereferencedValue)(clsTpbl).headOptionL
            .flatMap(_.map(_.asInstanceOf[GValue[T]]).map(Task.now).getOrElse {
              for {
                id    <- idProvider.next
                value <- createValue(id, dereferencedValue, ClassType.valueToOntologyResource(dereferencedValue))
              } yield value
            })
        } yield b
      })
    }
    final def create[T](value: T, dt: DataType[T]): Task[Value[T]] = { //add implicit DataType[T]
      for {
        dereferencedValue <- dereferenceValue(value).map(_.asInstanceOf[T])
        b <- byValue(dereferencedValue -> dt :: Nil).headOptionL
          .flatMap(_.map(_.asInstanceOf[GValue[T]]).map(Task.now).getOrElse {
            for {
              id    <- idProvider.next
              value <- createValue(id, dereferencedValue, dt)
            } yield value
          })
      } yield { b }
    }

    def upsert[V, TOut, CTOut <: ClassType[_]](value: V)(
        implicit clsTpbl: ClassTypeable.Aux[V, TOut, CTOut]): Task[Value[V]] = {
      byValue(value).toListL.flatMap {
        case Nil         => create(value)
        case List(value) => Task.now(value)
        case values =>
          mergeValues(values.toSet)
      }
    }

    final def upsert[V](value: V, dt: DataType[V]): Task[Value[V]] = {
//      val values = byValue(List(value -> dt))
//      values.headOptionL.flatMap(_.map(Task.now).getOrElse(create(value, dt)))
      byValue(List(value -> dt)).toListL.flatMap {
        case Nil         => create(value, dt)
        case List(value) => Task.now(value)
        case values =>
          mergeValues(values.toSet)
      }
//      val _value: Value[V] = if (values.isEmpty) {
//        create(value, dt)
////      } else if (values.size > 1) {
////        GraphUtils.mergeValues(values.toSet)
//      } else values.head
//      _value
    }
    final def upsert[V](value: Value[V]): Task[Value[V]] = {
      if (value.graph != this) {
        upsert(value.value, value.label)
      } else Task.now(value)
    }

    /**
      * adds a value to the graph including all edges and (meta) edges on edges as long as edges have edges
      * @param value
      * @tparam V
      * @tparam T
      * @return
      */
    def post[V](value: Value[V]): Task[Value[V]] = {
      if (value.graph != this) {
        hasIri(value.iri).toListL
          .flatMap {
            case List() => create(value.value, value.label)
            case List(storedValue: Value[_]) if storedValue.value == value.value =>
              Task.now(storedValue.asInstanceOf[Value[V]])
            case List(value: Value[_], _*) =>
              Task.raiseError(new Exception("multiple values with the same iri, what should we do?! Dedup?"))
          }
          .flatMap { newValue =>
            for { u <- addMeta(value, newValue) } yield newValue
          }
      } else Task.now(value)
    }

    final def delete(value: Value[_]): Task[Unit] = value match {
      case value: _Value[_] => deleteValue(value.asInstanceOf[GValue[_]])
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
    final def -(value: Value[_]): Task[Unit] = delete(value)

    /**
      * adds a value and meta-properties (value.outE())
      * @param value
      * @tparam V
      * @return
      */
    final def ++[V](value: Value[V]): Task[Value[V]] = post(value)
  }

  private lazy val _values: Values = new Values {}
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
  protected def storeNode(node: GNode): Task[Unit] = nodeStore.store(node)

  protected def newEdge[S, E](id: Long, from: GResource[S], key: Property, to: GResource[E]): GEdge[S, E]
//  protected def newEdge(id: Long, from: Long, key: Property, to: Long): GEdge[Any, Any]
//  protected def createEdge(id: Long, from: Long, key: Property, to: Long): Task[GEdge[Any, Any]] = {
////    if (ns.properties.get(key.iri).isEmpty) ns.properties.store(key)
//    if (Property.properties.default.byIri.get(key.iri).isEmpty)
//      ns.properties.store(key).runToFuture(monix.execution.Scheduler.global)
//
//    val _from = resources
//      .hasId(from)
//      .map(
//        _.map(_.asInstanceOf[GResource[Any]])
//          .getOrElse {
//            throw new Exception(s"cannot create edge, from-resource with id ${from} not found")
//          })
//    val _to =
//      resources
//        .hasId(to)
//        .map(
//          _.map(_.asInstanceOf[GResource[Any]])
//            .getOrElse {
//              throw new Exception(s"cannot create edge, to-resource with id ${to} not found")
//            })
////    val edge = createEdge(idProvider.next, _from, key, _to)
////    storeEdge(edge.asInstanceOf[GEdge[_, _]])
////    edge
//    for {
//      from <- _from
//      to   <- _to
//      id   <- idProvider.next
//      edge <- createEdge(id, from, key, to)
//      u    <- storeEdge(edge.asInstanceOf[GEdge[_, _]]).forkAndForget //what if this fails?
//    } yield edge
//  }
  protected def createEdge[S, E](id: Long, from: GResource[S], key: Property, to: GResource[E]): Task[GEdge[S, E]] = {
//    if (ns.properties.get(key.iri).isEmpty) ns.properties.store(key)
    if (Property.properties.default.byIri.get(key.iri).isEmpty)
      ns.properties.store(key).runToFuture(lspace.Implicits.Scheduler.global)

    for {
      edge <- Task.now(newEdge(id, from, key, to))
      u    <- storeEdge(edge.asInstanceOf[GEdge[_, _]]) //.forkAndForget //what if this fails?
    } yield edge
  }

  protected def storeEdge(edge: GEdge[_, _]): Task[Unit] = edgeStore.store(edge)

  protected def newValue[T](id: Long, value: T, label: DataType[T]): GValue[T]
  protected def createValue[T](id: Long, value: T, dt: DataType[T]): Task[GValue[T]] = {
//    if (ns.datatypes.get(dt.iri).isEmpty) ns.datatypes.store(dt)
//    ns.datatypes.store(dt).runToFuture(monix.execution.Scheduler.global)
    for {
      _value <- Task.now(newValue(id, value, dt))
      u      <- storeValue(_value.asInstanceOf[GValue[_]])
    } yield _value
  }

  protected def storeValue(value: GValue[_]): Task[Unit] = valueStore.store(value)

  /**
    * deletes the Node from the graph
    * @param value
    */
  protected def deleteNode(node: GNode): Task[Unit] = {
    for {
      _ <- deleteResource(node)
      _ <- nodeStore.delete(node)
    } yield ()
  }

  /**
    * deletes the Edge from the graph
    * @param value
    */
  protected def deleteEdge(edge: GEdge[_, _]): Task[Unit] = {
    for {
      _ <- deleteResource(edge)
      _ <- edgeStore.delete(edge)
    } yield ()
  }

  /**
    * deletes the Value from the graph
    * @param value
    */
  protected def deleteValue(value: GValue[_]): Task[Unit] = {
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

  private def addMeta[S <: Resource[_], T <: Resource[_]](source: S, target: T): Task[Unit] =
    Observable
      .fromIterable(source.outE().filterNot(p => Graph.baseKeys.contains(p.key)).map { edge =>
        for {
          t <- edges.create[Any, Any](target, edge.key, edge.to)
          u <- addMeta(edge, t)
        } yield u
      })
      .completedL

  def add: Graph => Task[Graph] = ++
  val ++ : Graph => Task[Graph] = (graph: Graph) => {
    if (graph != this) {
      for {
        oldIdNewNodeMap <- graph
          .nodes()
          .mapEval { node =>
            nodes.create(node.labels: _*).map(node.id -> _)
//            (if (node.iri.nonEmpty) nodes.upsert(node.iri, node.labels: _*)
//             else nodes.create(node.labels: _*)).map(node.id -> _)
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
//              .filter(e => !(e.key == Property.default.`@id` && e.to.hasLabel(TextType.datatype).isDefined))
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
  def *>[ST <: ClassType[_],
         ET <: ClassType[_],
         Segments <: HList,
         Steps <: HList,
         RSteps <: HList,
         Containers <: HList,
         F[_],
         Out,
         CT <: ClassType[Out],
         Out2](traversal: Traversal[ST, ET, Segments])(
      implicit flat: shapeless.ops.hlist.FlatMapper.Aux[Traversal.SegmentMapper.type, Segments, Steps],
      reverse: Reverse.Aux[Steps, RSteps],
      f: Collect.Aux[RSteps, ContainerSteps.type, Containers],
      lf: StructureCalculator.Aux[Containers, ET, Out, CT],
      tw: OutTweaker.Aux[ET, Out, Containers, Out2],
      guide: Guide[F],
      mapper: Mapper[F, Containers, Out]): mapper.FT =
    mapper.apply(traversal.segmentList, this).asInstanceOf[mapper.FT]

  def __[Start, End](implicit cltblStart: ClassTypeable[Start],
                     cltblEnd: ClassTypeable[End]): Traversal[cltblStart.CT, cltblEnd.CT, HNil] =
    Traversal[cltblStart.CT, cltblEnd.CT](cltblStart.ct, cltblEnd.ct)

  @deprecated("instead import lspace.g")
  def g(): Traversal[ClassType[Any], ClassType[Any], HNil] =
    Traversal[ClassType[Any], ClassType[Any]](ClassType.stubAny, ClassType.stubAny)

  @deprecated("instead import lspace.g")
  lazy val traversal: Traversal[ClassType[Any], ClassType[Any], HNil] = g

//  def buildTraversersStream[ST <: ClassType[_], ET <: ClassType[_], Segments <: HList, Out](
//      traversal: Traversal[ST, ET, Segments]): Stream[Out]
//  def buildAsyncTraversersStream[ST <: ClassType[_], ET <: ClassType[_], Segments <: HList, Out](
//      traversal: Traversal[ST, ET, Segments]): Task[Stream[Out]]

  def persist: CancelableFuture[Unit] = CancelableFuture.unit

  def close(): CancelableFuture[Unit] = CancelableFuture.unit

  override def toString: String = s"graph:$iri"
}
