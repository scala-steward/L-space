package lspace.librarian.structure

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}

import lspace.librarian.structure
import monix.eval.Task
import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.Property.default
import lspace.NS
import lspace.librarian.datatype.GraphType
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.transaction.Transaction
import lspace.librarian.provider.wrapped.WrappedResource
import lspace.librarian.structure.index.Index
import lspace.librarian.structure.store.{EdgeStore, NodeStore, Store, ValueStore}
import lspace.librarian.structure.util.{GraphUtils, IdProvider}
import shapeless.{::, HList, HNil}

import scala.collection.mutable
import scala.util.control.NonFatal

object Graph {
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

trait Graph extends IriResource {
  abstract class _Resource[+T] extends structure.Resource[T]
  abstract class _Node         extends _Resource[structure.Node] with structure.Node
  abstract class _Edge[+S, +E] extends _Resource[structure.Edge[S, E]] with structure.Edge[S, E]
  abstract class _Value[T]     extends _Resource[T] with structure.Value[T]

  implicit lazy val thisgraph: this.type = this
  def ns: NameSpaceGraph

  def idProvider: IdProvider

  /**
    * creates new transaction
    * @return
    */
  def transaction: Transaction

  protected def nodeStore: NodeStore[this.type]
  protected def edgeStore: EdgeStore[this.type]
  protected def valueStore: ValueStore[this.type]
//  protected def `@idStore`: ValueStore[this.type]
//  protected def `@typeStore`: ValueStore[String, _Value[String]]

  protected def `@idIndex`: Index
  protected def `@typeIndex`: Index

  def init(): Unit

  sealed trait RApi[T <: Resource[_]] {

    def apply(): Stream[T]

    def hasIri(iri: String, iris: String*): List[T] = hasIri(iri :: iris.toList)

    /**
      *
      * @param iris a set of uri's to get T for
      * @return
      */
    def hasIri(iris: List[String]): List[T]

    def hasId(id: Long): Option[T]

    def count(): Long = apply().size

  }
  trait Resources extends RApi[Resource[_]] {
    def apply(): Stream[Resource[_]] = nodes() ++ edges() ++ values()
    def hasIri(iris: List[String]): List[Resource[_]] = {
      val validIris = iris.filter(_.nonEmpty)
      if (validIris.nonEmpty) {
        validIris
          .flatMap(
            iri =>
              nodeStore
                .byIri(iri)
                .toList
                .asInstanceOf[List[Resource[_]]] ++ edgeStore.byIri(iri).toList.asInstanceOf[List[Resource[_]]])
          .asInstanceOf[List[Resource[_]]]
      } else List[Resource[_]]()
    }

    def upsert[V](value: V): Resource[V] = {
      value match {
        case resource: Resource[_] => upsertR(resource).asInstanceOf[Resource[V]]
        case value                 => values.create(value).asInstanceOf[Resource[V]]
      }
    }
    private def upsertR[V](value: Resource[V]): Resource[V] = {
      value match {
        case resource: _Resource[V]       => resource
        case resource: WrappedResource[V] => upsertR(resource.self)
        case resource: Resource[V] =>
          resource match {
            case node: Node =>
              nodes.upsert(node).asInstanceOf[Resource[V]]
            case edge: Edge[_, _] =>
              edges.upsert(edge).asInstanceOf[Resource[V]]
            case value: Value[V] =>
              values.upsert(value).asInstanceOf[Resource[V]]
            case _ =>
              println(s"${value.getClass.toString}")
              println(s"${value.asInstanceOf[Edge[_, _]].labels.map(_.iri).mkString(" and ")}")
              throw new Exception("???")
          }
      }
    }

    def hasId(id: Long): Option[Resource[_]] = nodes.hasId(id).orElse(edges.hasId(id)).orElse(values.hasId(id))
  }

  def resources: Resources = new Resources {}

  trait Edges extends RApi[Edge[_, _]] {
    def apply(): Stream[Edge[_, _]] = edgeStore.all()

    def hasId(id: Long): Option[Edge[_, _]] = edgeStore.byId(id)
    override def hasIri(iris: List[String]): List[Edge[_, _]] = {
      val validIris = iris.filter(_.nonEmpty)
      if (validIris.nonEmpty)
        validIris
          .flatMap(
            iri =>
              edgeStore
                .byIri(iri)
                .toList
                .asInstanceOf[List[Edge[_, _]]])
      else List[Edge[_, _]]()
    }

    def create[S, E](id: Long)(from: _Resource[S], key: Property, to: _Resource[E]): _Edge[S, E] =
      _createEdge(id)(from, key, to)
    final def create(id: Long, fromId: Long, key: Property, toId: Long): _Edge[Any, Any] = {
      val _from = resources
        .hasId(fromId)
        .map(_.asInstanceOf[_Resource[Any]])
        .getOrElse(throw new Exception(s"cannot create edge, from-resource with id ${fromId} not found"))
      val _to =
        resources
          .hasId(toId)
          .map(_.asInstanceOf[_Resource[Any]])
          .getOrElse(throw new Exception(s"cannot create edge, to-resource with id ${toId} not found"))
      _createEdge(id)(_from, key, _to)
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
    final def create[S, E](from: Resource[S], key: Property, to: Resource[E]): Edge[S, E] = {
      val _from = from match {
        case from: _Resource[S] => from
        case _ =>
          resources
            .hasIri(from.iri)
            .headOption
            .getOrElse(resources.upsert(from))
            .asInstanceOf[_Resource[S]]
      }
      val _to = to match {
        case to: _Resource[E] => to
        case _ =>
          resources
            .hasIri(to.iri)
            .headOption
            .getOrElse(resources.upsert(to))
            .asInstanceOf[_Resource[E]]
      }
      _createEdge(idProvider.next)(_from, key, _to)
    }

    def upsert[S, E](edge: Edge[S, E]): Edge[S, E] = {
      if (edge.graph != this) {
        val newEdge = resources.upsert(edge.from).addOut(edge.key, resources.upsert(edge.to))
        newEdge
      } else edge
    }.asInstanceOf[Edge[S, E]]

    /**
      * adds an edge and upserts the 'from' and 'to' resource and adds (meta) edges on edges as long as edges have edges
      * @param edge
      * @tparam S
      * @tparam E
      * @tparam T
      * @return
      */
    def post[S, E](edge: Edge[S, E]): Edge[S, E] = {
      if (edge.graph != this) {
        val newEdge = resources.upsert(edge.from).addOut(edge.key, resources.upsert(edge.to))
        addMeta(edge, newEdge)
        newEdge
      } else edge
    }.asInstanceOf[Edge[S, E]]

    final def delete(edge: Edge[_, _]): Unit = edge match {
      case edge: _Edge[_, _] => _deleteEdge(edge)
      case _                 => //LOG???
    }

    /**
      * adds an edge by reference (from --- key --> to)
      * @param edge
      * @tparam S
      * @tparam E
      * @return
      */
    final def +[S, E](edge: Edge[S, E]): Edge[S, E] = upsert(edge)

    /**
      * deletes an edge
      * @param edge
      */
    final def -(edge: Edge[_, _]): Unit = delete(edge)

    /**
      * adds an edge by reference (from --- key --> to) and meta-properties (edge.outE())
      * @param edge
      * @tparam S
      * @tparam E
      * @return
      */
    final def ++[S, E](edge: Edge[S, E]): Edge[S, E] = post(edge)
  }

  /**
    * Edges A.K.A. Links A.K.A. Properties
    * @return
    */
//  def edges: Stream[Edge[_, _]] = edgeStore.toStream()
  def edges: Edges = new Edges {}

  trait Nodes extends RApi[Node] {
    def apply(): Stream[Node] = nodeStore.all()

    def hasId(id: Long): Option[Node] = nodeStore.byId(id)
    override def hasIri(iris: List[String]): List[Node] = {
      //    println(s"get nodes $iris")
      val validIris = iris.distinct.filter(_.nonEmpty)
      if (validIris.nonEmpty)
        validIris.flatMap { iri =>
          //        println(s"get $iri")
          nodeStore
            .byIri(iri)
            .toList
            .asInstanceOf[List[Node]] //        println(r.map(_.iri))
        } else List[Node]()
    }

    final def create(ontology: Ontology*): Node           = _createNode(idProvider.next)(ontology: _*)
    final def create(id: Long)(ontology: Ontology*): Node = _createNode(id)(ontology: _*)

    /**
      *
      * @param iri an iri which should all resolve to the same resource as param uris
      * @param iris a set of iri's which should all resolve to the same resource
      * @return all vertices which identify by the uri's, expected to return (in time) only a single vertex due to eventual consistency
      */
    def upsert(iri: String, iris: Set[String] = Set()): Node = {
      val nodes = hasIri(iris + iri toList)
      val node: Node = if (nodes.isEmpty) {
//        println(s"create node ${iri} ${iris}")
        val node = create()
        if (iri.nonEmpty) node.addOut(default.typed.iriUrlString, iri)
        else if (iris.headOption.exists(_.nonEmpty))
          node.addOut(default.typed.iriUrlString, iris.head)
        //      node.addOut(default.typed.createdonDateTime, Instant.now())
//        println("added iri")
        node
      } else if (nodes.size > 1) {
        GraphUtils.mergeNodes(nodes.toSet)
      } else nodes.head
      val newIris = ((iris + iri) diff node.iris).toList.filter(_.nonEmpty)
      if (newIris.nonEmpty) newIris.foreach(node.addOut(default.`@ids`, _))
      node
    }

    def upsert(node: Node): Node = {
      if (node.graph != this) {
        if (node.iri.nonEmpty) upsert(node.iri)
        else {
          val newNode = create()
          node.labels.foreach(newNode.addLabel)
          addMeta(node, newNode)
          newNode
        }
      } else node
    }

    /**
      * adds a node to the graph including all edges and (meta) edges on edges as long as edges have edges
      * @param node
      * @tparam T
      * @return
      */
    def post(node: Node): Node = node match {
      case node: _Node => node
      case _ =>
        val newNode =
          if (node.iri.nonEmpty) upsert(node.iri, node.iris) else create()
        node.labels.foreach(newNode.addLabel)
        addMeta(node, newNode)
        newNode
    }

    final def delete(node: Node): Unit = node match {
      case node: _Node => _deleteNode(node)
      case _           => //LOG???
    }

    final def +(label: Ontology): Node = create(label)

    /**
      * adds a node by reference (iri(s))
      * @param node
      * @return
      */
    final def +(node: Node): Node = upsert(node)

    /**
      * deletes a node
      * @param node
      */
    final def -(node: Node): Unit = delete(node)

    /**
      * adds a node by every detail
      * @param node
      * @return
      */
    final def ++(node: Node): Node = post(node)
  }

  /**
    * Nodes A.K.A. Vertices
    * @return
    */
//  def nodes: Stream[Node] = nodeStore.toStream()
  def nodes: Nodes = new Nodes {}

  trait Values extends RApi[Value[_]] {
    def apply(): Stream[Value[_]] = valueStore.all()

    def hasId(id: Long): Option[Value[_]] = valueStore.byId(id)
    def hasIri(iris: List[String]): List[Value[_]] = {
      //    println(s"get nodes $iris")
      val validIris = iris.distinct.filter(_.nonEmpty)
      if (validIris.nonEmpty)
        validIris.flatMap { iri =>
          //        println(s"get $iri")
          valueStore
            .byIri(iri)
            .toList
            .asInstanceOf[List[Value[_]]] //        println(r.map(_.iri))
        } else List[Value[_]]()
    }

    def byValue[T](value: T): List[Value[_]] =
      byValue(List(value))
    def byValue(valueSet: List[Any]): List[Value[_]] = {
//      val valueList = valueSet.distinct.filter(_ != null)
//      if (valueList.nonEmpty) values().filter(v => valueSet.map(_._2).contains(v.value)).toList
//      //      or(
//      //      _.has(this.id, p.Within(iriList)),
//      //      _.has(this.ids, p.Within(iriList))).toSet
//      else List[Value[_]]()

      val l = valueSet
        .flatMap { value =>
          valueStore.byValue(value).toList.asInstanceOf[List[Value[_]]]
        }
//        .asInstanceOf[List[Value[_]]]
      l
    }

    def create[T](id: Long)(value: T)(dt: DataType[T]): _Value[T] = _createValue(id)(value)(dt)
    final def create[T](value: T): Value[T] = { //add implicit DataType[T]
      byValue(value).headOption.map(_.asInstanceOf[_Value[T]]).getOrElse {
        _createValue(idProvider.next)(value)(ClassType.valueToOntologyResource(value))
      }
    }
    final def create[T](value: T, dt: DataType[T]): Value[T] = { //add implicit DataType[T]
      byValue(value).headOption.map(_.asInstanceOf[_Value[T]]).getOrElse {
        _createValue(idProvider.next)(value)(dt)
      }
    }

    final def upsert[V](value: Value[V]): Value[V] = {
      if (value.graph != this) {
        hasIri(value.iri) match {
          case List() => create(value.value, value.label)
          case List(storedValue: Value[_]) if storedValue.value == value.value =>
            storedValue.asInstanceOf[Value[V]]
          case List(value: Value[_], _*) =>
            throw new Exception("multiple values with the same iri, what should we do?! Dedup?")
        }
      } else value
    }

    /**
      * adds a value to the graph including all edges and (meta) edges on edges as long as edges have edges
      * @param value
      * @tparam V
      * @tparam T
      * @return
      */
    def post[V](value: Value[V]): Value[V] = {
      if (value.graph != this) {
        val newValue = hasIri(value.iri) match {
          case List() => create(value.value, value.label)
          case List(storedValue: Value[_]) if storedValue.value == value.value =>
            storedValue.asInstanceOf[Value[V]]
          case List(value: Value[_], _*) =>
            throw new Exception("multiple values with the same iri, what should we do?! Dedup?")
        }
        addMeta(value, newValue)
        newValue
      } else value
    }

    final def delete(value: Value[_]): Unit = value match {
      case value: _Value[_] => _deleteValue(value)
      case _                => //LOG???
    }

    /**
      * adds a value
      * @param value
      * @tparam V
      * @return
      */
    final def +[V](value: Value[V]): Value[V] = upsert(value)

    /**
      * deletes a value
      * @param value
      */
    final def -(value: Value[_]): Unit = delete(value)

    /**
      * adds a value and meta-properties (value.outE())
      * @param value
      * @tparam V
      * @return
      */
    final def ++[V](value: Value[V]): Value[V] = post(value)
  }

  def values: Values = new Values {}

  protected def _createNode(id: Long)(ontology: Ontology*): _Node
  final def +(label: Ontology): Node          = nodes.create(label)
  protected def _storeNode(node: _Node): Unit = nodeStore.store(node)

  protected def _createEdge[S, E](id: Long)(from: _Resource[S], key: Property, to: _Resource[E]): _Edge[S, E]

  protected def _storeEdge(edge: _Edge[_, _]): Unit = edgeStore.store(edge)

  protected def _createValue[T](id: Long)(value: T)(dt: DataType[T]): _Value[T]

  protected def _storeValue(value: _Value[_]): Unit = valueStore.store(value)

  protected def _deleteNode(node: _Node): Unit = {
    _deleteResource(node)
    nodeStore.delete(node.id)
  }

  protected def _deleteEdge(edge: _Edge[_, _]): Unit = {
    _deleteResource(edge)
    edgeStore.delete(edge.id)
  }

  protected def _deleteValue(value: _Value[_]): Unit = {
    _deleteResource(value)
    valueStore.delete(value.id)
  }

  protected def _deleteResource(resource: _Resource[_]): Unit

  private def addMeta[S <: Resource[_], T, RT[Z] <: Resource[Z]](source: S, target: RT[T]): Unit =
    source.outE().filterNot(p => Graph.baseKeys.contains(p.key)).foreach { edge =>
      addMeta(edge, edges.create(target, edge.key, edge.to))
    }

  def ++(graph: Graph): Unit = {
    if (graph != this) {
      graph.nodes().foreach { node =>
        nodes.post(node)
      }
//      graph.edges().foreach { edge =>
//        postEdge(edge)
//      }
//      graph.values().foreach { value =>
//        postValue(value)
//      }
    }
  }

  def g: Traversal[DataType[Graph], DataType[Graph], HNil] = g()
  def g(graph: Graph*): Traversal[DataType[Graph], DataType[Graph], HNil] =
    Traversal[DataType[Graph], DataType[Graph]]()(this, GraphType.default, GraphType.default)

  lazy val traversal: Traversal[DataType[Graph], DataType[Graph], HNil] = g

  def buildTraversersStream[ST <: ClassType[_], ET <: ClassType[_], Steps <: HList, Out](
      traversal: Traversal[ST, ET, Steps])(ct: ClassType[_]): Stream[Out]
  def buildAsyncTraversersStream[ST <: ClassType[_], ET <: ClassType[_], Steps <: HList, Out](
      traversal: Traversal[ST, ET, Steps])(ct: ClassType[_]): Task[Stream[Out]]

  def close(): Unit = {}

  override def toString: String = s"graph:$iri"
}
