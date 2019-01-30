package lspace.librarian.structure

import lspace.librarian.structure
import monix.eval.Task
import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.Property.default
import lspace.librarian.datatype.{DataType, GraphType}
import lspace.librarian.provider.transaction.Transaction
import lspace.librarian.provider.wrapped.WrappedResource
import lspace.librarian.structure.store.{EdgeStore, NodeStore, ValueStore}
import lspace.librarian.structure.util.{GraphUtils, IdProvider}
import monix.execution.{Cancelable, CancelableFuture}
import shapeless.{::, HList, HNil}

import scala.collection.immutable.ListSet
import scala.collection.mutable

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
  trait _Resource[+T]        extends structure.Resource[T]
  abstract class _Node       extends _Resource[structure.Node] with structure.Node
  abstract class _Edge[S, E] extends _Resource[structure.Edge[S, E]] with structure.Edge[S, E]
  abstract class _Value[T]   extends _Resource[T] with structure.Value[T]

  type GResource[T] <: _Resource[T]
  type GNode <: _Node             // with GResource[Node]
  type GEdge[S, E] <: _Edge[S, E] //with GResource[Edge[S, E]]
  type GValue[T] <: _Value[T]     // with GResource[T]

  override lazy val hashCode: Int = iri.hashCode

  lazy val thisgraph: this.type = this
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

  }
  trait Resources extends RApi[Resource[_]] {
    def apply(): Stream[Resource[_]] = nodes() ++ edges() ++ values()
    def count(): Long                = nodeStore.count() + edgeStore.count() + valueStore.count()

    def hasIri(iris: List[String]): List[Resource[_]] = {
      val validIris = iris.filter(_.nonEmpty)
      if (validIris.nonEmpty) {
        validIris
          .flatMap(
            iri =>
              nodeStore
                .hasIri(iri)
                .toList
                .asInstanceOf[List[Resource[_]]] ++ edgeStore.hasIri(iri).toList.asInstanceOf[List[Resource[_]]])
          .asInstanceOf[List[Resource[_]]]
      } else List[Resource[_]]()
    }

    def upsert[V](resource: Resource[V]): Resource[V] = {
      upsertR(resource)
//      value match {
//        case resource: Resource[_] => upsertR(resource).asInstanceOf[Resource[V]]
//        case value                 => values.create(value).asInstanceOf[Resource[V]]
//      }
    }
    private def upsertR[V](value: Resource[V]): Resource[V] = {
      value match {
//        case resource: _Resource[V]       => resource
        case resource: WrappedResource[V] => upsertR(resource.self)
        case resource: Resource[V] =>
          resource match {
            case node: _Node => node
            case node: Node =>
              nodes.upsert(node).asInstanceOf[Resource[V]]
            case edge: _Edge[_, _] => edge
            case edge: Edge[_, _] =>
              edges.upsert(edge).asInstanceOf[Resource[V]]
            case value: _Value[V] =>
              value
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

  private lazy val _resources: Resources = new Resources {}
  def resources: Resources               = _resources

  trait Edges extends RApi[Edge[_, _]] {
    def apply(): Stream[Edge[_, _]] = edgeStore.all()
    def count(): Long               = edgeStore.count()

    def hasId(id: Long): Option[Edge[_, _]] = edgeStore.hasId(id)
    override def hasIri(iris: List[String]): List[Edge[_, _]] = {
      val validIris = iris.filter(_.nonEmpty)
      if (validIris.nonEmpty)
        validIris
          .flatMap(
            iri =>
              edgeStore
                .hasIri(iri)
                .toList
                .asInstanceOf[List[Edge[_, _]]]) distinct
      else List[Edge[_, _]]()
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
        case from: _Node       => from.asInstanceOf[GResource[S]]
        case from: _Edge[_, _] => from.asInstanceOf[GResource[S]]
        case from: _Value[_]   => from.asInstanceOf[GResource[S]]
        case _ =>
          resources
            .hasIri(from.iri)
            .headOption
            .getOrElse(resources.upsert(from))
            .asInstanceOf[GResource[S]]
      }
      val _to = to match {
        case to: _Node       => to.asInstanceOf[GResource[E]]
        case to: _Edge[_, _] => to.asInstanceOf[GResource[E]]
        case to: _Value[_]   => to.asInstanceOf[GResource[E]]
        case _ =>
          resources
            .hasIri(to.iri)
            .headOption
            .getOrElse(resources.upsert(to))
            .asInstanceOf[GResource[E]]
      }
      createEdge(idProvider.next, _from, key, _to)
    }

    def upsert[S, E](edge: Edge[S, E]): Edge[S, E] = {
      if (edge.graph != this) {
        val newEdge = resources.upsert(edge.from).addOut(edge.key, resources.upsert(edge.to))
        newEdge
      } else edge
    }

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
    }

    final def delete(edge: Edge[_, _]): Unit = edge match {
      case edge: _Edge[_, _] => deleteEdge(edge.asInstanceOf[GEdge[_, _]])
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
  private lazy val _edges: Edges = new Edges {}
  def edges: Edges               = _edges

  trait Nodes extends RApi[Node] {
    def apply(): Stream[Node] = nodeStore.all()
    def count(): Long         = nodeStore.count()

    def hasId(id: Long): Option[Node]       = nodeStore.hasId(id)
    def hasId(id: List[Long]): Stream[Node] = nodeStore.hasId(id)
    override def hasIri(iris: List[String]): List[Node] = {
      //    println(s"get nodes $iris")
      val validIris = iris.distinct.filter(_.nonEmpty)
      if (validIris.nonEmpty)
        validIris.flatMap { iri =>
          //        println(s"get $iri")
          nodeStore
            .hasIri(iri)
            .toList
            .asInstanceOf[List[Node]] //        println(r.map(_.iri))
        } distinct
      else List[Node]()

    }

    def create(ontology: Ontology*): Node = {
      val node = getOrCreateNode(idProvider.next)
      ontology.foreach(node.addLabel)
      node
    }

    def upsert(iri: String, ontology: Ontology, ontologies: Ontology*): Node = {
      val node = upsert(iri)
      ontology :: ontologies.toList foreach node.addLabel
      node
    }

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
        GraphUtils.mergeNodes(nodes.toSet).runAsyncAndForget(monix.execution.Scheduler.global)
        nodes.minBy(_.id)
      } else {
//        println(s"found existing $iri")
//        try {
//          throw new Exception(iri)
//        } catch {
//          case t => t.printStackTrace()
//        }
        nodes.head
      }
      val newIris = ((iris + iri) diff node.iris).toList.filter(_.nonEmpty)
      if (newIris.nonEmpty) newIris.foreach(node.addOut(default.`@ids`, _))
      node
    }

    def upsert(node: Node): Node = {
      if (node.graph != this) { //
        val edges = node.outE()
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
      case node: _Node => //match on GNode does also accept _Node instances from other Graphs???? Why?
        node
      case _ =>
        val newNode =
          if (node.iri.nonEmpty) upsert(node.iri, node.iris) else create()
        node.labels.foreach(newNode.addLabel)
        addMeta(node, newNode)
        newNode
    }

    final def delete(node: Node): Unit = node match {
      case node: _Node => deleteNode(node.asInstanceOf[GNode])
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
  private lazy val _nodes: Nodes = new Nodes {}
  def nodes: Nodes               = _nodes

  trait Values extends RApi[Value[_]] {
    def apply(): Stream[Value[_]] = valueStore.all()
    def count(): Long             = valueStore.count()

    def hasId(id: Long): Option[Value[_]] = valueStore.hasId(id)
    def hasIri(iris: List[String]): List[Value[_]] = {
      //    println(s"get nodes $iris")
      val validIris = iris.distinct.filter(_.nonEmpty)
      if (validIris.nonEmpty)
        validIris.flatMap { iri =>
          //        println(s"get $iri")
          valueStore
            .hasIri(iri)
            .toList
            .asInstanceOf[List[Value[_]]] //        println(r.map(_.iri))
        } distinct
      else List[Value[_]]()
    }

    def byValue[T, TOut, CTOut <: ClassType[_]](value: T)(
        implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut]): List[Value[T]] =
      valueStore.byValue(value, clsTpbl.ct.asInstanceOf[DataType[T]]).toList.asInstanceOf[List[Value[T]]]
    def byValue[T](valueSet: List[(T, DataType[T])]): List[Value[T]] = {
//      val valueList = valueSet.distinct.filter(_ != null)
//      if (valueList.nonEmpty) values().filter(v => valueSet.map(_._2).contains(v.value)).toList
//      //      or(
//      //      _.has(this.id, p.Within(iriList)),
//      //      _.has(this.ids, p.Within(iriList))).toSet
//      else List[Value[_]]()

      valueSet.flatMap { value =>
        valueStore.byValue(value._1, value._2).toList.asInstanceOf[List[Value[T]]]
      } distinct
    }

    def dereferenceValue(t: Any): Any = t match {
      case v: Vector[_]     => v.map(dereferenceValue)
      case v: ListSet[_]    => v.map(dereferenceValue)
      case v: List[_]       => v.map(dereferenceValue)
      case v: Set[_]        => v.map(dereferenceValue)
      case v: Map[_, _]     => v.map { case (key, value) => (dereferenceValue(key), dereferenceValue(value)) }
      case (v1, v2)         => (dereferenceValue(v1), dereferenceValue(v2))
      case (v1, v2, v3)     => (dereferenceValue(v1), dereferenceValue(v2), dereferenceValue(v3))
      case (v1, v2, v3, v4) => (dereferenceValue(v1), dereferenceValue(v2), dereferenceValue(v3), dereferenceValue(v4))
      case (v1, v2, v3, v4, v5) =>
        (dereferenceValue(v1), dereferenceValue(v2), dereferenceValue(v3), dereferenceValue(v4), dereferenceValue(v5))
      case v: Ontology    => nodes.upsert(ns.ontologies.store(v))
      case v: Property    => nodes.upsert(ns.properties.store(v))
      case v: DataType[_] => nodes.upsert(ns.datatypes.store(v))
      case v: Node        => nodes.upsert(v)
      case v: Edge[_, _]  => edges.upsert(v)
      case v: Value[_]    => values.upsert(v)
      case _              => t
    }

    final def create[T, TOut, CTOut <: ClassType[_]](value: T)(
        implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut]): Value[T] = { //add implicit DataType[T]
      byValue(value).headOption.map(_.asInstanceOf[GValue[T]]).getOrElse {
        val dereferencedValue = dereferenceValue(value).asInstanceOf[T]
        byValue(dereferencedValue)(clsTpbl).headOption.map(_.asInstanceOf[GValue[T]]).getOrElse {
          createValue(idProvider.next, dereferencedValue, ClassType.valueToOntologyResource(dereferencedValue))
        }
      }
    }
    final def create[T](value: T, dt: DataType[T]): Value[T] = { //add implicit DataType[T]
      byValue(value -> dt :: Nil).headOption.map(_.asInstanceOf[GValue[T]]).getOrElse {
        createValue(idProvider.next, value, dt)
      }
    }

    def upsert[V, TOut, CTOut <: ClassType[_]](value: V)(
        implicit clsTpbl: ClassTypeable.Aux[V, TOut, CTOut]): Value[V] = {
      val values = byValue(value)

      val _value: Value[V] = if (values.isEmpty) {
        create(value)
      } else if (values.size > 1) {
        Task(GraphUtils.mergeValues(values.toSet)).runAsyncAndForget(monix.execution.Scheduler.global)
        values.minBy(_.id)
      } else values.head
      _value
    }

    final def upsert[V](value: V, dt: DataType[V]): Value[V] = {
      val values = byValue(List(value -> dt))
      val _value: Value[V] = if (values.isEmpty) {
        create(value, dt)
//      } else if (values.size > 1) {
//        GraphUtils.mergeValues(values.toSet)
      } else values.head
      _value
    }
    final def upsert[V](value: Value[V]): Value[V] = {
      if (value.graph != this) {
        upsert(value.value, value.label)
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
      case value: _Value[_] => deleteValue(value.asInstanceOf[GValue[_]])
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

  private lazy val _values: Values = new Values {}
  def values: Values               = _values

  protected def newNode(id: Long): GNode
  protected[lspace] def getOrCreateNode(id: Long): GNode = {
    nodeStore.hasId(id).getOrElse {
      val node = newNode(id)
      storeNode(node)
      node
    }
  }
  final def +(label: Ontology): Node         = nodes.create(label)
  protected def storeNode(node: GNode): Unit = nodeStore.store(node)

  protected def newEdge[S, E](id: Long, from: GResource[S], key: Property, to: GResource[E]): GEdge[S, E]
  protected def newEdge(id: Long, from: Long, key: Property, to: Long): GEdge[Any, Any]
  protected def createEdge(id: Long, from: Long, key: Property, to: Long): GEdge[Any, Any] = {
    if (ns.properties.get(key.iri).isEmpty) ns.properties.store(key)

    val _from = resources
      .hasId(from)
      .map(_.asInstanceOf[GResource[Any]])
      .getOrElse {
        throw new Exception(s"cannot create edge, from-resource with id ${from} not found")
      }
    val _to =
      resources
        .hasId(to)
        .map(_.asInstanceOf[GResource[Any]])
        .getOrElse {
          throw new Exception(s"cannot create edge, to-resource with id ${to} not found")
        }
    val edge = createEdge(idProvider.next, _from, key, _to)
    storeEdge(edge.asInstanceOf[GEdge[_, _]])
    edge
  }
  protected def createEdge[S, E](id: Long, from: GResource[S], key: Property, to: GResource[E]): GEdge[S, E] = {
    if (ns.properties.get(key.iri).isEmpty) ns.properties.store(key)

    val edge = newEdge(id, from, key, to)
    storeEdge(edge.asInstanceOf[GEdge[_, _]])
    edge
  }

  protected def storeEdge(edge: GEdge[_, _]): Unit = edgeStore.store(edge)

  protected def newValue[T](id: Long, value: T, label: DataType[T]): GValue[T]
  protected def createValue[T](id: Long, value: T, dt: DataType[T]): GValue[T] = {
    if (ns.datatypes.get(dt.iri).isEmpty) ns.datatypes.store(dt)
    val _value = newValue(id, value, dt)
    storeValue(_value.asInstanceOf[GValue[_]])
    _value
  }

  protected def storeValue(value: GValue[_]): Unit = valueStore.store(value)

  /**
    * deletes the Node from the graph
    * @param value
    */
  protected def deleteNode(node: GNode): Unit = {
    deleteResource(node)
    nodeStore.delete(node)
  }

  /**
    * deletes the Edge from the graph
    * @param value
    */
  protected def deleteEdge(edge: GEdge[_, _]): Unit = {
    deleteResource(edge)
    edgeStore.delete(edge)
  }

  /**
    * deletes the Value from the graph
    * @param value
    */
  protected def deleteValue(value: GValue[_]): Unit = {
    deleteResource(value)
    valueStore.delete(value)
  }

  /**
    * TODO: rename to _deleteProperties/_deleteEdges?
    * @param resource
    */
  protected def deleteResource[T <: _Resource[_]](resource: T): Unit

  private def addMeta[S <: Resource[_], T, RT[Z] <: Resource[Z]](source: S, target: RT[T]): Unit =
    source.outE().filterNot(p => Graph.baseKeys.contains(p.key)).foreach { edge =>
      addMeta(edge, edges.create(target, edge.key, edge.to))
    }

  def add(graph: Graph): Unit = ++(graph)
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
  def __[Start, End](implicit cltblStart: ClassTypeable[Start],
                     cltblEnd: ClassTypeable[End]): Traversal[cltblStart.CT, cltblEnd.CT, HNil] =
    Traversal[cltblStart.CT, cltblEnd.CT]()(this, cltblStart.ct, cltblEnd.ct)
  def g(graph: Graph*): Traversal[DataType[Graph], DataType[Graph], HNil] =
    Traversal[DataType[Graph], DataType[Graph]]()(this, GraphType.datatype, GraphType.datatype)

  lazy val traversal: Traversal[DataType[Graph], DataType[Graph], HNil] = g

  def buildTraversersStream[ST <: ClassType[_], ET <: ClassType[_], Segments <: HList, Out](
      traversal: Traversal[ST, ET, Segments]): Stream[Out]
  def buildAsyncTraversersStream[ST <: ClassType[_], ET <: ClassType[_], Segments <: HList, Out](
      traversal: Traversal[ST, ET, Segments]): Task[Stream[Out]]

  def persist: CancelableFuture[Unit] = CancelableFuture.unit

  def close(): CancelableFuture[Unit] = CancelableFuture.unit

  override def toString: String = s"graph:$iri"
}
