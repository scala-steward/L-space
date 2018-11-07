package lspace.librarian.structure

import java.time.Instant

import monix.eval.Task
import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.structure.Property.default
import lspace.NS
import lspace.librarian.datatype.GraphType
import shapeless.{::, HList, HNil}

import scala.collection.mutable
import scala.util.control.NonFatal

object Graph {
  val graphs: mutable.HashMap[String, Graph] = mutable.HashMap[String, Graph]()
  lazy val baseKeys = Set(
    Property.default.iri,
    Property.default.iris,
    Property.default.TYPE,
    Property.default.createdon,
    Property.default.modifiedon,
    Property.default.deletedon,
    Property.default.transcendedOn
  )
  lazy val reservedKeys = Set(
    Property.default.iri,
    Property.default.value,
    Property.default.iris,
    Property.default.TYPE,
    Property.default.container,
    Property.default.label,
    Property.default.comment,
    Property.default.createdon,
    Property.default.modifiedon,
    Property.default.deletedon,
    Property.default.transcendedOn,
    Property.default.properties,
    Property.default.graph,
    Property.default.range,
    Property.default.EXTENDS
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
  implicit lazy val thisgraph: this.type = this
  def ns: NameSpaceGraph

  def init(): Unit

  /**
    * Links A.K.A. Edges A.K.A. Properties
    * @return
    */
  def links: Stream[Edge[_, _]]

  /**
    * Nodes A.K.A. Vertices
    * @return
    */
  def nodes: Stream[Node]
  def values: Stream[Value[_]]

  protected def _createNode(ontology: Ontology*): Node
  def createNode(ontology: Ontology*): Node = _createNode(ontology: _*)
  def +(label: Ontology): Node              = createNode(label)

  /**
    * adds a node by reference (iri(s))
    * @param node
    * @return
    */
  def +(node: Node): Node = upsertNode(node)

  /**
    * deletes a node
    * @param node
    */
  def -(node: Node): Unit = deleteNode(node)

  /**
    * adds a node by every detail
    * @param node
    * @return
    */
  def ++(node: Node): Node = postNode(node)

  /**
    * adds an edge by reference (from --- key --> to)
    * @param edge
    * @tparam S
    * @tparam E
    * @return
    */
  def +[S, E](edge: Edge[S, E]): Edge[S, E] = upsertEdge(edge)

  /**
    * deletes an edge
    * @param edge
    */
  def -(edge: Edge[_, _]): Unit = deleteEdge(edge)

  /**
    * adds an edge by reference (from --- key --> to) and meta-properties (edge.outE())
    * @param edge
    * @tparam S
    * @tparam E
    * @return
    */
  def ++[S, E](edge: Edge[S, E]): Edge[S, E] = postEdge(edge)

  /**
    * adds a value
    * @param value
    * @tparam V
    * @return
    */
  def +[V](value: Value[V]): Value[V] = upsertValue(value)

  /**
    * deletes a value
    * @param value
    */
  def -(value: Value[_]): Unit = deleteValue(value)

  /**
    * adds a value and meta-properties (value.outE())
    * @param value
    * @tparam V
    * @return
    */
  def ++[V](value: Value[V]): Value[V] = postValue(value)

  protected def _createEdge[S, E](from: Resource[S], key: Property, to: Resource[E]): Edge[S, E]
  def createEdge[S, E](from: Resource[S], key: Property, to: Resource[E]): Edge[S, E] = _createEdge(from, key, to)

  protected def _createValue[T](value: T)(dt: DataType[T]): Value[T]
  def createValue[T](value: T)(dt: DataType[T]): Value[T] = _createValue(value)(dt)

  /**
    *
    * @param iri a uri to a resource
    * @return all vertices which identify by the iri, expected to return (in time) only a single vertex due to eventual consistency
    */
  def getNode(iri: String): List[Node] = getNodes(Set(iri))
  def getNodeById(id: Long): Option[Node]
  def getEdge(iri: String): List[Edge[_, _]] = getEdges(Set(iri))
  def getEdgeById(id: Long): Option[Edge[_, _]]
  def getValue[T](value: Any)(dt: DataType[T]): List[Value[_]] =
    getValues(Set(dt -> value))
  def getValueById(id: Long): Option[Value[_]]
  def getResource(iri: String): List[Resource[_]] = getResources(Set(iri))
  def getResourceById(id: Long): Option[Resource[_]]

  /**
    *
    * @param iris a set of uri's to get nodes for
    * @return
    */
  def getNodes(iris: Set[String]): List[Node] = {
    val iriList = iris.toList.filter(_.nonEmpty)
    Traversal.WithTraversalStream(g.N.hasIri(iris)).toList
    if (iriList.nonEmpty) g.N().hasIri(iris).toList //.asInstanceOf[List[Node]]
    else List[Node]()
  }

  def getEdges(iris: Set[String]): List[Edge[_, _]] = {
    val iriList = iris.toList.filter(_.nonEmpty)
    if (iriList.nonEmpty) g.E.hasIri(iris).toList //.asInstanceOf[List[Edge[_, _]]]
    else List[Edge[_, _]]()
  }

  def getValues(valueSet: Set[(DataType[_], Any)]): List[Value[_]] = {
    val valueList = valueSet.toList.filter(_ != null)
    if (valueList.nonEmpty) values.filter(v => valueSet.map(_._2).contains(v.value)).toList
    //      or(
    //      _.has(this.id, p.Within(iriList)),
    //      _.has(this.ids, p.Within(iriList))).toSet
    else List[Value[_]]()
  }

  def getResources(iris: Set[String]): List[Resource[_]] = {
    val validIris = iris.filter(_.nonEmpty)
    getNodes(iris) ++ getEdges(iris) ++ getValues(validIris.map(DataType.default.textType -> _))
  }

  /**
    *
    * @param uri a uri which should all resolve to the same resource as param uris
    * @param uris a set of uri's which should all resolve to the same resource
    * @return all vertices which identify by the uri's, expected to return (in time) only a single vertex due to eventual consistency
    */
  def upsertNode(uri: String, uris: Set[String] = Set()): Node = {
    val nodes = getNodes(uris + uri)
    val node: Node = if (nodes.isEmpty) {
      val node = createNode()
      if (uri.nonEmpty) node.addOut(default.typed.iriUrlString, uri)
      else if (uris.headOption.exists(_.nonEmpty))
        node.addOut(default.typed.iriUrlString, uris.head)
//      node.addOut(default.typed.createdonDateTime, Instant.now())
      node
    } else if (nodes.size > 1) {
      mergeNodes(nodes.toSet)
    } else nodes.head
    val newIris = ((uris + uri) diff node.iris).toList.filter(_.nonEmpty)
    if (newIris.nonEmpty) newIris.foreach(node.addOut(default.iris, _))
    node
  }

  def upsertResource[V](value: Resource[V]): Resource[V] = {
    value match {
      case resource: Resource[V] if resource.graph.iri == iri =>
        resource.self
      case resource: Resource[V] =>
        resource match {
          case node: Node =>
            upsertNode(node).asInstanceOf[Resource[V]]
          case edge: Edge[_, _] =>
            upsertEdge(edge).asInstanceOf[Resource[V]]
          case value: Value[V] =>
            upsertValue(value).asInstanceOf[Resource[V]]
          case _ =>
            println(s"${value.getClass.toString}")
            println(s"${value.asInstanceOf[Edge[_, _]].labels.map(_.iri).mkString(" and ")}")
            throw new Exception("???")
        }
    }
  }

  def upsertNode(node: Node): Node = {
    if (node.graph != this) {
      if (node.iri.nonEmpty) upsertNode(node.iri)
      else {
        val newNode = createNode()
        node.labels.foreach(newNode.addLabel)
        addMeta(node, newNode)
        newNode
      }
    } else node
  }

  def upsertEdge[S, E](edge: Edge[S, E]): Edge[S, E] = {
    if (edge.graph != this) {
      val newEdge = upsertResource[S](edge.from).addOut(edge.key, upsertResource[E](edge.to))
      newEdge
    } else edge
  }

  def upsertValue[V](value: Value[V]): Value[V] = {
    if (value.graph != this) {
      getResource(value.iri) match {
        case List() => createValue(value.value)(value.label)
        case List(storedValue: Value[_]) if storedValue.value == value.value =>
          storedValue.asInstanceOf[Value[V]]
        case List(value: Value[_], _*) =>
          throw new Exception("multiple values with the same iri, what should we do?! Dedup?")
      }
    } else value
  }

  protected def _deleteNode(node: Node): Unit
  def deleteNode(node: Node): Unit = {
    _deleteNode(node)
  }

  protected def _deleteEdge(edge: Edge[_, _]): Unit
  def deleteEdge(edge: Edge[_, _]): Unit = {
    _deleteEdge(edge)
  }

  protected def _deleteValue(value: Value[_]): Unit
  def deleteValue(value: Value[_]): Unit = {
    _deleteValue(value)
  }

  def getDT[V](resource: Resource[V]): ClassType[V] = resource match {
    case node: Node       => DataType.default.nodeURLType.asInstanceOf[ClassType[V]]
    case edge: Edge[_, _] => DataType.default.edgeURLType.asInstanceOf[ClassType[V]]
    case value: Value[_]  => value.label.asInstanceOf[ClassType[V]]
  }

  private def addMeta[S <: Resource[_], T, RT[Z] <: Resource[Z]](source: S, target: RT[T]): Unit =
    source.outE().filterNot(p => Graph.baseKeys.contains(p.key)).foreach { edge =>
      addMeta(edge, createEdge(target, edge.key, upsertResource(edge.to)))
    }

  /**
    * adds a node to the graph including all edges and (meta) edges on edges as long as edges have edges
    * @param node
    * @tparam T
    * @return
    */
  def postNode(node: Node): Node = {
    if (node.graph != this) {
      val newNode =
        if (node.iri.nonEmpty) upsertNode(node.iri, node.iris) else createNode()
      node.labels.foreach(newNode.addLabel)
      addMeta(node, newNode)
      newNode
    } else node
  }

  /**
    * adds an edge and upserts the 'from' and 'to' resource and adds (meta) edges on edges as long as edges have edges
    * @param edge
    * @tparam S
    * @tparam E
    * @tparam T
    * @return
    */
  def postEdge[S, E](edge: Edge[S, E]): Edge[S, E] = {
    if (edge.graph != this) {
      val newEdge = upsertResource[S](edge.from).addOut(edge.key, upsertResource[E](edge.to))
      addMeta(edge, newEdge)
      newEdge
    } else edge
  }

  /**
    * adds a value to the graph including all edges and (meta) edges on edges as long as edges have edges
    * @param value
    * @tparam V
    * @tparam T
    * @return
    */
  def postValue[V](value: Value[V]): Value[V] = {
    if (value.graph != this) {
      val newValue = getResource(value.iri) match {
        case List() => createValue(value.value)(value.label)
        case List(storedValue: Value[_]) if storedValue.value == value.value =>
          storedValue.asInstanceOf[Value[V]]
        case List(value: Value[_], _*) =>
          throw new Exception("multiple values with the same iri, what should we do?! Dedup?")
      }
      addMeta(value, newValue)
      newValue
    } else value
  }

  def getLink(iri: String): Set[Edge[_, _]] =
    g.E().has(default.iri, P.eqv(iri)).toSet.asInstanceOf[Set[Edge[_, _]]]

  def mergeNodes(nodes: Set[Node]): Node = {
//    val nodesByCreatedOnDateTime =
//      nodes.toList.sortBy(_.out(default.typed.createdonDateTime).take(1).map(_.toEpochMilli).head)
    val nodesSortedById =
      nodes.toList.sortBy(_.id)

    val (unmerged, transcended) =
//      nodesByCreatedOnDateTime.partition(_.out(default.typed.transcendedOnDateTime).isEmpty)
      nodesSortedById.partition(_.out(default.typed.transcendedOnDateTime).isEmpty)
    unmerged.tail.foreach { slave =>
      val masterVertexOntologies = unmerged.head.labels //out(this.typeOntology).map(Ontology.wrap)
      val suborVertexOntologies  = slave.labels //out(this.typeOntology).map(Ontology.wrap)
      val ontologyGap            = suborVertexOntologies.diff(masterVertexOntologies)
      val typesToAdd             = mutable.HashSet[Ontology]()
      val typesToRemove          = mutable.HashSet[Ontology]()
      ontologyGap.foreach { ontology =>
        if (!masterVertexOntologies.exists(_.`extends`(ontology))) {
          masterVertexOntologies.find(o => ontology.`extends`(o)) match {
            case Some(inheritedOntology) =>
              typesToRemove += inheritedOntology
            case None =>
          }
          typesToAdd += ontology
        }
      }
      //      typesToRemove.foreach(_.remove()) ???
      typesToAdd
        .filterNot(tpe => typesToAdd.exists(_.`extends`(tpe)))
        .foreach(tpe => unmerged.head.addLabel(tpe))
      val linksIn = slave.inEMap()

      linksIn.foreach {
        case (key, properties) =>
          properties.foreach { property =>
            scala.util.control.NonFatal
            try {
              property.from.addOut(property.key, unmerged.head)
            } catch {
              case NonFatal(e) =>
                println(property.key.iri)
                println(property.from.iri)
                println(property.from.value)
                throw e
            }
          }
      }
      val linksOut = slave.outEMap().filterNot(p => Graph.baseKeys.contains(p._1))
      linksOut.foreach {
        case (key, properties) =>
          properties.foreach { property =>
            unmerged.head.addOut(property.key, property.to)
          }
      }
      slave.addOut(default.typed.transcendedOnDateTime, Instant.now())
    }
    transcended.foreach(_.remove())
    unmerged.head
  }

  def ++(graph: Graph): Unit = {
    if (graph != this) {
      graph.g.N.toList.foreach { node =>
        postNode(node)
      }
//      graph.g.E.toStream.foreach { edge =>
//        postEdge(edge)
//      }
//      graph.g.V.toList.foreach { value =>
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
