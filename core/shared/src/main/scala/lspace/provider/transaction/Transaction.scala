package lspace.provider.transaction

import java.util.concurrent.ConcurrentHashMap

import lspace.datatype.DataType
import lspace.provider.mem._
import lspace.structure._
import lspace.structure.util.{ClassTypeable, IdProvider}
import monix.eval.{Coeval, Task}
import monix.reactive.Observable

import scala.collection.JavaConverters._
import scala.collection.immutable.ListSet
import scala.collection.mutable

object Transaction {}

/**
  * A transaction is build using an in-memory graph
  */
abstract class Transaction(val parent: Graph) extends MemDataGraph {
//  override type GNode       = TNode
//  override type GEdge[S, E] = TEdge[S, E]
//  override type GValue[T]   = TValue[T]

  trait _TResource[T] extends _Resource[T] with TResource[T]
  object _TNode {
    def apply(self: parent._Node): Coeval[_TNode] = Coeval { new _TNode(self) }
  }
  class _TNode(override val self: parent._Node) extends _Node with TNode {
    val graph: Transaction = thisgraph
  }
  object _TEdge {
    def apply[S, E](self: parent._Edge[S, E]): Coeval[_TEdge[S, E]] = {
      for {
        from <- resources.cached
          .hasId(self.from.id)
          .map(_.asInstanceOf[Resource[S]])
          .map(Coeval.now)
          .getOrElse(wrapTR(self.from.asInstanceOf[parent.GResource[S]]).map(_.asInstanceOf[Resource[S]]))
        to <- resources.cached
          .hasId(self.to.id)
          .map(_.asInstanceOf[Resource[E]])
          .map(Coeval.now)
          .getOrElse(wrapTR(self.to.asInstanceOf[parent.GResource[E]]).map(_.asInstanceOf[Resource[E]]))
      } yield new _TEdge(self, from, to)
    }
  }
  class _TEdge[S, E](override val self: parent._Edge[S, E], val from: Resource[S], val to: Resource[E])
      extends _Edge[S, E]
      with TEdge[S, E] {
    val graph: Transaction = thisgraph
    def key                = self.key
  }
  object _TValue {
    def apply[T](self: parent._Value[T]): Coeval[_TValue[T]] = Coeval.now(new _TValue(self))

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
      case v: parent._Node       => nodeStore.cached.hasId(v.id).getOrElse(_TNode(v).value())
      case v: parent._Edge[_, _] => edgeStore.cached.hasId(v.id).getOrElse(_TEdge(v).value())
      case v: parent._Value[_]   => valueStore.cached.hasId(v.id).getOrElse(_TValue(v).value())
      case _                     => t
    }
  }
  class _TValue[T](override val self: parent._Value[T]) extends _Value[T] with TValue[T] {
    val graph: Transaction = thisgraph

    def value = _TValue.dereferenceValue(self.value).asInstanceOf[T]
    def label = self.label
  }

  def wrapTR[T <: parent.GResource[_]](resource: T): Coeval[_Resource[_]] = resource match {
    case n: parent._Node =>
      nodeStore.cached.hasId(n.id) match {
        case Some(node) => Coeval.now(node.asInstanceOf[_Node])
        case None       => _TNode(n)
      }
    case e: parent._Edge[Any, Any] =>
      edgeStore.cached
        .hasId(e.id) match {
        case Some(edge) => Coeval.now(edge.asInstanceOf[_Edge[_, _]])
        case None       => _TEdge[Any, Any](e.asInstanceOf[parent._Edge[Any, Any]])
      }
//      super.edgeStore.cached //compiler-error <refinement>.type (of class scala.reflect.internal.Types$UniqueSuperType)
//        .hasId(e.id)
//        .map(_.asInstanceOf[_Edge[_,_]])
//        .map(Coeval.now)
//        .get //OrElse(_TEdge[Any, Any](e.asInstanceOf[parent._Edge[Any, Any]]))
    case v: parent._Value[Any] =>
      valueStore.cached
        .hasId(v.id) match {
        case Some(value) => Coeval.now(value.asInstanceOf[_Value[Any]])
        case None        => _TValue[Any](v.asInstanceOf[parent.GValue[Any]])
      }
//        .map(_.asInstanceOf[_Value[Any]])
//        .map(Coeval.now)
//        .getOrElse(_TValue(v.asInstanceOf[parent.GValue[Any]]))
  }

  lazy val ns: NameSpaceGraph = parent.ns

  lazy val idProvider: IdProvider = parent.idProvider

  trait Resources extends super.Resources {
    override def apply(): Observable[Resource[_]] = {
      val tresources = super.apply()
      import scala.collection.JavaConverters._
      val idResourceMap: scala.collection.concurrent.Map[Long, Resource[_]] =
        new ConcurrentHashMap[Long, Resource[_]]().asScala
      tresources.map { resource =>
        idResourceMap += resource.id -> resource; resource
      } ++ parent.resources().filter(n => !idResourceMap.contains(n.id))
    }

    override def hasIri(iris: List[String]): Observable[Resource[_]] = {
      val fromTransaction = super.hasIri(iris)
      val fromParent = parent.resources
        .hasIri(iris)
        .mapEval {
          case n: parent._Node           => _TNode(n).task
          case e: parent._Edge[Any, Any] => _TEdge(e).task
          case v: parent._Value[Any]     => _TValue(v).task
        }
        .filter(n => nodes.deleted.contains(n.id) || edges.deleted.contains(n.id) || values.deleted.contains(n.id))
      val ids = fromTransaction.map(_.id)

      val idResourceMap: scala.collection.concurrent.Map[Long, Resource[_]] =
        new ConcurrentHashMap[Long, Resource[_]]().asScala
      fromTransaction.map { resource =>
        idResourceMap += resource.id -> resource; resource
      } ++ fromParent.filter(n => !idResourceMap.contains(n.id))
    }

    override def hasId(id: Long): Task[Option[Resource[_]]] = {
      if (nodes.deleted.contains(id) || edges.deleted.contains(id) || values.deleted.contains(id)) Task.now(None)
      else {
        for {
          r <- super
            .hasId(id)
          r1 <- if (r.nonEmpty) Task.now(r)
          else
            parent.resources
              .hasId(id)
              .flatMap {
                case Some(value) =>
                  (value match {
                    case n: parent._Node           => _TNode(n).task
                    case e: parent._Edge[Any, Any] => _TEdge(e).task
                    case v: parent._Value[Any]     => _TValue(v).task
                  }) map (Some(_))
                case None => Task.now(None)
              }
        } yield r1
      }
    }
  }
  private lazy val _resources: Resources = new Resources {}
  override def resources: Resources      = _resources

  trait Nodes extends super.Nodes {
    val added: scala.collection.concurrent.Map[Long, GNode] = new ConcurrentHashMap[Long, GNode]().asScala
    val deleted: scala.collection.concurrent.Map[Long, parent.GNode] =
      new ConcurrentHashMap[Long, parent.GNode]().asScala

    override def apply(): Observable[Node] = {
      val tnodes = super.apply()
      val idSet: scala.collection.concurrent.Map[Long, Node] =
        new ConcurrentHashMap[Long, Node]().asScala
      tnodes.map { node =>
        idSet += node.id -> node; node
      } ++ parent.nodes().filter(n => !idSet.contains(n.id))
    }

    override def hasIri(iris: List[String]): Observable[Node] = {
      val fromTransaction = super.hasIri(iris)
      val fromParent =
        parent.nodes
          .hasIri(iris)
          .asInstanceOf[Observable[parent.GNode]]
          .mapEval(_TNode(_).task)
          .filter(n => !deleted.contains(n.id))
      val idSet: scala.collection.concurrent.Map[Long, Node] =
        new ConcurrentHashMap[Long, Node]().asScala
      fromTransaction.map { node =>
        idSet += node.id -> node; node
      } ++ fromParent.filter(n => !idSet.contains(n.id))
    }

    override def hasId(id: Long): Task[Option[Node]] = {
      if (deleted.contains(id)) Task.now(None)
      else
        for {
          r <- super
            .hasId(id)
          r1 <- if (r.nonEmpty) Task.now(r)
          else
            parent.nodes
              .hasId(id)
              .flatMap {
                case Some(node) => _TNode(node.asInstanceOf[parent.GNode]).task.map(Some(_))
                case None       => Task.now(None)
              }
        } yield r1
    }

    override def hasId(id: List[Long]): Observable[Node] = {
      Observable
        .fromIterable(id)
        .filter(!deleted.contains(_))
        .flatMap { id =>
          Observable
            .fromTask(for {
              nodeOption <- super.hasId(id)
              nodeOrEdgeOption <- if (nodeOption.nonEmpty) Task.now(nodeOption)
              else
                parent.nodes.hasId(id).flatMap {
                  case Some(node) => _TNode(node.asInstanceOf[parent.GNode]).task.map(Some(_))
                  case None       => Task.now(None)
                }
            } yield nodeOrEdgeOption)
            .map(_.toList)
            .flatMap(Observable.fromIterable(_))
        }
    }
  }
  private lazy val _nodes: Nodes = new Nodes {}
  override def nodes: Nodes      = _nodes

  trait Edges extends super.Edges {
    val added: mutable.HashSet[GEdge[_, _]] = mutable.HashSet[GEdge[_, _]]()
    val deleted: scala.collection.concurrent.Map[Long, parent.GEdge[_, _]] =
      new ConcurrentHashMap[Long, parent.GEdge[_, _]]().asScala

    override def apply(): Observable[Edge[_, _]] = {
      val tedges = super.apply()
      val idSet: scala.collection.concurrent.Map[Long, Edge[_, _]] =
        new ConcurrentHashMap[Long, Edge[_, _]]().asScala
      tedges.map { edge =>
        idSet += edge.id -> edge; edge
      } ++ parent.edges().filter(n => !idSet.contains(n.id))
    }

    override def hasIri(iris: List[String]): Observable[Edge[_, _]] = {
      val fromTransaction = super.hasIri(iris)
      val fromParent = parent.edges
        .hasIri(iris)
        .asInstanceOf[Observable[parent.GEdge[Any, Any]]]
        .mapEval(_TEdge(_).task)
        .filter(n => !deleted.contains(n.id))
      val idSet: scala.collection.concurrent.Map[Long, Edge[_, _]] =
        new ConcurrentHashMap[Long, Edge[_, _]]().asScala
      fromTransaction.map { edge =>
        idSet += edge.id -> edge; edge
      } ++ fromParent.filter(n => idSet.contains(n.id))
    }

    override def hasId(id: Long): Task[Option[Edge[_, _]]] = {
      if (deleted.contains(id)) Task.now(None)
      else
        for {
          r <- super
            .hasId(id)
          r1 <- if (r.nonEmpty) Task.now(r)
          else
            parent.edges
              .hasId(id)
              .flatMap {
                case Some(edge) => _TEdge(edge.asInstanceOf[parent.GEdge[Any, Any]]).task.map(Some(_))
                case None       => Task.now(None)
              }
        } yield r1
    }
  }
  private lazy val _edges: Edges = new Edges {}
  override def edges: Edges      = _edges

  trait Values extends super.Values {
    val added: mutable.HashSet[GValue[_]]                    = mutable.HashSet[GValue[_]]()
    val deleted: mutable.OpenHashMap[Long, parent.GValue[_]] = mutable.OpenHashMap[Long, parent.GValue[_]]()

    override def apply(): Observable[Value[_]] = {
      val tvalues = super.apply()
      val idSet: scala.collection.concurrent.Map[Long, Value[_]] =
        new ConcurrentHashMap[Long, Value[_]]().asScala
      tvalues.map { value =>
        idSet += value.id -> value; value
      } ++ parent.values().filter(n => !idSet.contains(n.id))
    }

    override def hasIri(iris: List[String]): Observable[Value[_]] = {
      val fromTransaction = super.hasIri(iris)
      val fromParent = parent.values
        .hasIri(iris)
        .asInstanceOf[Observable[parent.GValue[Any]]]
        .mapEval(_TValue(_).task)
        .filter(n => !deleted.contains(n.id))
      val idSet: scala.collection.concurrent.Map[Long, Value[_]] =
        new ConcurrentHashMap[Long, Value[_]]().asScala
      fromTransaction.map { value =>
        idSet += value.id -> value; value
      } ++ fromParent.filter(n => !idSet.contains(n.id))
    }

    override def byValue[T, TOut, CTOut <: ClassType[_]](value: T)(
        implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut]): Observable[Value[T]] =
      byValue(List(value -> clsTpbl.ct.asInstanceOf[DataType[T]]))
    override def byValue[T](valueSet: List[(T, DataType[T])]): Observable[Value[T]] = {
      val fromTransaction = super.byValue(valueSet)
      val fromParent = parent.values
        .byValue(valueSet)
        .asInstanceOf[Observable[parent.GValue[T]]]
        .mapEval(_TValue(_).task)
        .filter(n => !deleted.contains(n.id))
      val idSet: scala.collection.concurrent.Map[Long, Value[_]] =
        new ConcurrentHashMap[Long, Value[_]]().asScala
      fromTransaction.map { value =>
        idSet += value.id -> value; value
      } ++ fromParent.filter(n => !idSet.contains(n.id))
    }

    override def hasId(id: Long): Task[Option[Value[_]]] = {
      if (deleted.contains(id)) Task.now(None)
      else
        for {
          r <- super
            .hasId(id)
          r1 <- if (r.nonEmpty) Task.now(r)
          else
            for {
              v <- parent.values
                .hasId(id)
              v1 <- if (v.nonEmpty) {
                _TValue(v.get.asInstanceOf[parent.GValue[Any]]).map(Some(_)).task
              } else Task.now(v)
            } yield v1
        } yield r1
    }
  }
  private lazy val _values: Values = new Values {}
  override def values: Values      = _values

  protected var open: Boolean = true

  def commit(): Task[Unit] = Task {
    open = false
  }
  def isOpen: Boolean = open

  /**
    * clears the transaction's MemGraph
    */
  def rollback(): Task[Unit]

  override protected[lspace] def newNode(id: Long): GNode = {
    val node = super.newNode(id)
    nodes.added += node.id -> node
    node
  }
  override protected[lspace] def getOrCreateNode(id: Long): Task[GNode] = {
    for {
      node <- super.getOrCreateNode(id)
    } yield node
  }

  override protected[lspace] def newEdge[S, E](id: Long,
                                               from: GResource[S],
                                               key: Property,
                                               to: GResource[E]): GEdge[S, E] = {
    val edge = super.newEdge(id, from, key, to)
    edges.added += edge
    edge
  }
  override protected def createEdge[S, E](id: Long,
                                          from: GResource[S],
                                          key: Property,
                                          to: GResource[E]): Task[GEdge[S, E]] = {
    for {
      edge <- super.createEdge(id, from, key, to)
    } yield edge
  }

  override protected[lspace] def newValue[T](id: Long, value: T, label: DataType[T]): GValue[T] = {
    val value0 = super.newValue(id, value, label)
    values.added += value0
    value0
  }
  override protected def createValue[T](_id: Long, _value: T, dt: DataType[T]): Task[GValue[T]] = {
    for {
      value <- super.createValue(_id, _value, dt)
    } yield value
  }

  override protected def deleteNode(node: GNode): Task[Unit] = Task.defer {
    node match {
      case node: _TNode =>
        nodes.deleted += node.id -> node.self.asInstanceOf[parent.GNode]
      case _ =>
    }
    nodes.added -= node.id
    super.deleteNode(node)
  }

  override protected def deleteEdge(edge: GEdge[_, _]): Task[Unit] = Task.defer {
    edge match {
      case edge: _TEdge[_, _] =>
        edges.deleted += edge.id -> edge.self.asInstanceOf[parent.GEdge[_, _]]
        edge.from match {
          case tr: TResource[_] => tr.deletedEdges += edge.id
          case _                =>
        }
        edge.to match {
          case tr: TResource[_] => tr.deletedEdges += edge.id
          case _                =>
        }
      case _ =>
    }
    edges.added -= edge
    super.deleteEdge(edge)
  }

  /**
    * deletes the Value from the transaction and marks the id as deleted
    * @param value
    */
  override protected def deleteValue(value: GValue[_]): Task[Unit] = Task.defer {
    value match {
      case value: _TValue[_] =>
        values.deleted += value.id -> value.self.asInstanceOf[parent.GValue[_]]
      case _ =>
    }
    values.added -= value
    super.deleteValue(value)
  }
}
