package lspace.provider.transaction

import lspace.datatype.DataType
import lspace.provider.mem._
import lspace.structure._
import lspace.structure.util.IdProvider
import monix.eval.{Coeval, Task}

import scala.collection.immutable.ListSet

object Transaction {}

/**
  * A transaction is build using an in-memory graph
  */
abstract class Transaction(val parent: Graph) extends MemDataGraph {
//  override type GResource[T] = TResource[T]
//  override type GNode        = TNode
//  override type GEdge[S, E]  = TEdge[S, E]
//  override type GValue[T]    = TValue[T]

  trait _TResource[T] extends _Resource[T] with MemResource[T] with TResource[T]
  object _TNode {
    def apply(self: parent._Node): Coeval[_TNode] = Coeval { new _TNode(self) }
  }
  class _TNode(override val self: parent._Node) extends _Node with _TResource[Node] with MemNode with TNode {
    val graph: Transaction = thisgraph
  }
  object _TEdge {
    def apply[S, E](self: parent._Edge[S, E]): Coeval[_TEdge[S, E]] = {
      for {
        from <- resources.cached
          .hasId(self.from.id)
          .map(_.asInstanceOf[Resource[S]])
          .map(Coeval.now)
          .getOrElse(wrapTR(self.from.asInstanceOf[parent._Resource[S]]).map(_.asInstanceOf[Resource[S]]))
        to <- resources.cached
          .hasId(self.to.id)
          .map(_.asInstanceOf[Resource[E]])
          .map(Coeval.now)
          .getOrElse(wrapTR(self.to.asInstanceOf[parent._Resource[E]]).map(_.asInstanceOf[Resource[E]]))
      } yield new _TEdge(self, from, to)
    }
  }
  class _TEdge[S, E](override val self: parent._Edge[S, E], val from: Resource[S], val to: Resource[E])
      extends _Edge[S, E]
      with MemEdge[S, E]
      with _TResource[Edge[S, E]]
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
  class _TValue[T](override val self: parent._Value[T])
      extends _Value[T]
      with MemValue[T]
      with _TResource[T]
      with TValue[T] {
    val graph: Transaction = thisgraph

    def value = _TValue.dereferenceValue(self.value).asInstanceOf[T]
    def label = self.label
  }

  def wrapTR[T <: parent._Resource[_]](resource: T): Coeval[GResource[_]] = resource match {
    case n: parent._Node =>
      nodeStore.cached.hasId(n.id) match {
        case Some(node) => Coeval.now(node.asInstanceOf[GResource[_]] )
        case None       => _TNode(n).asInstanceOf[Coeval[GResource[_]]]
      }
    case e: parent._Edge[_, _] =>
      edgeStore.cached
        .hasId(e.id) match {
        case Some(edge) => Coeval.now(edge.asInstanceOf[GResource[_]] )
        case None       => _TEdge[Any, Any](e.asInstanceOf[parent._Edge[Any, Any]]).asInstanceOf[Coeval[GResource[_]]]
      }
//      super.edgeStore.cached //compiler-error <refinement>.type (of class scala.reflect.internal.Types$UniqueSuperType)
//        .hasId(e.id)
//        .map(_.asInstanceOf[_Edge[_,_]])
//        .map(Coeval.now)
//        .get //OrElse(_TEdge[Any, Any](e.asInstanceOf[parent._Edge[Any, Any]]))
    case v: parent._Value[_] =>
      valueStore.cached
        .hasId(v.id) match {
        case Some(value) => Coeval.now(value.asInstanceOf[GResource[_]] )
        case None        => _TValue[Any](v.asInstanceOf[parent.GValue[Any]]).asInstanceOf[Coeval[GResource[_]]]
      }
//        .map(_.asInstanceOf[_Value[Any]])
//        .map(Coeval.now)
//        .getOrElse(_TValue(v.asInstanceOf[parent.GValue[Any]]))
    case _ => throw new Exception(s"unexpected type ${resource.getClass.getSimpleName}")
  }

  lazy val ns: NameSpaceGraph = parent.ns

  protected[lspace] lazy val idProvider: IdProvider = parent.idProvider

  private lazy val _resources: TResources[this.type] = new TResources[this.type](this) {}
  override def resources: TResources[this.type]      = _resources

  private lazy val _nodes: TNodes[this.type] = new TNodes[this.type](this) {}
  override def nodes: TNodes[this.type]      = _nodes

  private lazy val _edges: TEdges[this.type] = new TEdges[this.type](this) {}
  override def edges: TEdges[this.type]      = _edges

  private lazy val _values: TValues[this.type] = new TValues[this.type](this) {}
  override def values: TValues[this.type]      = _values

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
                                               from: _Resource[S],
                                               key: Property,
                                               to: _Resource[E]): GEdge[S, E] = {
    val edge = super.newEdge(id, from, key, to)
    edges.added += edge
    edge
  }
  override protected[lspace] def createEdge[S, E](id: Long,
                                                  from: _Resource[S],
                                                  key: Property,
                                                  to: _Resource[E]): Task[GEdge[S, E]] = {
    for {
      edge <- super.createEdge(id, from, key, to)
    } yield edge
  }

  override protected[lspace] def newValue[T](id: Long, value: T, label: DataType[T]): GValue[T] = {
    val value0 = super.newValue(id, value, label)
    values.added += value0
    value0
  }
  override protected[lspace] def createValue[T](_id: Long, _value: T, dt: DataType[T]): Task[GValue[T]] = {
    for {
      value <- super.createValue(_id, _value, dt)
    } yield value
  }

  override protected[lspace] def deleteNode(node: _Node): Task[Unit] = Task.defer {
    node match {
      case node: _TNode =>
        nodes.deleted += node.id -> node.self.asInstanceOf[parent.GNode]
      case _ =>
    }
    nodes.added -= node.id
    super.deleteNode(node)
  }

  override protected[lspace] def deleteEdge(edge: _Edge[_, _]): Task[Unit] = Task.defer {
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
      case edge: GEdge[_, _] =>
        edges.added -= edge
      case _ => throw new Exception(s"unexpected type ${edge.getClass.getSimpleName}")
    }
    super.deleteEdge(edge)
  }

  /**
    * deletes the Value from the transaction and marks the id as deleted
    * @param value
    */
  override protected[lspace] def deleteValue(value: _Value[_]): Task[Unit] = Task.defer {
    value match {
      case value: _TValue[_] =>
        values.deleted += value.id -> value.self.asInstanceOf[parent.GValue[_]]
      case value: GValue[_] =>
        values.added -= value
      case _ => throw new Exception(s"unexpected type ${value.getClass.getSimpleName}")
    }
    super.deleteValue(value)
  }
}
