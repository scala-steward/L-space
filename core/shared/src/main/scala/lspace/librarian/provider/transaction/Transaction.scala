package lspace.librarian.provider.transaction

import lspace.librarian.process.computer.TransactionStreamComputer
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.mem._
import lspace.librarian.structure._
import lspace.librarian.structure.util.IdProvider

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
  case class _TNode(override val self: parent._Node) extends _Node with TNode {
    val graph: Transaction = thisgraph
  }
  case class _TEdge[S, E](override val self: parent._Edge[S, E]) extends _Edge[S, E] with TEdge[S, E] {
    val graph: Transaction = thisgraph

    lazy val to: Resource[E] =
      resources.hasId(self.to.id).getOrElse(wrapTR(self.to.asInstanceOf[parent.GResource[E]])).asInstanceOf[Resource[E]]
    lazy val from: Resource[S] =
      resources
        .hasId(self.from.id)
        .getOrElse(wrapTR(self.from.asInstanceOf[parent.GResource[S]]))
        .asInstanceOf[Resource[S]]
    def key = self.key
  }
  case class _TValue[T](override val self: parent._Value[T]) extends _Value[T] with TValue[T] {
    val graph: Transaction = thisgraph

    def value = self.value
    def label = self.label
  }

  def wrapTR[T <: parent.GResource[_]](resource: T): TResource[_] = resource match {
    case n: parent.GNode => super.nodes.hasId(n.id).map(_.asInstanceOf[TNode]).getOrElse(_TNode(n))
    case e: parent.GEdge[Any, Any] =>
      super.edges
        .hasId(e.id)
        .map(_.asInstanceOf[TEdge[Any, Any]])
        .getOrElse(_TEdge(e.asInstanceOf[parent.GEdge[Any, Any]]))
    case v: parent.GValue[Any] =>
      super.values.hasId(v.id).map(_.asInstanceOf[TValue[Any]]).getOrElse(_TValue(v.asInstanceOf[parent.GValue[Any]]))
  }

  lazy val ns: NameSpaceGraph = parent.ns

  lazy val idProvider: IdProvider = parent.idProvider

  trait Resources extends super.Resources {
    override def apply(): Stream[Resource[_]] = {
      val tvalues = super.apply()
      tvalues ++ parent.resources().filterNot(n => tvalues.exists(_.id == n.id))
    }

    override def hasIri(iris: List[String]): List[Resource[_]] = {
      val fromTransaction = super.hasIri(iris)
      val fromParent = parent.resources
        .hasIri(iris)
        .map {
          case n: parent._Node           => _TNode(n)
          case e: parent._Edge[Any, Any] => _TEdge(e)
          case v: parent._Value[Any]     => _TValue(v)
        }
        .filterNot(n => nodes.deleted.contains(n.id) || edges.deleted.contains(n.id) || values.deleted.contains(n.id))
      val ids = fromTransaction.map(_.id)
      fromTransaction ++ fromParent.filter(n => ids.contains(n.id))
    }

    override def hasId(id: Long): Option[Resource[_]] = {
      if (nodes.deleted.contains(id) || edges.deleted.contains(id) || values.deleted.contains(id)) None
      else
        super
          .hasId(id)
          .orElse(parent.resources.hasId(id).map {
            case n: parent._Node           => _TNode(n)
            case e: parent._Edge[Any, Any] => _TEdge(e)
            case v: parent._Value[Any]     => _TValue(v)
          })
    }
  }

  override def resources: Resources = new Resources {}

  trait Nodes extends super.Nodes {
    val added: mutable.OpenHashMap[Long, GNode]          = mutable.OpenHashMap[Long, GNode]()
    val deleted: mutable.OpenHashMap[Long, parent.GNode] = mutable.OpenHashMap[Long, parent.GNode]()

    override def apply(): Stream[Node] = {
      val tnodes = super.apply()
      tnodes ++ parent.nodes().filterNot(n => tnodes.exists(_.id == n.id))
    }

    override def hasIri(iris: List[String]): List[Node] = {
      val fromTransaction = super.hasIri(iris)
      val fromParent =
        parent.nodes.hasIri(iris).asInstanceOf[List[parent.GNode]].map(_TNode).filterNot(n => deleted.contains(n.id))
      val ids = fromTransaction.map(_.id)
      fromTransaction ++ fromParent.filterNot(n => ids.contains(n.id))
    }

    override def hasId(id: Long): Option[Node] = {
      if (deleted.contains(id)) None
      else super.hasId(id).orElse(parent.nodes.hasId(id).map(_.asInstanceOf[parent.GNode]).map(_TNode))
    }
  }

  override val nodes: Nodes = new Nodes {}

  trait Edges extends super.Edges {
    val added: mutable.HashSet[GEdge[_, _]]                    = mutable.HashSet[GEdge[_, _]]()
    val deleted: mutable.OpenHashMap[Long, parent.GEdge[_, _]] = mutable.OpenHashMap[Long, parent.GEdge[_, _]]()

    override def apply(): Stream[Edge[_, _]] = {
      val tedges = super.apply()
      tedges ++ parent.edges().filterNot(n => tedges.exists(_.id == n.id))
    }

    override def hasIri(iris: List[String]): List[Edge[_, _]] = {
      val fromTransaction = super.hasIri(iris)
      val fromParent = parent.edges
        .hasIri(iris)
        .asInstanceOf[List[parent.GEdge[Any, Any]]]
        .map(_TEdge(_))
        .filterNot(n => deleted.contains(n.id))
      val ids = fromTransaction.map(_.id)
      fromTransaction ++ fromParent.filterNot(n => ids.contains(n.id))
    }

    override def hasId(id: Long): Option[Edge[_, _]] = {
      if (deleted.contains(id)) None
      else super.hasId(id).orElse(parent.edges.hasId(id).map(_.asInstanceOf[parent.GEdge[Any, Any]]).map(_TEdge(_)))
    }
  }

  override val edges: Edges = new Edges {}

  trait Values extends super.Values {
    val added: mutable.HashSet[GValue[_]]                    = mutable.HashSet[GValue[_]]()
    val deleted: mutable.OpenHashMap[Long, parent.GValue[_]] = mutable.OpenHashMap[Long, parent.GValue[_]]()

    override def apply(): Stream[Value[_]] = {
      val tvalues = super.apply()
      tvalues ++ parent.values().filterNot(n => tvalues.exists(_.id == n.id))
    }

    override def hasIri(iris: List[String]): List[Value[_]] = {
      val fromTransaction = super.hasIri(iris)
      val fromParent = parent.values
        .hasIri(iris)
        .asInstanceOf[List[parent.GValue[Any]]]
        .map(_TValue(_))
        .filterNot(n => deleted.contains(n.id))
      val ids = fromTransaction.map(_.id)
      fromTransaction ++ fromParent.filterNot(n => ids.contains(n.id))
    }

    override def byValue[T, TOut, CTOut <: ClassType[_]](value: T)(
        implicit clsTpbl: ClassTypeable.Aux[T, TOut, CTOut]): List[Value[T]] =
      byValue(List(value -> clsTpbl.ct.asInstanceOf[DataType[T]]))
    override def byValue[T](valueSet: List[(T, DataType[T])]): List[Value[T]] = {
      val fromTransaction = super.byValue(valueSet)
      val fromParent = parent.values
        .byValue(valueSet)
        .asInstanceOf[List[parent.GValue[T]]]
        .map(_TValue(_))
        .filterNot(n => deleted.contains(n.id))
      val ids = fromTransaction.map(_.id)
      fromTransaction ++ fromParent.filter(n => ids.contains(n.id))
    }

    override def hasId(id: Long): Option[Value[_]] = {
      if (deleted.contains(id)) None
      else super.hasId(id).orElse(parent.values.hasId(id).map(_.asInstanceOf[parent.GValue[Any]]).map(_TValue(_)))
    }
  }

  override val values: Values = new Values {}

  protected var open: Boolean = true

  def commit(): Unit = {
    open = false
  }
  def isOpen: Boolean = open

  /**
    * clears the transaction's MemGraph
    */
  def rollback(): Unit

  override protected def getOrCreateNode(id: Long): GNode = synchronized {
    try {
      val node = super.getOrCreateNode(id)
      nodes.added += node.id -> node
      node
    } catch {
      case t: Throwable =>
        t.printStackTrace()
        throw t
    }
  }

  override protected def createEdge[S, E](id: Long,
                                          from: GResource[S],
                                          key: Property,
                                          to: GResource[E]): GEdge[S, E] = {
    val edge = super.createEdge(id, from, key, to)
    edges.added += edge
    edge
  }

  override protected def createValue[T](_id: Long, _value: T, dt: DataType[T]): GValue[T] = {
    val value = super.createValue(_id, _value, dt)
    values.added += value
    value
  }

  override protected def deleteNode(node: GNode): Unit = {
    node match {
      case node: _TNode =>
        nodes.deleted += node.id -> node.self.asInstanceOf[parent.GNode]
      case _ =>
    }
    nodes.added -= node.id
    super.deleteNode(node)
  }

  override protected def deleteEdge(edge: GEdge[_, _]): Unit = {
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
  override protected def deleteValue(value: GValue[_]): Unit = {
    value match {
      case value: _TValue[_] =>
        values.deleted += value.id -> value.self.asInstanceOf[parent.GValue[_]]
      case _ =>
    }
    values.added -= value
    super.deleteValue(value)
  }

  override val computer = new TransactionStreamComputer(this)
}
