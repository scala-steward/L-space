package lspace.librarian.provider.transaction

import lspace.librarian.process.computer.TransactionStreamComputer
import lspace.librarian.provider.mem._
import lspace.librarian.structure._
import lspace.librarian.structure.util.IdProvider

import scala.collection.mutable

object Transaction {}

/**
  * A transaction is build using an in-memory graph
  */
trait Transaction extends MemDataGraph {
  trait _TResource[T]                                                         extends _Resource[T] with TResource[T]
  case class _TNode(override val self: Node)(implicit val graph: Transaction) extends _Node with TNode
  case class _TEdge[S, E](override val self: Edge[S, E])(implicit val graph: Transaction)
      extends _Edge[S, E]
      with TEdge[S, E] {
    lazy val inV: Resource[E] =
      resources.hasId(self.inV.id).getOrElse(wrapTR(self.inV)).asInstanceOf[Resource[E]]
    lazy val outV: Resource[S] =
      resources.hasId(self.outV.id).getOrElse(wrapTR(self.outV)).asInstanceOf[Resource[S]]
    def key = self.key
  }
  case class _TValue[T](override val self: Value[T])(implicit val graph: Transaction) extends _Value[T] with TValue[T] {
    def value = self.value
    def label = self.label
  }

  def wrapTR[T <: Resource[_]](resource: T): TResource[_] = resource match {
    case n: Node       => super.nodes.hasId(n.id).map(_.asInstanceOf[TResource[_]]).getOrElse(_TNode(n))
    case e: Edge[_, _] => super.edges.hasId(e.id).map(_.asInstanceOf[TResource[_]]).getOrElse(_TEdge(e))
    case v: Value[_]   => super.values.hasId(v.id).map(_.asInstanceOf[TResource[_]]).getOrElse(_TValue(v))
  }

  private def cacheResource[R <: TResource[_]](r: R): R = {
    r.asInstanceOf[MemResource[_]] match { // why does the compiler need this cast??? or else it says 'pattern type is incompatible with expected type, found: lspace.librarian.provider.mem.MemValue[_], required: Transaction.this.TResource[_], case v: MemValue[_] =>;
      case n: _Node =>
        nodeStore.store(n)
      case e: _Edge[_, _] =>
        edgeStore.store(e)
      case v: _Value[_] =>
        valueStore.store(v)
    }
    r
  }

  def parent: Graph
  lazy val ns: NameSpaceGraph = parent.ns

  lazy val idProvider: IdProvider = parent.idProvider

  trait Resources extends super.Resources {
    override def apply(): Stream[Resource[_]] = {
      val tvalues = super.apply()
      tvalues ++ (parent.nodes() ++ parent.edges() ++ parent.values()).filterNot(n => tvalues.exists(_.id == n.id))
    }

    override def hasIri(iris: List[String]): List[Resource[_]] = {
      super.hasIri(iris) match {
        case List() =>
          parent.resources
            .hasIri(iris)
            .map {
              case n: Node       => _TNode(n)
              case e: Edge[_, _] => _TEdge(e)
              case v: Value[_]   => _TValue(v)
            }
        case l => l
      }
    }

    override def hasId(id: Long): Option[Resource[_]] = {
      super.hasId(id) match {
        case None =>
          parent.resources
            .hasId(id)
            .map {
              case n: Node       => _TNode(n)
              case e: Edge[_, _] => _TEdge(e)
              case v: Value[_]   => _TValue(v)
            }
        case l => l
      }
    }
  }

  override def resources: Resources = new Resources {}

  trait Nodes extends super.Nodes {
    val added: mutable.HashSet[_Node] = mutable.HashSet[_Node]()
    val deleted: mutable.Set[Long]    = mutable.Set[Long]()

    override def apply(): Stream[Node] = {
      val tnodes = super.apply()
      tnodes ++ parent.nodes().filterNot(n => tnodes.exists(_.id == n.id))
    }

    override def hasIri(iris: List[String]): List[Node] = {
      super.hasIri(iris) match {
        case List() => parent.nodes.hasIri(iris).map(_TNode(_))
        case l      => l
      }
    }

    override def hasId(id: Long): Option[Node] = {
      super.hasId(id) match {
        case None => parent.nodes.hasId(id).map(_TNode(_))
        case l    => l
      }
    }
  }

  override val nodes: Nodes = new Nodes {}

  trait Edges extends super.Edges {
    val added: mutable.HashSet[_Edge[_, _]] = mutable.HashSet[_Edge[_, _]]()
    val deleted: mutable.Set[Long]          = mutable.Set[Long]()

    override def apply(): Stream[Edge[_, _]] = {
      val tedges = super.apply()
      tedges ++ parent.edges().filterNot(n => tedges.exists(_.id == n.id))
    }

    override def hasIri(iris: List[String]): List[Edge[_, _]] = {
      super.hasIri(iris) match {
        case List() => parent.edges.hasIri(iris).map(_TEdge(_))
        case l      => l
      }
    }

    override def hasId(id: Long): Option[Edge[_, _]] = {
      super.hasId(id) match {
        case None => parent.edges.hasId(id).map(_TEdge(_))
        case l    => l
      }
    }
  }

  override val edges: Edges = new Edges {}

  trait Values extends super.Values {
    val added: mutable.HashSet[_Value[_]] = mutable.HashSet[_Value[_]]()
    val deleted: mutable.Set[Long]        = mutable.Set[Long]()

    override def apply(): Stream[Value[_]] = {
      val tvalues = super.apply()
      tvalues ++ parent.values().filterNot(n => tvalues.exists(_.id == n.id))
    }

    override def hasIri(iris: List[String]): List[Value[_]] = {
      super.hasIri(iris) match {
        case List() => parent.values.hasIri(iris).map(_TValue(_))
        case l      => l
      }
    }

    override def byValue(valueSet: List[Any]): List[Value[_]] = {
      super.byValue(valueSet) match {
        case List() => parent.values.byValue(valueSet).map(_TValue(_))
        case l      => l
      }
    }

    override def hasId(id: Long): Option[Value[_]] = {
      super.hasId(id) match {
        case None => parent.values.hasId(id).map(_TValue(_))
        case l    => l
      }
    }
  }

  override val values: Values = new Values {}

  protected var open: Boolean = true

  def commit(): Unit  = open = false
  def isOpen: Boolean = open

  /**
    * clears the transaction's MemGraph
    */
  def rollback(): Unit

  override protected def _createNode(_id: Long)(ontology: Ontology*): _Node = {
    val node = super._createNode(_id)(ontology: _*)
    nodes.added += node
    node
  }

  override protected def _createEdge[S, E](
      id: Long)(from: _Resource[S], key: Property, to: _Resource[E]): _Edge[S, E] = {
    val edge = super._createEdge(id)(from, key, to)
    edges.added += edge
    edge
  }

  override protected def _createValue[T](_id: Long)(_value: T)(dt: DataType[T]): _Value[T] = {
    val value = super._createValue(_id)(_value)(dt)
    values.added += value
    value
  }

  override protected def _deleteNode(node: _Node): Unit = {
    node match {
      case node: _TNode =>
        nodes.deleted += node.id
      case _ =>
        super._deleteNode(node)
    }
  }

  override protected def _deleteEdge(edge: _Edge[_, _]): Unit = {
    edge match {
      case edge: _TEdge[_, _] =>
        edges.deleted += edge.id
        edge.from match {
          case tr: TResource[_] => tr.deletedEdges += edge.id
          case _                =>
        }
        edge.to match {
          case tr: TResource[_] => tr.deletedEdges += edge.id
          case _                =>
        }
      case _ =>
        super._deleteEdge(edge)
    }
  }

  override protected def _deleteValue(value: _Value[_]): Unit = {
    value match {
      case value: _TValue[_] =>
        values.deleted += value.id
      case _ =>
        super._deleteValue(value)
    }
  }

  override protected def _deleteResource(resource: _Resource[_]): Unit = {

    resource.outE().foreach { e =>
      e.to match {
        case tr: TResource[_] => tr.deletedEdges += e.id
        case n: Node          =>
        case e: Edge[_, _]    =>
        case v: Value[_]      =>
      }
      e.remove()
    }
    resource.inE().foreach { e =>
      e.from match {
        case tr: TResource[_] => tr.deletedEdges += e.id
        case n: Node          =>
        case e: Edge[_, _]    =>
        case v: Value[_]      =>
      }
      e.remove()
    }
    super._deleteResource(resource)
  }

  override val computer = new TransactionStreamComputer(this)
}
