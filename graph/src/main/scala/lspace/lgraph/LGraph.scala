package lspace.lgraph

import java.time.Instant

import lspace.lgraph.index.{IndexManager, IndexProvider, LIndex}
import lspace.lgraph.store._
import lspace.librarian.process.computer.DefaultStreamComputer
import monix.eval.Task
import lspace.librarian.process.traversal.Traversal
import lspace.librarian.provider.transaction.Transaction
import lspace.librarian.structure._
import lspace.librarian.structure.util.IdProvider
import shapeless.HList

object LGraph {
  def apply(storeProvider: StoreProvider, indexProvider: IndexProvider): LGraph = {
    val _iri           = storeProvider.iri
    val _storeProvider = storeProvider
    val _indexProvider = indexProvider

    val graph = new LDataGraph {
      val iri: String  = _iri
      private val self = this

      lazy val storeManager: StoreManager[this.type] = storeProvider.dataManager(this)

      def ns: NameSpaceGraph = new LNSGraph {
        val iri: String = _iri + ".ns"

        lazy val graph: LGraph = self

        private val _thisgraph = thisgraph
        lazy val index: LIndexGraph = new LIndexGraph {
          def iri: String = _iri + ".ns" + ".index"

          lazy val graph: LGraph      = _thisgraph
          lazy val index: LIndexGraph = this

          lazy val storeManager: StoreManager[this.type] = storeProvider.nsIndexManager(this)
          lazy val indexManager: IndexManager[this.type] = indexProvider.nsIndexManager(this)
        }

        lazy val storeManager: StoreManager[this.type] = storeProvider.nsManager(this)
      }

      def index: IndexGraph = new LIndexGraph {
        def iri: String = _iri + ".index"

        lazy val graph: LGraph = self
        private val _thisgraph = thisgraph

        lazy val index: LIndexGraph = new LIndexGraph {
          def iri: String = _iri + ".index" + ".index"

          lazy val graph: LGraph      = _thisgraph
          lazy val index: LIndexGraph = this

          lazy val storeManager: StoreManager[this.type] = storeProvider.indexIndexManager(this)
          lazy val indexManager: IndexManager[this.type] = indexProvider.indexIndexManager(this)
        }

        lazy val storeManager: StoreManager[this.type] = storeProvider.indexManager(this)
        lazy val indexManager: IndexManager[this.type] = indexProvider.indexManager(this)
      }

      val stateManager: GraphManager[this.type] = storeProvider.stateManager(this)
      override def idProvider: IdProvider       = stateManager.idProvider

      override def close(): Unit = {
        super.close()
        stateManager.close()
      }
    }
    graph.init()
    graph
  }
//  def apply(_iri: String, _idProvider: IdProvider, _ns: LNSGraph, _index: LIndexGraph)
}

trait LGraph extends Graph {
//  def createStoreManager[G <: LGraph]: G => StoreManager[G]
//  def stateManager: GraphManager[this.type]
  def storeManager: StoreManager[this.type]

  override protected[lgraph] val nodeStore: LNodeStore[this.type]   = LNodeStore("@node", thisgraph)
  override protected[lgraph] val edgeStore: LEdgeStore[this.type]   = LEdgeStore("@edge", thisgraph)
  override protected[lgraph] val valueStore: LValueStore[this.type] = LValueStore("@value", thisgraph)

  override protected def _createNode(id: Long)(ontology: Ontology*): _Node = {
    val node = LNode(id, thisgraph)
    node._lastoutsync = Some(Instant.now())
    ontology.foreach(node.addLabel)
    node
  }
  override protected def _createEdge[S, E](
      id: Long)(from: _Resource[S], key: Property, to: _Resource[E]): _Edge[S, E] = {
    val edge = LEdge(thisgraph)(id, from, key, to)
    edge._lastoutsync = Some(Instant.now())
    edge
  }
  override protected def _createValue[T](id: Long)(value: T)(dt: DataType[T]): _Value[T] = {
    val rv = LValue(id, value, dt, thisgraph)
    rv._lastoutsync = Some(Instant.now())
    rv
  }

  protected def _deleteResource(resource: _Resource[_]): Unit = {
    resource.asInstanceOf[LResource[Any]].outEMap().foreach {
      case (key, properties) =>
        properties.foreach(edge => edge.to.removeIn(edge))
    }
    resource.asInstanceOf[LResource[Any]].inEMap().foreach {
      case (key, properties) =>
        properties.foreach(edge => edge.from.removeOut(edge))
    }
  }

  override def transaction: Transaction = new LTransaction(thisgraph)

  val computer = DefaultStreamComputer()
  def buildTraversersStream[Start <: ClassType[_], End <: ClassType[_], Steps <: HList, Out](
      traversal: Traversal[Start, End, Steps])(ct: ClassType[_]): Stream[Out] =
    computer.traverse[Start, End, Steps, Out, this.type](traversal)(thisgraph)

  def buildAsyncTraversersStream[Start <: ClassType[_], End <: ClassType[_], Steps <: HList, Out](
      traversal: Traversal[Start, End, Steps])(ct: ClassType[_]): Task[Stream[Out]] =
    Task(computer.traverse[Start, End, Steps, Out, this.type](traversal)(thisgraph))

  override def close(): Unit = {
    super.close()
    storeManager.close()
  }
}
