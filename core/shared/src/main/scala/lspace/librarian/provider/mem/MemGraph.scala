package lspace.librarian.provider.mem

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime}
import java.util.concurrent.atomic.AtomicLong

import lspace.NS
import monix.eval.Task
import lspace.librarian.process.computer.DefaultStreamComputer
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.index.MemIndex
import lspace.librarian.provider.mem.store.{MemEdgeStore, MemNodeStore, MemStore, MemValueStore}
import lspace.librarian.provider.transaction.Transaction
import lspace.librarian.structure._
import lspace.librarian.structure.index.Index
import lspace.librarian.structure.store.{EdgeStore, NodeStore, Store, ValueStore}
import lspace.librarian.structure.util.IdProvider
import lspace.types.vector.Point
import shapeless.{::, HList}

import scala.collection.mutable

object MemGraph {

  lazy val default: MemGraph = {
    //    Graph.graphs += "default" -> MemGraphDefault
    MemGraphDefault
  }

  def apply(_iri: String): MemGraph = {
    MemGraphDefault.iri
    val graph = new MemDataGraph {
      val iri: String = _iri

      lazy val idProvider: IdProvider = new IdProvider {
        private val id = new AtomicLong()
        id.set(1000)
        def next: Long = id.incrementAndGet
      }

      private val self = this
      lazy val ns: MemNSGraph = new MemNSGraph {
        def iri: String = _iri + ".ns"

        lazy val graph: MemGraph = self
        private val _thisgraph   = thisgraph
        lazy val index: MemIndexGraph = new MemIndexGraph {
          def iri: String = _iri + ".ns" + ".index"

          lazy val graph: MemGraph      = _thisgraph
          lazy val index: MemIndexGraph = this
        }
      }
      val index: MemIndexGraph = new MemIndexGraph {
        def iri: String = _iri + ".index"

        lazy val graph: MemGraph = self
        private val _thisgraph   = thisgraph
        lazy val index: MemIndexGraph = new MemIndexGraph {
          def iri: String = _iri + ".index" + ".index"

          lazy val graph: MemGraph      = _thisgraph
          lazy val index: MemIndexGraph = this
        }
      }
      init()
    }

    graph
  }

}

trait MemGraph extends Graph {

  def transaction: Transaction = MemTransaction(thisgraph)

  protected[mem] val nodeStore: NodeStore[this.type]   = MemNodeStore("@node", thisgraph)
  protected[mem] val edgeStore: EdgeStore[this.type]   = MemEdgeStore("@edge", thisgraph)
  protected[mem] val valueStore: ValueStore[this.type] = MemValueStore("@edge", thisgraph)

  protected[mem] val `@idStore`: ValueStore[this.type] =
    MemValueStore(NS.types.`@id`, thisgraph)

  override protected def _createNode(_id: Long)(ontology: Ontology*): _Node = {
    val node = new _Node with MemNode {
      implicit val graph: Graph = thisgraph
      val id: Long              = _id
    }
    ontology.foreach(node.addLabel)
    node
  }

  override protected def _createEdge[S, E](
      _id: Long)(_from: _Resource[S], _key: Property, _to: _Resource[E]): _Edge[S, E] = {
//    ns.getProperty(_key.iri).orElse(MemGraphDefault.ns.getProperty(_key.iri)).getOrElse {
//      if (!Graph.reservedKeys.contains(_key)) Property(ns.storeProperty(_key))
//    }

    new _Edge[S, E] with MemEdge[S, E] {
      val key: Property         = _key
      implicit val graph: Graph = thisgraph

      val outV: Resource[S] = _from
      val inV: Resource[E]  = _to

      val id: Long = _id
    }
  }

//  protected def _storeEdge(edge: _Edge[_, _]): Unit = {
//    edge.from
//      .asInstanceOf[MemResource[Any]]
//      .linksOut += edge.key -> (edge.from
//      .asInstanceOf[MemResource[Any]]
//      .linksOut
//      .getOrElse(edge.key, mutable.LinkedHashSet[Edge[Any, Any]]()) += edge)
//
//    edge.to.asInstanceOf[MemResource[Any]].linksIn += edge.key -> (edge.to
//      .asInstanceOf[MemResource[Any]]
//      .linksIn
//      .getOrElse(edge.key, mutable.LinkedHashSet[Edge[Any, Any]]()) += edge)
//    //      if (key != TYPE) p.inV.linksIn += p.key -> (p.inV.linksIn.getOrElse(p.key, List()) :+ p)
//
//    edgeStore.store(edge)
//  }

  protected def _createValue[T](_id: Long)(_value: T)(dt: DataType[T]): _Value[T] = synchronized {
//    println(s"memgraph._createValue ${_value}")
    //    val d = value match {
    //      case r: Resource[_] => throw new Exception("newValue only accepts literal-values and no resources")
    //      case _ => valueToDataType(value)
    //    }

    new _Value[T] with MemValue[T] {
      val value: T              = _value
      val label: DataType[T]    = dt
      implicit val graph: Graph = thisgraph

      val id: Long = _id
    }

    //    valueResource.property(valueResource.graph.createdonDateTime, new DataTypes.Date()) //self-recursive
  }

//  def getNodeById(id: Long): Option[Node]       = nodeStore.byId(id)
//  def getEdgeById(id: Long): Option[Edge[_, _]] = edgeStore.byId(id)
//  def getValueById(id: Long): Option[Value[_]]  = valueStores.byId(id)
//  def getResourceById(id: Long): Option[Resource[_]] =
//    getNodeById(id).orElse(getEdgeById(id)).orElse(getValueById(id))

  /**
    * delete in-/out-going edges from the resource
    * @param resource
    */
  protected def _deleteResource(resource: _Resource[_]): Unit = {
    resource.asInstanceOf[MemResource[Any]].linksOut.foreach {
      case (key, properties) =>
        properties.foreach(edge => edge.to.asInstanceOf[Resource[Any]].removeInE(edge.asInstanceOf[Edge[Any, Any]]))
    }
    resource.asInstanceOf[MemResource[Any]].linksIn.foreach {
      case (key, properties) =>
        properties.foreach(edge => edge.from.asInstanceOf[Resource[Any]].removeOutE(edge.asInstanceOf[Edge[Any, Any]]))
    }
  }

  val computer = DefaultStreamComputer()
  def buildTraversersStream[Start <: ClassType[_], End <: ClassType[_], Steps <: HList, Out](
      traversal: Traversal[Start, End, Steps])(ct: ClassType[_]): Stream[Out] =
    computer.traverse[Start, End, Steps, Out, this.type](traversal)(thisgraph)

  def buildAsyncTraversersStream[Start <: ClassType[_], End <: ClassType[_], Steps <: HList, Out](
      traversal: Traversal[Start, End, Steps])(ct: ClassType[_]): Task[Stream[Out]] =
    Task(computer.traverse[Start, End, Steps, Out, this.type](traversal)(thisgraph))

  def toFile(path: String = "defaultname.json", process: (Stream[Resource[_]], String => Unit) => String): Task[Unit] =
    Task {
      import java.io._

      // FileWriter
      val jsonfile = new File(path)
      val bw       = new BufferedWriter(new FileWriter(jsonfile))
      val context = process(nodes(), { value: String =>
        bw.write(value)
        bw.newLine()
      })
      bw.close()
      val contextfile = new File(path + ".context")
      val bwc         = new BufferedWriter(new FileWriter(contextfile))
      bwc.write(context)
      bwc.close()
    }
}
