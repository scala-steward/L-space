package lspace.librarian.provider.mem

import java.util.concurrent.atomic.AtomicLong

import monix.eval.Task
import lspace.librarian.process.computer.DefaultStreamComputer
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._
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
      val iri: String  = _iri
      private val self = this
      val ns: MemNSGraph = new MemNSGraph {
        def iri: String     = _iri + ".ns"
        val graph: MemGraph = self
      }
      init()
    }

    graph
  }

}

trait MemDataGraph extends MemGraph with DataGraph {

  def init(): Unit = {}
}

trait MemNSGraph extends MemGraph with NameSpaceGraph {
  def graph: MemGraph

  def init(): Unit = {}
}

trait MemGraph extends Graph {
  protected[mem] lazy val IdGenerator = new {
    private val id = new AtomicLong()
    def next: Long = id.incrementAndGet
  }

  protected[librarian] lazy val edgeCache: mutable.OpenHashMap[Long, MemEdge[_, _]] =
    mutable.OpenHashMap[Long, MemEdge[_, _]]()
  protected[librarian] lazy val nodeCache: mutable.OpenHashMap[Long, MemNode] =
    mutable.OpenHashMap[Long, MemNode]()
  protected[librarian] lazy val valueCache: mutable.OpenHashMap[Long, MemValue[_]] =
    mutable.OpenHashMap[Long, MemValue[_]]()
  protected[librarian] lazy val valueIndex: mutable.OpenHashMap[(DataType[_], Any), MemValue[_]] =
    mutable.OpenHashMap[(DataType[_], Any), MemValue[_]]()
  protected[librarian] lazy val iriIndex: mutable.OpenHashMap[String, MemValue[_]] =
    mutable.OpenHashMap[String, MemValue[_]]()

  def links: Stream[MemEdge[_, _]] = edgeCache.toStream.map(_._2)
  def nodes: Stream[MemNode]       = nodeCache.toStream.map(_._2)
  def values: Stream[MemValue[_]]  = valueCache.toStream.map(_._2)

  //  def newClassNode(ontology: Ontology*): MemNode = {
  //    val node: MemNode = MemNode.apply
  //
  //    if (ontology.nonEmpty) ontology.foreach(node.addLabel)
  //
  //    node
  //  }
  protected def _createNode(ontology: Ontology*): MemNode = {
    val node: MemNode = MemNode.apply
    nodeCache += node.id -> node
    ontology.foreach(node.addLabel)
    node
  }

  protected def _createEdge[S, E](from: Resource[S], key: Property, to: Resource[E]): MemEdge[S, E] = {
    ns.getProperty(key.iri).orElse(MemGraphDefault.ns.getProperty(key.iri)).getOrElse {
      if (!Graph.reservedKeys.contains(key)) Property(ns.storeProperty(key))
    }

    val f =
      if (from.graph == thisgraph) from.asInstanceOf[MemResource[S]]
      else if (from.iri.nonEmpty)
        getResource(from.iri).headOption
          .getOrElse(upsertResource(from))
          .asInstanceOf[MemResource[S]]
      else throw new Exception("")
    val k = key
    val t =
      if (to.graph == thisgraph) to.asInstanceOf[MemResource[E]]
      else if (to.iri.nonEmpty)
        getResource(to.iri).headOption.getOrElse(upsertResource(to)).asInstanceOf[MemResource[E]]
      else throw new Exception("")
    val p: MemEdge[S, E] = MemEdge(f, k, t)

    p.outV.linksOut += p.key -> (p.outV.linksOut.getOrElse(p.key, mutable.LinkedHashSet()) += p)

    if (thisgraph != DetachedGraph) {
      p.graph.edgeCache += p.id -> p
      p.inV.linksIn += p.key    -> (p.inV.linksIn.getOrElse(p.key, mutable.LinkedHashSet()) += p)
      //      if (key != TYPE) p.inV.linksIn += p.key -> (p.inV.linksIn.getOrElse(p.key, List()) :+ p)
    }

    //    p.linksOut += p.graph.TYPE -> mutable.LinkedHashSet(MemEdge(
    //      p,
    //      TYPE,
    //      if (thisgraph.iri != "detachedmemgraph") k.value.asInstanceOf[MemResource[Node]]
    //      else thisgraph.getPropertyKey(k.iri).get.value.asInstanceOf[MemResource[Node]])(thisgraph))

    if (key == Property.default.iri) {
      val r = iriIndex.getOrElse(to.value.asInstanceOf[String], p.to.asInstanceOf[MemValue[String]])
      if (r.id != to.id) throw new Exception(s"duplicate MemValues? ${to.value}")
      iriIndex += to.value.asInstanceOf[String] -> r
    } else if (key == Property.default.iris) {
      val r = iriIndex.getOrElse(to.value.asInstanceOf[String], p.to.asInstanceOf[MemValue[String]])
      iriIndex += to.value.asInstanceOf[String] -> r
    }

    p
  }

  protected def _createValue[T](value: T)(dt: DataType[T]): MemValue[T] = synchronized {
    //    val d = value match {
    //      case r: Resource[_] => throw new Exception("newValue only accepts literal-values and no resources")
    //      case _ => valueToDataType(value)
    //    }
    val valueResource: MemValue[T] =
      valueIndex.get(dt -> value).map(_.asInstanceOf[MemValue[T]]).getOrElse {
        val v: MemValue[T] = MemValue[T](value, dt)
        //      v.linksOut += TYPE -> mutable.LinkedHashSet(
        //        newEdge(v, TYPE, d.value.asInstanceOf[MemNode]))
        if (thisgraph != DetachedGraph) {
          valueCache += v.id -> v
          valueIndex += (dt  -> value) -> v
        }
        v
      }

    //    valueResource.property(valueResource.graph.createdonDateTime, new DataTypes.Date()) //self-recursive
    valueResource
  }

  override def getNode(iri: String): List[Node] = {
    iriIndex
      .get(iri)
      .map(
        _.in(Property.default.iri, Property.default.iris).toSet
          .filter(_.isInstanceOf[Node])
          .map(_.asInstanceOf[Node])
          .toList)
      .getOrElse(List())
  }

  override def getNodes(iris: Set[String]): List[Node] = {
    val validIris = iris.filter(_.nonEmpty)
    if (validIris.nonEmpty)
      validIris
        .flatMap(
          iri =>
            iriIndex
              .get(iri)
              .map(
                _.in(Property.default.iri, Property.default.iris).toSet
                  .filter(_.isInstanceOf[Node])
                  .map(_.asInstanceOf[Node])
                  .toList)
              .getOrElse(List()))
        .toList
    else List[Node]()
  }
  override def getEdges(iris: Set[String]): List[Edge[_, _]] = {
    val validIris = iris.filter(_.nonEmpty)
    if (validIris.nonEmpty)
      validIris
        .flatMap(
          iri =>
            iriIndex
              .get(iri)
              .map(
                _.in(Property.default.iri, Property.default.iris).toSet
                  .filter(_.isInstanceOf[Edge[_, _]])
                  .map(_.asInstanceOf[Edge[_, _]])
                  .toList)
              .getOrElse(List()))
        .toList
    else List[Edge[_, _]]()
  }
  override def getValues(valueSet: Set[(DataType[_], Any)]): List[Value[_]] = {
    valueSet.flatMap(value => valueIndex.get(value)).toList
  }
  override def getResources(iris: Set[String]): List[Resource[_]] = {
    val validIris = iris.filter(_.nonEmpty)
    if (validIris.nonEmpty) {
      validIris
        .flatMap(
          iri =>
            iriIndex
              .get(iri)
              .map(
                _.in(Property.default.iri, Property.default.iris).toSet
                  .filter(_.isInstanceOf[Resource[_]])
                  .map(_.asInstanceOf[Resource[_]])
                  .toList)
              .getOrElse(List()))
        .toList
    } else List[Resource[_]]()
  }

  def getNodeById(id: Long): Option[Node]       = nodeCache.get(id)
  def getEdgeById(id: Long): Option[Edge[_, _]] = edgeCache.get(id)
  def getValueById(id: Long): Option[Value[_]]  = valueCache.get(id)
  def getResourceById(id: Long): Option[Resource[_]] =
    nodeCache.get(id).orElse(edgeCache.get(id)).orElse(valueCache.get(id))

  protected def _deleteNode(node: Node): Unit = {
    node match {
      case node: MemNode if node.graph == this =>
        node._remove()
        nodeCache -= node.id
      case node if node.iri.nonEmpty => getNode(node.iri).foreach(_.remove())
    }
  }

  protected def _deleteEdge(edge: Edge[_, _]): Unit = {
    edge match {
      case edge: MemEdge[_, _] if edge.graph == this =>
        edge._remove()
        edgeCache -= edge.id
      case _ => //Cannot remove foreign edge
    }
  }

  protected def _deleteValue(value: Value[_]): Unit = {
    value match {
      case value: MemNode if value.graph == this =>
        value._remove()
        valueCache -= value.id
      case value =>
        try {
          getValue(value)(ClassType.valueToOntologyResource(value)).foreach(_.remove())
        } catch {
          case e: Throwable => //???
        }
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
      val context = process(nodes, { value: String =>
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
