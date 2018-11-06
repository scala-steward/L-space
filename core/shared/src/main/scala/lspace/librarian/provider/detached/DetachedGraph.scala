package lspace.librarian.provider.detached

import lspace.librarian.process.computer.DefaultStreamComputer
import lspace.librarian.process.traversal._
import lspace.librarian.process.traversal.step._
import lspace.librarian.provider.mem._
import lspace.librarian.provider.mem.MemNode
import lspace.librarian.structure._

import scala.collection.mutable

object DetachedGraph extends MemDataGraph {
  lazy val iri: String = "detachedmemgraph"

  val ns: MemNSGraph = MemGraphDefault.ns

  override protected def _createNode(ontology: Ontology*): MemNode = {
    val node: MemNode = MemNode.apply
    ontology.foreach(node.addLabel)
    node
  }

  override protected def _createEdge[S, E](from: Resource[S], key: Property, to: Resource[E]): MemEdge[S, E] = {
    val f =
      if (from.graph == thisgraph || from.graph == MemGraphDefault)
        from.asInstanceOf[MemResource[S]]
      else if (from.iri.nonEmpty) upsertNode(from.iri).asInstanceOf[MemResource[S]]
      else throw new Exception("")
    val k = key
    val t =
      if (to.graph == thisgraph || to.graph == MemGraphDefault) to.asInstanceOf[MemResource[E]]
      else if (to.iri.nonEmpty) upsertNode(to.iri).asInstanceOf[MemResource[E]]
      else throw new Exception("")
    val p: MemEdge[S, E] = MemEdge(f, k, t)

    p.outV.linksOut += p.key -> (p.outV.linksOut.getOrElse(p.key, mutable.LinkedHashSet()) += p)

    //    p.linksOut += p.graph.TYPE -> mutable.LinkedHashSet(MemEdge(
    //      p,
    //      TYPE,
    //      thisgraph.getPropertyKey(k.iri).get.value.asInstanceOf[MemResource[Node]])(thisgraph))
    p
  }

  override protected def _createValue[T](value: T)(dt: DataType[T]): MemValue[T] = {
    //    val d = value match {
    //      case r: Resource[_] => throw new Exception("newValue only accepts literal-values and no resources")
    //      case _ => valueToDataType(value)
    //    }
    val v: MemValue[T] = MemValue[T](value, dt)
    //    v.linksOut += TYPE -> mutable.LinkedHashSet(
    //      newEdge(v, TYPE, d.value.asInstanceOf[MemNode]))
    v
  }

  override def upsertNode(uri: String, uris: Set[String] = Set()): Node = {
    val node = createNode()
    node.addOut(Property.default.typed.iriUrlString, uri)
    //    node.property(MemGraphDefault.createdonDateTime, Instant.now())
    node
  }

  override val computer = new DefaultStreamComputer()

}
