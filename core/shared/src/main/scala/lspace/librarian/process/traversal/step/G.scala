package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault

object G extends StepDef("G") with StepWrapper[G] {

  def wrap(node: Node): G = node match {
    case node: G => node
    case _       => G(node)(node.out(G.keys.graphUrl))
  }

  object keys {
    private val graphNode = MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/step/g/Graphsource")
    graphNode.addLabel(Property.ontology)
    graphNode --- Property.default.`@label` --> "Graphsource" --- Property.default.`@language` --> "en"
    graphNode --- Property.default.`@comment` --> "An edge" --- Property.default.`@language` --> "en"
    graphNode --- Property.default.`@container` --> types.`@list`
    graphNode --- Property.default.`@range` --> DataType.urlType[IriResource]

    lazy val graph: Property                 = Property(graphNode)
    val graphUrl: TypedProperty[IriResource] = graph + DataType.urlType[IriResource]
  }

  def apply(graphSources: List[Graph]): G = {
    val node = DetachedGraph.nodes.create(ontology)

    graphSources.foreach(graph => node.addOut(keys.graphUrl, graph))

    G(node)(graphSources)
  }

  ontologyNode --- Property.default.`@properties` --> keys.graph
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class G(override val value: Node)(val graphSource: List[IriResource])
    extends WrappedNode(value)
    with TraverseStep {
  override def prettyPrint: String = "g(" + graphSource.map(_.iri).mkString(", ") + ")"
}
