package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault

object G
    extends StepDef("G", "A g-step selects graphs to traverse on.", () => TraverseStep.ontology :: Nil)
    with StepWrapper[G] {

  def toStep(node: Node): G = G(node.out(G.keys.graphUrl))

  object keys extends TraverseStep.Properties {
    object graph
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/G/Graphsource",
          "Graphsource",
          "A graph to apply the upcoming traversal on",
          container = types.`@list` :: Nil,
          `@range` = () => DataType.urlType[IriResource] :: Nil
        )
    val graphUrl: TypedProperty[IriResource] = graph.property + DataType.urlType[IriResource]
  }
  override lazy val properties: List[Property] = keys.graph :: TraverseStep.properties
  trait Properties extends TraverseStep.Properties {
    val graph    = keys.graph
    val graphUrl = keys.graphUrl
  }

  implicit def toNode(g: G): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    g.graphSource.foreach(graph => node.addOut(keys.graphUrl, graph))
    node
  }
}

case class G(graphSource: List[IriResource]) extends TraverseStep {

  def toNode: Node                 = this
  override def prettyPrint: String = "g(" + graphSource.map(_.iri).mkString(", ") + ")"
}
