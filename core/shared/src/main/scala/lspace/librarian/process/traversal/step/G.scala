package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._
import lspace.librarian.datatype.{DataType, ListType}

object G
    extends StepDef("G", "A g-step selects graphs to traverse on.", () => GraphStep.ontology :: Nil)
    with StepWrapper[G] {

  def toStep(node: Node): G = G(node.out(G.keys.graphUrl).take(1).flatten)

  object keys extends GraphStep.Properties {
    object graph
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/G/Graphsource",
          "Graphsource",
          "A graph to apply the upcoming traversal on",
          `@range` = () => ListType(DataType.urlType[IriResource] :: Nil) :: Nil
        )
    val graphUrl: TypedProperty[List[IriResource]] = graph.property + ListType(DataType.urlType[IriResource] :: Nil)
  }
  override lazy val properties: List[Property] = keys.graph :: GraphStep.properties
  trait Properties extends GraphStep.Properties {
    val graph    = keys.graph
    val graphUrl = keys.graphUrl
  }

  implicit def toNode(g: G): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(keys.graphUrl, g.graphSource)
    node
  }
}

case class G(graphSource: List[IriResource]) extends GraphStep {

  def toNode: Node                 = this
  override def prettyPrint: String = "g(" + graphSource.map(_.iri).mkString(", ") + ")"
}
