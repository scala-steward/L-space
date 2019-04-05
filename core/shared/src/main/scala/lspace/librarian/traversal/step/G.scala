package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import lspace.datatype.{DataType, ListType}
import monix.eval.Task

object G
    extends StepDef("G", "A g-step selects graphs to traverse on.", () => GraphStep.ontology :: Nil)
    with StepWrapper[G] {

  def toStep(node: Node): Task[G] = Task.now(G(node.out(G.keys.graphUrl).take(1).flatten))

  object keys extends GraphStep.Properties {
    object graph
        extends PropertyDef(
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

  implicit def toNode(g: G): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.graphUrl, g.graphSource)
    } yield node
  }.memoizeOnSuccess
}

case class G(graphSource: List[IriResource]) extends GraphStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "g(" + graphSource.map(_.iri).mkString(", ") + ")"
}
