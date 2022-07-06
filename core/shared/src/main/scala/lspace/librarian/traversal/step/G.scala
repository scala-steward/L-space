package lspace.librarian.traversal.step

import lspace.datatype.{GraphType, ListType}
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object G
    extends StepDef("G", "A g-step selects graphs to traverse on.", GraphStep.ontology :: Nil)
    with StepWrapper[G] {

  def toStep(node: Node): Task[G] = Task.now(G(node.out(G.keys.graphGraph).take(1).flatten))

  object keys extends GraphStep.Properties {
    object graph
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/G/graph",
          "graph",
          "A graph to apply the upcoming traversal on",
          `@range` = ListType(GraphType.datatype) :: Nil
        )
    val graphGraph: TypedProperty[List[Graph]] = graph.property.as(ListType(GraphType.datatype))
  }
  override lazy val properties: List[Property] = keys.graph :: GraphStep.properties
  trait Properties extends GraphStep.Properties {
    val graph    = keys.graph
    val graphUrl = keys.graphGraph
  }

  implicit def toNode(g: G): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.graphGraph, g.graph)
    } yield node
  }.memoizeOnSuccess
}

case class G(graph: List[Graph]) extends GraphStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "g(" + graph.map(_.iri).mkString(", ") + ")"
}
