package lspace.librarian.process.traversal.step

import lspace.NS.types
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Is extends StepDef("Is") with StepWrapper[Is] {

  def wrap(node: Node): Is = node match {
    case node: Is => node
    case _        => Is(node.out(Is.keys.predicateUrl).map(P.wrap), node)
  }

  object keys {
    private val predicateNode = MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/step/Is/Predicate")
    predicateNode.addLabel(Property.ontology)
    predicateNode --- Property.default.`@label` --> "Predicate" --- Property.default.`@language` --> "en"
    predicateNode --- Property.default.`@comment` --> "A Predicate" --- Property.default.`@language` --> "en"
    predicateNode --- Property.default.`@container` --> types.`@list`
    predicateNode --- Property.default.`@range` --> P.ontology

    lazy val predicate: Property          = Property(predicateNode)
    val predicateUrl: TypedProperty[Node] = predicate + P.ontology
  }

  def apply(predicates: List[P[_]]): Is = {
    val node = DetachedGraph.nodes.create(ontology)

    predicates.foreach(node.addOut(keys.predicate, _))

    Is(predicates, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.predicate
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Is private (predicate: List[P[_]], override val value: Node) extends WrappedNode(value) with FilterStep {
  override def prettyPrint: String = "is(P." + predicate.head.prettyPrint + ")"
}
