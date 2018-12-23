package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object HasValue
    extends StepDef("HasValue",
                    "A hasValue-step is successful if the resources satisfies certain predicates.",
                    () => HasStep.ontology :: Nil)
    with StepWrapper[HasValue] {

  def wrap(node: Node): HasValue = node match {
    case node: HasValue => node
    case _              => HasValue(node.out(Has.keys.predicateUrl).map(P.wrap), node)
  }

  object keys {
    val predicate = Has.keys.predicate
  }
  override lazy val properties: List[Property] = keys.predicate :: HasStep.properties
  trait Properties extends HasStep.Properties {
    val predicate = keys.predicate
  }

  def apply(predicates: List[P[_]]): HasValue = {
    val node = DetachedGraph.nodes.create(ontology)

    predicates.foreach(node.addOut(Has.keys.predicate, _))

    HasValue(predicates, node)
  }
}

case class HasValue private (predicate: List[P[_]], override val value: Node) extends WrappedNode(value) with HasStep {}
