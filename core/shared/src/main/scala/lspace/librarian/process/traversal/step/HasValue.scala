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

  def toStep(node: Node): HasValue = HasValue(node.out(Has.keys.predicateUrl).map(P.toNode))

  object keys {
    val predicate = Has.keys.predicate
  }
  override lazy val properties: List[Property] = keys.predicate :: HasStep.properties
  trait Properties extends HasStep.Properties {
    val predicate = keys.predicate
  }

  implicit def toNode(hasValue: HasValue): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    hasValue.predicate.map(_.toNode).foreach(node.addOut(Has.keys.predicate, _))
    node
  }
}

case class HasValue(predicate: List[P[_]]) extends HasStep {

  lazy val toNode: Node = this
  override def prettyPrint: String =
    if (predicate.nonEmpty) s"hasValue(P.${predicate.head.prettyPrint})"
    else "hasValue(" + predicate.map(_.prettyPrint).mkString(", ") + ")"
}
