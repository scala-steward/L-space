package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object HasValue extends StepDef("HasValue") with StepWrapper[HasValue] {

  def wrap(node: Node): HasValue = node match {
    case node: HasValue => node
    case _              => HasValue(node.out(Has.keys.predicateUrl).map(P.wrap), node)
  }

  def apply(predicates: List[P[_]]): HasValue = {
    val node = DetachedGraph.nodes.create(ontology)

    predicates.foreach(node.addOut(Has.keys.predicate, _))

    HasValue(predicates, node)
  }

  ontologyNode --- Property.default.`@properties` --> Has.keys.predicate
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class HasValue private (predicate: List[P[_]], override val value: Node) extends WrappedNode(value) with HasStep {}
