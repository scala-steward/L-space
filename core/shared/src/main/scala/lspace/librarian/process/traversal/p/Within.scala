package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Within extends PredicateCompanion("Within") with PredicateWrapper[Within[_]] {
  ontologyNode --- Property.default.EXTENDS --> CollectionP.ontology

  def wrap(node: Node): Within[_] = node match {
    case node: Within[_] => node
    case _               => new Within(node.out(EqP.keys.value), node)
  }

  def apply[T](pvalues: List[T]): Within[T] = {
    val node = DetachedGraph.createNode(ontology)

    pvalues.foreach(pvalue => node.addOut(EqP.keys.value, ClassType.valueToOntologyResource(pvalue), pvalue))

    new Within(pvalues, node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

class Within[T] private (val pvalues: List[T], override val value: Node)
    extends WrappedNode(value)
    with CollectionP[T] {
  def assert(avalue: Any): Boolean = {
    avalue match {
      case avalue: Resource[_] =>
        pvalues.exists {
          case pvalue: Resource[_] => pvalue.iri == avalue.iri
          case _                   => false
        }
      case _ => pvalues.contains(avalue)
    }
  }

  override def prettyPrint: String = s"within(${pvalues.mkString(", ")})"
}
