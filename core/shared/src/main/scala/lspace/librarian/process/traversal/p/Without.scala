package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Without extends PredicateCompanion("Without") with PredicateWrapper[Without[_]] {
  ontologyNode --- Property.default.`@extends` --> CollectionP.ontology

  def wrap(node: Node): Without[_] = node match {
    case node: Without[_] => node
    case _                => new Without(node.out(EqP.keys.value), node)
  }

  def apply[T](pvalues: List[T]): Without[T] = {
    val node = DetachedGraph.nodes.create(ontology)

    pvalues.foreach(pvalue => node.addOut(EqP.keys.value, ClassType.valueToOntologyResource(pvalue), pvalue))
    new Without(pvalues, node)
  }

  //  MemGraphDefault.ns.storeOntology(ontology)
}

class Without[T] private (val pvalues: List[T], override val value: Node)
    extends WrappedNode(value)
    with CollectionP[T] {
  def assert(avalue: Any): Boolean = ! {
    avalue match {
      case avalue: Resource[_] =>
        pvalues.exists {
          case pvalue: Resource[_] => pvalue.iri == avalue.iri
          case _                   => false
        }
      case _ => pvalues.contains(avalue)
    }
  }

  override def prettyPrint: String = s"without(${pvalues.mkString(", ")})"
}
