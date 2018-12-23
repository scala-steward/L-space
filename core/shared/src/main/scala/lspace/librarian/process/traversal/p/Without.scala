package lspace.librarian.process.traversal.p

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._

object Without
    extends PredicateDef("Without", `@extends` = () => List(CollectionP.ontology))
    with PredicateWrapper[Without[_]] {

  def wrap(node: Node): Without[_] = node match {
    case node: Without[_] => node
    case _                => new Without(node.out(EqP.keys.value), node)
  }

  object keys extends CollectionP.Properties
  override lazy val properties: List[Property] = CollectionP.properties
  trait Properties extends CollectionP.Properties

  def apply[T](pvalues: List[T]): Without[T] = {
    val node = DetachedGraph.nodes.create(ontology)

    pvalues.foreach(pvalue => node.addOut(EqP.keys.value, ClassType.valueToOntologyResource(pvalue), pvalue))
    new Without(pvalues, node)
  }
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
