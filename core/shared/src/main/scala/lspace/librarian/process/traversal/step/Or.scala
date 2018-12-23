package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import shapeless.{HList, HNil}

object Or
    extends StepDef("Or",
                    "An or-step traverser only survives if at least one of the n-traversals has a non-empty result.",
                    () => FilterStep.ontology :: Nil)
    with StepWrapper[Or] {

  def wrap(node: Node): Or = node match {
    case node: Or => node
    case _ =>
      Or(
        node
          .out(keys.traversalTraversal)
          .map(
            Traversal
              .wrap(_)(DetachedGraph)
              .asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]]),
        node
      )
  }

  object keys extends FilterStep.Properties {
    object traversal
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Or/traversal",
          "traversal",
          "A traversal ..",
          container = lspace.NS.types.`@list` :: Nil,
          `@range` = () => Traversal.ontology :: Nil
        )
    val traversalTraversal: TypedProperty[Node] = traversal.property + Traversal.ontology
  }
  override lazy val properties: List[Property] = keys.traversal :: FilterStep.properties
  trait Properties extends FilterStep.Properties {
    val traversal          = keys.traversal
    val traversalTraversal = keys.traversalTraversal
  }

  def apply(traversals: List[Traversal[_, _, _ <: HList]]): Or = {
    val node = DetachedGraph.nodes.create(ontology)

    traversals.map(_.self).foreach(node.addOut(keys.traversal, _))

    Or(traversals, node)
  }

}

case class Or private (traversals: List[Traversal[_, _, _ <: HList]], override val value: Node)
    extends WrappedNode(value)
    with FilterStep {
  override def prettyPrint: String =
    "or(" + traversals.map(_.toString).map("_." + _).mkString(", ") + ")"
}
