package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import shapeless.{HList, HNil}

object And
    extends StepDef("And",
                    "An and-step traverser only survives if all n-traversals have a non-empty result.",
                    () => FilterStep.ontology :: Nil)
    with StepWrapper[And] {

  def wrap(node: Node): And = node match {
    case node: And => node
    case _ =>
      And(
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
          lspace.NS.vocab.Lspace + "librarian/step/And/traversal",
          "traversal",
          "A traversal which must have a non-empty result",
          container = lspace.NS.types.`@list` :: Nil,
          `@range` = () => Traversal.ontology :: Nil
        )
    val traversalTraversal: TypedProperty[Node] = traversal.property + Traversal.ontology
  }
  override lazy val properties: List[Property] = keys.traversal :: FilterStep.properties

  trait Properties extends FilterStep.Properties {
    lazy val `ns.l-space.eu/librarian/step/And/traversal`: Property                  = keys.traversal
    lazy val `ns.l-space.eu/librarian/step/And/traversal @Traversal`: TypedKey[Node] = keys.traversalTraversal
  }

  def apply(traversals: List[Traversal[_, _, _ <: HList]]): And = {
    val node = DetachedGraph.nodes.create(ontology)

    traversals.map(_.self).foreach(node.addOut(keys.traversal, _))
    And(traversals, node)
  }
}

case class And private (traversals: List[Traversal[_, _, _ <: HList]], override val value: Node)
    extends WrappedNode(value)
    with FilterStep {
  override def prettyPrint: String =
    "and(" + traversals.map(_.toString).map("_." + _).mkString(", ") + ")"
}
