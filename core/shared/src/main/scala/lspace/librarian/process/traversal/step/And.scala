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

  def toStep(node: Node): And = node match {
    case node: And => node
    case _ =>
      And(
        node
          .out(keys.traversalTraversal)
          .map(
            Traversal
              .toTraversal(_)(DetachedGraph)
              .asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
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

  implicit def toNode(and: And): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    and.traversals.map(_.toNode).foreach(node.addOut(keys.traversal, _))
    node
  }
}

case class And(traversals: List[Traversal[_, _, _ <: HList]]) extends FilterStep {

  lazy val toNode: Node = this
  override def prettyPrint: String =
    "and(" + traversals.map(_.toString).map("_." + _).mkString(", ") + ")"
}
