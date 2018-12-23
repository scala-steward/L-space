package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import shapeless.{HList, HNil}

object Where extends StepDef("Where", "A where-step ..", () => FilterStep.ontology :: Nil) with StepWrapper[Where] {

  def wrap(node: Node): Where = node match {
    case node: Where => node
    case _ =>
      Where(
        node
          .out(keys.traversalTraversal)
          .take(1)
          .map(
            Traversal
              .wrap(_)(DetachedGraph)
              .asInstanceOf[Traversal[_ <: ClassType[_], _ <: ClassType[_], HList]])
          .head,
        node
      )
  }

  object keys extends FilterStep.Properties {
    object traversal
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Where/traversal",
          "traversal",
          "A traversal which must have a non-empty result",
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

  def apply(traversal: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]): Where = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.traversalTraversal, traversal.self)
    Where(traversal, node)
  }
}

case class Where private (traversal: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList],
                          override val value: Node)
    extends WrappedNode(value)
    with FilterStep {
  override def prettyPrint: String = "where(_." + traversal.toString + ")"
}
