package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import shapeless.{HList, HNil}

object Not extends StepDef("Not", "A not-step ..", () => FilterStep.ontology :: Nil) with StepWrapper[Not] {

  def wrap(node: Node): Not = node match {
    case node: Not => node
    case _ =>
      Not(
        node
          .out(keys.traversalTraversal)
          .take(1)
          .map(
            Traversal
              .wrap(_)(DetachedGraph)
              .asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
          .head,
        node
      )
  }

  object keys extends FilterStep.Properties {
    object traversal
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Not/traversal",
          "traversal",
          "A traversal which must have a empty result",
          `@range` = () => Traversal.ontology :: Nil
        )
    val traversalTraversal: TypedProperty[Node] = traversal.property + Traversal.ontology
  }
  override lazy val properties: List[Property] = keys.traversal :: FilterStep.properties
  trait Properties extends FilterStep.Properties {
    val traversal          = keys.traversal
    val traversalTraversal = keys.traversalTraversal
  }

  def apply(traversal: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]): Not = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.traversal, traversal.self)
    Not(traversal, node)
  }

}

case class Not private (traversal: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList],
                        override val value: Node)
    extends WrappedNode(value)
    with FilterStep {
  override def prettyPrint: String = "not(_." + traversal.toString + ")"
}
