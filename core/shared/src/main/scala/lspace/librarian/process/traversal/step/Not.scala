package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import shapeless.{HList, HNil}

object Not extends StepDef("Not", "A not-step ..", () => FilterStep.ontology :: Nil) with StepWrapper[Not] {

  def toStep(node: Node): Not = Not(
    node
      .out(keys.traversalTraversal)
      .take(1)
      .map(
        Traversal
          .toTraversal(_)(DetachedGraph)
          .asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
      .head
  )

  object keys extends FilterStep.Properties {
    object traversal
        extends PropertyDef(
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

  implicit def toNode(not: Not): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(keys.traversalTraversal, not.traversal.toNode)
    node
  }

}

case class Not(traversal: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]) extends FilterStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "not(_." + traversal.toString + ")"
}
