package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.provider.wrapped.WrappedNode
import lspace.structure._
import monix.eval.Task
import shapeless.{HList, HNil}

object Not extends StepDef("Not", "A not-step ..", () => FilterStep.ontology :: Nil) with StepWrapper[Not] {

  def toStep(node: Node): Task[Not] =
    for {
      traversal <- node
        .out(keys.traversalTraversal)
        .take(1)
        .map(
          Traversal
            .toTraversal(_)
            .map(_.asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]]))
        .head
    } yield Not(traversal)

  object keys extends FilterStep.Properties {
    object traversal
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Not/traversal",
          "traversal",
          "A traversal which must have a empty result",
          `@range` = () => Traversal.ontology :: Nil
        )
    val traversalTraversal: TypedProperty[Node] = traversal.property as Traversal.ontology
  }
  override lazy val properties: List[Property] = keys.traversal :: FilterStep.properties
  trait Properties extends FilterStep.Properties {
    val traversal          = keys.traversal
    val traversalTraversal = keys.traversalTraversal
  }

  implicit def toNode(step: Not): Task[Node] = {
    for {
      node      <- DetachedGraph.nodes.create(ontology)
      traversal <- step.traversal.toNode
      _         <- node.addOut(keys.traversalTraversal, traversal)
    } yield node
  }.memoizeOnSuccess

}

case class Not(traversal: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]) extends FilterStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "not(_." + traversal.toString + ")"
}
