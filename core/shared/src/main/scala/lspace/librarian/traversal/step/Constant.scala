package lspace.librarian.traversal.step

import lspace.librarian.traversal.{FilterStep, MoveStep, StepDef, StepWrapper, TraverseStep}
import lspace.provider.detached.DetachedGraph
import lspace.structure.util.ClassTypeable
import lspace.structure.{ClassType, Node, Property, PropertyDef}
import monix.eval.Task

object Constant
    extends StepDef("Constant",
                    "A constant-step sets a strict value for the traverser.",
                    () => TraverseStep.ontology :: Nil)
    with StepWrapper[Constant[Any]] {

  def toStep(node: Node): Task[Constant[Any]] = {
    val value = node.out(Constant.keys.value).head
    Task.now(new Constant(value)(ClassType.valueToOntologyResource(value)))
  }

  object keys extends TraverseStep.Properties {
    object value
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Constant/value",
          "value",
          "A strict value"
        )
  }
  override lazy val properties: List[Property] = keys.value.property :: TraverseStep.properties

  trait Properties extends TraverseStep.Properties {
    val value = keys.value
  }

  implicit def toNode[T](step: Constant[T]): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.value, step.label, step.value)
    } yield node
  }.memoizeOnSuccess

}

case class Constant[T](value: T)(val label: ClassType[T]) extends TraverseStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "constant(" + value + ")"
}
