package lspace.librarian.traversal.step

import lspace.librarian.traversal.{FilterStep, MoveStep, StepDef, StepWrapper}
import lspace.provider.detached.DetachedGraph
import lspace.structure.util.ClassTypeable
import lspace.structure.{ClassType, Node, Property, PropertyDef}
import monix.eval.Task

object Constant
    extends StepDef("Constant",
                    "A constant-step sets a strict value for the traverser.",
                    () => MoveStep.ontology :: Nil)
    with StepWrapper[Constant[Any, Any, ClassType[Any]]] {

  def toStep(node: Node): Task[Constant[Any, Any, ClassType[Any]]] =
    Task.now(new Constant(node.out(Constant.keys.value).head))

  object keys extends MoveStep.Properties {
    object value
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Constant/value",
          "value",
          "A strict value"
        )
  }
  override lazy val properties: List[Property] = keys.value.property :: MoveStep.properties

  trait Properties extends MoveStep.Properties {
    val value = keys.value
  }

  implicit def toNode[T, T0, TT0 <: ClassType[_]](step: Constant[T, T0, TT0]): Task[Node] = {
    import step.ct
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.value, step.value)
    } yield node
  }.memoizeOnSuccess

}

case class Constant[T, T0, TT0 <: ClassType[_]](value: T)(implicit val ct: ClassTypeable.Aux[T, T0, TT0])
    extends MoveStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "constant(" + value + ")"
}
