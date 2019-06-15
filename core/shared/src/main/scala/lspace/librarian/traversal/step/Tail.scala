package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.datatype.DataType
import lspace.structure._
import monix.eval.Task

object Tail
    extends StepDef("Tail", "A tail-step limits the traversal to last n-results.", () => ClipStep.ontology :: Nil)
    with StepWrapper[Tail] {

  def toStep(node: Node): Task[Tail] = Task.now(Tail(node.out(Tail.keys.maxInt).head))

  object keys extends ClipStep.Properties {
    object max
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Tail/max",
          "max",
          "The maximum number of tail-results",
          `@range` = () => DataType.default.`@string` :: Nil
        )
    val maxInt: TypedProperty[Int] = max.property as DataType.default.`@int`
  }
  override lazy val properties: List[Property] = keys.max :: ClipStep.properties
  trait Properties extends ClipStep.Properties {
    val max    = keys.max
    val maxInt = keys.maxInt
  }

  implicit def toNode(step: Tail): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.maxInt, step.max)
    } yield node
  }.memoizeOnSuccess
}

case class Tail(max: Int) extends ClipStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"tail($max)"
}
