package lspace.librarian.traversal.step

import lspace.datatype.DataType
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object Limit
    extends StepDef("Limit", "A limit-step limits the traversal to first n-results.", ClipStep.ontology :: Nil)
    with StepWrapper[Limit] {

  def toStep(node: Node): Task[Limit] = Task.now(Limit(node.out(Limit.keys.maxInt).head))

  object keys extends ClipStep.Properties {
    object max
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Limit/max",
          "max",
          "The maximum number of results",
          `@range` = DataType.default.`@string` :: Nil
        )
    val maxInt: TypedProperty[Int] = max.property.as(DataType.default.`@int`)
  }
  override lazy val properties: List[Property] = keys.max :: ClipStep.properties
  trait Properties extends ClipStep.Properties {
    val max    = keys.max
    val maxInt = keys.maxInt
  }

  implicit def toNode(limit: Limit): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.maxInt, limit.max)
    } yield node
  }.memoizeOnSuccess

}

case class Limit(max: Int) extends ClipStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "limit(" + max + ")"
}
