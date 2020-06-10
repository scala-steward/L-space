package lspace.librarian.traversal.step

import lspace.datatype.DataType
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

case class Skip(n: Int) extends ClipStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "skip(" + n + ")"
}

object Skip
    extends StepDef("Skip", "A skip-step skips the first n-results.", ClipStep.ontology :: Nil)
    with StepWrapper[Skip] {

  def toStep(node: Node): Task[Skip] = Task.now(Skip(node.out(Skip.keys.nInt).head))

  object keys extends ClipStep.Properties {
    object n
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Skip/n",
          "n",
          "The number of first-results to skip",
          `@range` = DataType.default.`@string` :: Nil
        )
    val nInt: TypedProperty[Int] = n.property as DataType.default.`@int`
  }
  override lazy val properties: List[Property] = keys.n :: ClipStep.properties
  trait Properties extends ClipStep.Properties {
    val n    = keys.n
    val nInt = keys.nInt
  }

  implicit def toNode(skip: Skip): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- node.addOut(keys.nInt, skip.n)
    } yield node
  }.memoizeOnSuccess

}
