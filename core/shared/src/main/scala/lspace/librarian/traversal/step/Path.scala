package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.provider.wrapped.WrappedNode
import lspace.structure._
import monix.eval.Task
import shapeless.HList

object Path
    extends StepDef("Path", "A path-step ..", () => MapStep.ontology :: Nil)
    with StepWrapper[Path[ClassType[Any], HList]] {

  def toStep(node: Node): Path[ClassType[Any], HList] = Path[ClassType[Any], HList](
    node
      .out(keys.byTraversal)
      .take(1)
      .map(
        Traversal
          .toTraversal(_)
          .asInstanceOf[Traversal[_ <: ClassType[_], _ <: ClassType[_], HList]])
      .head
  )

  object keys {
    object by
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Path/by",
          "by",
          "A traversal ..",
          `@range` = () => Traversal.ontology :: Nil
        )
    val byTraversal: TypedProperty[Node] = by.property + Traversal.ontology
  }
  override lazy val properties: List[Property] = keys.by :: MapStep.properties
  trait Properties extends MapStep.Properties {
    val by          = keys.by
    val byTraversal = keys.byTraversal
  }

  implicit def toNode[ET <: ClassType[_], Segments <: HList](step: Path[ET, Segments]): Task[Node] = {
    for {
      node      <- DetachedGraph.nodes.create(ontology)
      traversal <- step.by.toNode
      _         <- node.addOut(keys.byTraversal, traversal)
    } yield node
  }

}

case class Path[+ET <: ClassType[_], Segments <: HList](by: Traversal[_ <: ClassType[_], ET, Segments])
    extends MapStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = if (by.segmentList.nonEmpty) "path(" + by.toString + ")" else "path"
}
