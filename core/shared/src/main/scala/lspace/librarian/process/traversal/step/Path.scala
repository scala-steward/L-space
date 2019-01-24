package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import shapeless.HList

object Path extends StepDef("Path", "A path-step ..", () => MapStep.ontology :: Nil) with StepWrapper[Path] {

  def toStep(node: Node): Path = Path(
    node
      .out(keys.byTraversal)
      .take(1)
      .map(
        Traversal
          .toTraversal(_)(DetachedGraph)
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

  implicit def toNode(path: Path): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(keys.by, path.by.toNode)
    node
  }

}

case class Path(by: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]) extends MapStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = if (by.segmentList.nonEmpty) "path(" + by.toString + ")" else "path"
}
