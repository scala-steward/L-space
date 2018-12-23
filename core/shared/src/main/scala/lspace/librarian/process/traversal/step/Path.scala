package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import shapeless.HList

object Path extends StepDef("Path", "A path-step ..", () => MapStep.ontology :: Nil) with StepWrapper[Path] {

  def wrap(node: Node): Path = node match {
    case node: Path => node
    case _ =>
      Path(
        node
          .out(keys.byTraversal)
          .take(1)
          .map(
            Traversal
              .wrap(_)(DetachedGraph)
              .asInstanceOf[Traversal[_ <: ClassType[_], _ <: ClassType[_], HList]])
          .head,
        node
      )
  }

  object keys {
    object by
        extends Property.PropertyDef(
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

  def apply(by: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]): Path = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.by, by.self)
    Path(by, node)
  }

}

case class Path private (by: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList], override val value: Node)
    extends WrappedNode(value)
    with MapStep {
  override def prettyPrint: String = if (by.stepsList.nonEmpty) "path(" + by.toString + ")" else "path"
}
