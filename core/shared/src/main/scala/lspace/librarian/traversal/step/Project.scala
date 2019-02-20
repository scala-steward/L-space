package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.provider.wrapped.WrappedNode
import lspace.structure._
import lspace.NS.types
import shapeless.HList

object Project
    extends StepDef("Project", "A project-step ..", () => TraverseStep.ontology :: Nil)
    with StepWrapper[Project] {

  def toStep(node: Node): Project =
    Project(
      node
        .out(Project.keys.byTraversal)
        .map(Traversal.toTraversal(_)))

  object keys extends TraverseStep.Properties {
    object by
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Project/by",
          "by",
          "A traversal ..",
          container = lspace.NS.types.`@list` :: Nil,
          `@range` = () => Traversal.ontology :: Nil
        )
    val byTraversal: TypedProperty[Node] = by.property + Traversal.ontology
  }
  override lazy val properties: List[Property] = keys.by :: TraverseStep.properties
  trait Properties extends TraverseStep.Properties

  implicit def toNode(project: Project): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    project.by.map(_.toNode).foreach(node.addOut(keys.by, _))
    node
  }

}

case class Project(by: List[Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]]) extends TraverseStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "project(" + by.map(_.toString).map("_." + _).mkString(", ") + ")"
}
