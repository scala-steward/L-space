package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import shapeless.HList

object Project
    extends StepDef("Project", "A project-step ..", () => Terminate.ontology :: Nil)
    with StepWrapper[Project] {

  def wrap(node: Node): Project = node match {
    case node: Project => node
    case _ =>
      new Project(node
                    .out(Project.keys.byTraversal)
                    .map(Traversal.wrap(_)(DetachedGraph)),
                  node)
  }

  object keys extends Terminate.Properties {
    object by
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Project/by",
          "by",
          "A traversal ..",
          container = lspace.NS.types.`@list` :: Nil,
          `@range` = () => Traversal.ontology :: Nil
        )
    val byTraversal: TypedProperty[Node] = by.property + Traversal.ontology
  }
  override lazy val properties: List[Property] = keys.by :: Terminate.properties
  trait Properties extends Terminate.Properties

  def apply(traversals: List[Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]]): Project = {
    val node = DetachedGraph.nodes.create(ontology)

    traversals.map(_.self).foreach(node.addOut(keys.by, _))
    Project(traversals, node)
  }

}

case class Project private (by: List[Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]],
                            override val value: Node)
    extends WrappedNode(value)
    with Terminate {
  override def prettyPrint: String = "project(" + by.map(_.toString).map("_." + _).mkString(", ") + ")"
}
