package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import shapeless.HList

object Project extends StepDef("Project") with StepWrapper[Project] {

  def wrap(node: Node): Project = node match {
    case node: Project => node
    case _ =>
      new Project(node
                    .out(Project.keys.byTraversal)
                    .map(Traversal.wrap(_)(DetachedGraph)),
                  node)
  }

  object keys {
    private val byNode =
      MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/step/Project/by")
    byNode.addLabel(Property.ontology)
    byNode --- Property.default.`@label` --> "by" --- Property.default.`@language` --> "en"
    byNode --- Property.default.`@comment` --> "A traversal ..." --- Property.default.`@language` --> "en"
    byNode --- Property.default.`@container` --> types.`@list`
    byNode --- Property.default.`@range` --> Traversal.ontology

    lazy val by: Property                = Property(byNode)
    val byTraversal: TypedProperty[Node] = by + Traversal.ontology
  }

  def apply(traversals: List[Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]]): Project = {
    val node = DetachedGraph.nodes.create(ontology)

    traversals.map(_.self).foreach(node.addOut(keys.by, _))
    Project(traversals, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.by
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Project private (by: List[Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]],
                            override val value: Node)
    extends WrappedNode(value)
    with Terminate {
  override def prettyPrint: String = "project(" + by.map(_.toString).map("_." + _).mkString(", ") + ")"
}
