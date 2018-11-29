package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import shapeless.HList

object Path extends StepDef("Path") with StepWrapper[Path] {

  def wrap(node: Node): Path = node match {
    case node: Path => node
    case _ =>
      Path(
        node
          .out(keys.byTraversal)
          .take(1)
          .map(
            Traversal
              .wrap(_)(DetachedGraph)(ClassType.default[Any])
              .asInstanceOf[Traversal[_ <: ClassType[_], _ <: ClassType[_], HList]])
          .head,
        node
      )
  }

  object keys {
    private val byNode =
      MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/step/Path/by")
    byNode.addLabel(Property.ontology)
    byNode --- Property.default.`@label` --> "by" --- Property.default.`@language` --> "en"
    byNode --- Property.default.`@comment` --> "A traversal .." --- Property.default.`@language` --> "en"
    byNode --- Property.default.`@range` --> Traversal.ontology
    lazy val by                          = Property(byNode)
    val byTraversal: TypedProperty[Node] = by + Traversal.ontology
  }

  //  def apply(property: Property): Local = {
  //    val node = DetachedGraph.newNode(ontology)
  //
  //    node.property(keys.by, property)
  //    Path(property, node)
  //  }

  def apply(by: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]): Path = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.by, by.self)
    Path(by, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.by
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Path private (by: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList], override val value: Node)
    extends WrappedNode(value)
    with MapStep {
  override def prettyPrint: String = if (by.stepsList.nonEmpty) "path(" + by.toString + ")" else "path"
}
