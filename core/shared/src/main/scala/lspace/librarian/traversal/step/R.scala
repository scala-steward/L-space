package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.datatype.{DataType, ListType}
import lspace.structure._
import lspace.NS.types
import monix.eval.Task

object R
    extends StepDef("R", "An r-step selects resources to traverse from.", () => ResourceStep.ontology :: Nil)
    with StepWrapper[R] {

  def toStep(node: Node): Task[R] = Task.now(R(node.outE(R.keys.resource).map(_.to)))

  object keys extends ResourceStep.Properties {
    object resource
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/R/resource",
          "resource",
          "A resource",
          `@range` = () => ListType(DataType.default.`@url` :: Nil) :: Nil
        )
    val resourceUrl: TypedProperty[List[Any]] = resource + ListType(Nil)
  }
  override lazy val properties: List[Property] = keys.resource.property :: ResourceStep.properties
  trait Properties extends ResourceStep.Properties {
    val resource = keys.resource
//    val resourceUrl = keys.resourceUrl
  }

  implicit def toNode(step: R): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- if (step.resources.nonEmpty) node.addOut(keys.resourceUrl, step.resources) else Task.unit
    } yield node
  }.memoizeOnSuccess
}

case class R(resources: List[Resource[_]]) extends ResourceStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "R(" + resources.map(_.iri).mkString(", ") + ")"
}
