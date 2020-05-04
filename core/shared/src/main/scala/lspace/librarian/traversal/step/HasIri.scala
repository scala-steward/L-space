package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import lspace.NS.types
import lspace.datatype.DataType
import monix.eval.Task

object HasIri
    extends StepDef("HasIri", "A hasIri-step filters resources by iri.", HasStep.ontology :: Nil)
    with StepWrapper[HasIri] {

  def toStep(node: Node): Task[HasIri] = Task.now(HasIri(node.out(HasIri.keys.iriString).toSet))

  object keys {
    object iri
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/HasIri/iri",
          "Iri",
          "An iri",
          container = types.`@set` :: Nil,
          `@range` = DataType.default.`@string` :: Nil
        )
    val iriString: TypedProperty[String] = iri.property.as(DataType.default.`@string`)
  }
  override lazy val properties: List[Property] = keys.iri.property :: HasStep.properties
  trait Properties extends HasStep.Properties {
    val iri       = keys.iri
    val iriString = keys.iriString
  }

  implicit def toNode(step: HasIri): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- Task.parSequence(step.iris.map(node.addOut(keys.iri, _)))
    } yield node
  }.memoizeOnSuccess

}

case class HasIri(iris: Set[String]) extends HasStep {

  lazy val toNode: Task[Node] = this
  override def prettyPrint: String =
    s"hasIri(${iris.mkString(", ")})"
}
