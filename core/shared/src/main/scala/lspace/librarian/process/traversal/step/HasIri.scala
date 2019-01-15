package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.datatype.DataType

object HasIri
    extends StepDef("HasIri", "A hasIri-step filters resources by iri.", () => HasStep.ontology :: Nil)
    with StepWrapper[HasIri] {

  def toStep(node: Node): HasIri = HasIri(node.out(HasIri.keys.iriString).toSet)

  object keys {
    object iri
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/HasIri/iri",
          "Iri",
          "An iri",
          container = types.`@set` :: Nil,
          `@range` = () => DataType.default.`@string` :: Nil
        )
    val iriString: TypedProperty[String] = iri.property + DataType.default.`@string`
  }
  override lazy val properties: List[Property] = keys.iri.property :: HasStep.properties
  trait Properties extends HasStep.Properties {
    val iri       = keys.iri
    val iriString = keys.iriString
  }

  implicit def toNode(hasIri: HasIri): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    hasIri.iris.foreach(node.addOut(keys.iri, _))
    node
  }

}

case class HasIri(iris: Set[String]) extends HasStep {

  lazy val toNode: Node = this
  override def prettyPrint: String =
    s"hasIri(${iris.mkString(", ")})"
}
