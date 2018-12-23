package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.types._

object HasIri
    extends StepDef("HasIri", "A hasIri-step filters resources by iri.", () => HasStep.ontology :: Nil)
    with StepWrapper[HasIri] {

  def wrap(node: Node): HasIri = node match {
    case node: HasIri => node
    case _            => HasIri(node)
  }

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

  def apply(ids: Set[String]): HasIri = {
    val node = DetachedGraph.nodes.create(ontology)

    //    node.addOuts(keys.idString, ids.toList)
    ids.foreach(node.addOut(keys.iri, _))
    HasIri(node)
  }

}

case class HasIri private (override val value: Node) extends WrappedNode(value) with HasStep {
  def ids: Set[String] = out(HasIri.keys.iriString).toSet
}
