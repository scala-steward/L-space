package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.types._

object HasIri extends StepDef("HasIri") with StepWrapper[HasIri] {

  def wrap(node: Node): HasIri = node match {
    case node: HasIri => node
    case _            => HasIri(node)
  }

  object keys {
    private val iriNode =
      MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/step/HasIri/iri")
    iriNode.addLabel(Property.ontology)
    iriNode --- Property.default.`@label` --> "Iri" --- Property.default.`@language` --> "en"
    iriNode --- Property.default.`@comment` --> "An iri" --- Property.default.`@language` --> "en"
    iriNode --- Property.default.`@container` --> types.`@set`
    iriNode --- Property.default.`@range` --> DataType.default.`@string`

    lazy val iri: Property               = Property(iriNode)
    val iriString: TypedProperty[String] = iri + DataType.default.`@string`
  }

  def apply(ids: Set[String]): HasIri = {
    val node = DetachedGraph.nodes.create(ontology)

    //    node.addOuts(keys.idString, ids.toList)
    ids.foreach(node.addOut(keys.iri, _))
    HasIri(node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.iri
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class HasIri private (override val value: Node) extends WrappedNode(value) with HasStep {
  def ids: Set[String] = out(HasIri.keys.iriString).toSet
}
