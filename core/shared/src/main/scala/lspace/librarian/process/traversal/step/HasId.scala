package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.types._

object HasId extends StepDef("HasId") with StepWrapper[HasId] {

  def wrap(node: Node): HasId = node match {
    case node: HasId => node
    case _           => HasId(node)
  }

  object keys {
    private val idNode =
      MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/step/HasId/Id")
    idNode.addLabel(Property.ontology)
    idNode --- Property.default.`@label` --> "Id" --- Property.default.`@language` --> "en"
    idNode --- Property.default.`@comment` --> "An id" --- Property.default.`@language` --> "en"
    idNode --- Property.default.`@container` --> types.`@set`
    idNode --- Property.default.`@range` --> DataType.default.`@long`

    lazy val id: Property           = Property(idNode)
    val idLong: TypedProperty[Long] = id + DataType.default.`@long`
  }

  def apply(ids: Set[Long]): HasId = {
    val node = DetachedGraph.nodes.create(ontology)

    //    node.addOuts(keys.idString, ids.toList)
    ids.foreach(node.addOut(keys.id, _))
    HasId(node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.id
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class HasId private (override val value: Node) extends WrappedNode(value) with HasStep {
  def ids: Set[Long] = out(HasId.keys.idLong).toSet
}
