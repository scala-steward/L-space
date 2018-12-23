package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.types._

object HasId
    extends StepDef("HasId", "A hasId-step filters resources by id.", () => HasStep.ontology :: Nil)
    with StepWrapper[HasId] {

  def wrap(node: Node): HasId = node match {
    case node: HasId => node
    case _           => HasId(node)
  }

  object keys {
    object id
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/HasId/Id",
          "Id",
          "An id",
          container = types.`@set` :: Nil,
          `@range` = () => DataType.default.`@long` :: Nil
        )
    val idLong: TypedProperty[Long] = id.property + DataType.default.`@long`
  }
  override lazy val properties: List[Property] = keys.id.property :: HasStep.properties
  trait Properties extends HasStep.Properties {
    val id     = keys.id
    val idLong = keys.idLong
  }

  def apply(ids: Set[Long]): HasId = {
    val node = DetachedGraph.nodes.create(ontology)

    //    node.addOuts(keys.idString, ids.toList)
    ids.foreach(node.addOut(keys.id, _))
    HasId(node)
  }

}

case class HasId private (override val value: Node) extends WrappedNode(value) with HasStep {
  def ids: Set[Long] = out(HasId.keys.idLong).toSet
}
