package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.datatype.DataType

object HasId
    extends StepDef("HasId", "A hasId-step filters resources by id.", () => HasStep.ontology :: Nil)
    with StepWrapper[HasId] {

  def toStep(node: Node): HasId = HasId(node.out(HasId.keys.idLong).toSet)

  object keys {
    object id
        extends PropertyDef(
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

  implicit def toNode(hasId: HasId): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    hasId.ids.foreach(node.addOut(keys.id, _))
    node
  }

}

case class HasId(ids: Set[Long]) extends HasStep {

  lazy val toNode: Node = this
  override def prettyPrint: String =
    s"hasId(${ids.mkString(", ")})"
}
