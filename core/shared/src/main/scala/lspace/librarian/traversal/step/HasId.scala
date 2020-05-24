package lspace.librarian.traversal.step

import lspace.NS.types
import lspace.datatype.DataType
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task

object HasId
    extends StepDef("HasId", "A hasId-step filters resources by id.", HasStep.ontology :: Nil)
    with StepWrapper[HasId] {

  def toStep(node: Node): Task[HasId] = Task.now(HasId(node.out(HasId.keys.idLong).toSet))

  object keys {
    object id
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/HasId/Id",
          "Id",
          "An id",
          container = types.`@set` :: Nil,
          `@range` = DataType.default.`@long` :: Nil
        )
    val idLong: TypedProperty[Long] = id.property.as(DataType.default.`@long`)
  }
  override lazy val properties: List[Property] = keys.id.property :: HasStep.properties
  trait Properties extends HasStep.Properties {
    val id     = keys.id
    val idLong = keys.idLong
  }

  implicit def toNode(step: HasId): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- Task.parSequence(step.ids.map(node.addOut(keys.id, _)))
    } yield node
  }.memoizeOnSuccess

}

case class HasId(ids: Set[Long]) extends HasStep {

  lazy val toNode: Task[Node] = this
  override def prettyPrint: String =
    s"hasId(${ids.mkString(", ")})"
}
