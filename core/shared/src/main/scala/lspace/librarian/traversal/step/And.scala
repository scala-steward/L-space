package lspace.librarian.traversal.step

import lspace.datatype.ListType
import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task
import shapeless.HList

object And
    extends StepDef(
      "And",
      "An and-step traverser only survives if all n-traversals have a non-empty result.",
      FilterStep.ontology :: Nil
    )
    with StepWrapper[And] {

  def toStep(node: Node): Task[And] = node match {
    case node: And => Task.now(node)
    case _ =>
      for {
        traversals <- Task.parSequence(
          node
            .out(keys.traversalTraversal)
            .map(
              _.map(
                Traversal
                  .toTraversal(_)
                  .map(_.asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
              )
            )
            .head
        )
      } yield And(traversals)
  }

  object keys extends FilterStep.Properties {
    object traversal
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/And/traversal",
          "traversal",
          "A traversal which must have a non-empty result",
          `@range` = ListType(Traversal.ontology) :: Nil
        )
    val traversalTraversal: TypedProperty[List[Node]] = traversal.property.as(ListType(Traversal.ontology))
  }
  override lazy val properties: List[Property] = keys.traversal :: FilterStep.properties

  trait Properties extends FilterStep.Properties {
    lazy val `ns.l-space.eu/librarian/step/And/traversal`: Property                        = keys.traversal
    lazy val `ns.l-space.eu/librarian/step/And/traversal @Traversal`: TypedKey[List[Node]] = keys.traversalTraversal
  }

  implicit def toNode(and: And): Task[Node] = {
    for {
      node       <- DetachedGraph.nodes.create(ontology)
      traversals <- Task.parSequence(and.traversals.map(_.toNode))
      _          <- node.addOut(keys.traversalTraversal, traversals)
    } yield node
  }.memoizeOnSuccess
}

case class And(traversals: List[Traversal[_ <: ClassType[Any], _ <: ClassType[Any], _ <: HList]]) extends FilterStep {

  lazy val toNode: Task[Node] = this
  override def prettyPrint: String =
    "and(" + traversals.map(_.toString).map("_." + _).mkString(", ") + ")"
}
