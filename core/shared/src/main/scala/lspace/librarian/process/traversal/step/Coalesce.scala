package lspace.librarian.process.traversal.step

import lspace.NS.types
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.structure._
import shapeless.{HList, HNil}

object Coalesce
    extends StepDef("Coalesce",
                    "A coalesce-steps continues on the first of n-traversals which has a non-empty result.",
                    () => BranchStep.ontology :: Nil)
    with StepWrapper[Coalesce[_, _]] {

  def toStep(node: Node): Coalesce[ClassType[Any], ClassType[Any]] = node match {
    //    case node: Union[Any, Any, F] => node
    case _ =>
      Coalesce[ClassType[Any], ClassType[Any]](
        node
          .out(keys.traversalTraversal)
          .map(
            Traversal
              .toTraversal(_)(DetachedGraph)
              .asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]]))
  }

  object keys extends BranchStep.Properties {
    object traversal
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Coalesce/traversal",
          "traversal",
          "A traversal ..",
          container = types.`@list` :: Nil,
          `@range` = () => Traversal.ontology :: Nil
        )
    val traversalTraversal: TypedProperty[Node] = traversal.property + Traversal.ontology
  }
  override lazy val properties: List[Property] = keys.traversal :: BranchStep.properties

  trait Properties extends BranchStep.Properties {
    lazy val `ns.l-space.eu/librarian/step/Coalesce/traversal`: Property = Coalesce.keys.traversal
    lazy val `ns.l-space.eu/librarian/step/Coalesce/traversal @Traversal`: TypedKey[Node] =
      Coalesce.keys.traversalTraversal
  }

  implicit def toNode(step: Coalesce[_ <: ClassType[_], _ <: ClassType[_]]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    step.traversals.map(_.toNode).foreach(node.addOut(keys.traversal, _))
    node
  }
}

case class Coalesce[S <: ClassType[_], E <: ClassType[_]](traversals: List[Traversal[S, E, _ <: HList]])
    extends BranchStep {

  def toNode: Node = this
  override def prettyPrint: String =
    "coalesce(" + traversals.map(_.toString).map("_." + _).mkString(", ") + ")"
}
