package lspace.librarian.traversal.step

import lspace.NS.types
import lspace.librarian.traversal.{Traversal, TypedKey}
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task
import shapeless.HList

object Choose
    extends StepDef(
      "Choose",
      "A choose-steps continues on the first of n-traversals which has a non-empty result.",
      BranchStep.ontology :: Nil
    )
    with StepWrapper[Choose[_, _]] {

  def toStep(node: Node): Task[Choose[ClassType[Any], ClassType[Any]]] = node match {
    //    case node: Union[Any, Any, F] => node
    case _ =>
      for {
        by <- node
          .out(keys.byTraversal)
          .headOption
          .map(
            Traversal
              .toTraversal(_)
              .map(_.asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
          )
          .get
        right <- node
          .out(keys.rightTraversal)
          .headOption
          .map(
            Traversal
              .toTraversal(_)
              .map(_.asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
          )
          .get
        left <- node
          .out(keys.leftTraversal)
          .headOption
          .map(
            Traversal
              .toTraversal(_)
              .map(_.asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
          )
          .get
      } yield Choose[ClassType[Any], ClassType[Any]](by, right, left)
  }

  object keys extends BranchStep.Properties {
    object by
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Choose/traversal",
          "traversal",
          "A traversal ..",
          container = types.`@list` :: Nil,
          `@range` = Traversal.ontology :: Nil
        )
    val byTraversal: TypedProperty[Node] = by.property.as(Traversal.ontology)

    object right
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Choose/right",
          "right",
          "The path to take when 'by' evaluates non-empty.",
          container = types.`@list` :: Nil,
          `@range` = Traversal.ontology :: Nil
        )
    val rightTraversal: TypedProperty[Node] = right.property.as(Traversal.ontology)

    object left
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Choose/left",
          "left",
          "The path to take when 'by' evaluates empty",
          container = types.`@list` :: Nil,
          `@range` = Traversal.ontology :: Nil
        )
    val leftTraversal: TypedProperty[Node] = left.property.as(Traversal.ontology)
  }
  override lazy val properties: List[Property] =
    keys.by.property :: keys.right.property :: keys.left.property :: BranchStep.properties

  trait Properties extends BranchStep.Properties {
    lazy val `ns.l-space.eu/librarian/step/Choose/by`: Property = Choose.keys.by
    lazy val `ns.l-space.eu/librarian/step/Choose/by@Traversal`: TypedKey[Node] =
      Choose.keys.byTraversal
    lazy val `ns.l-space.eu/librarian/step/Choose/right`: Property = Choose.keys.right
    lazy val `ns.l-space.eu/librarian/step/Choose/right@Traversal`: TypedKey[Node] =
      Choose.keys.rightTraversal
    lazy val `ns.l-space.eu/librarian/step/Choose/left`: Property = Choose.keys.left
    lazy val `ns.l-space.eu/librarian/step/Choose/left@Traversal`: TypedKey[Node] =
      Choose.keys.leftTraversal
  }

  implicit def toNode(step: Choose[_ <: ClassType[_], _ <: ClassType[_]]): Task[Node] = {
    for {
      node  <- DetachedGraph.nodes.create(ontology)
      by    <- step.by.toNode
      right <- step.right.toNode
      left  <- step.left.toNode
      _     <- node.addOut(keys.by, by)
      _     <- node.addOut(keys.right, right)
      _     <- node.addOut(keys.left, left)
    } yield node
  }.memoizeOnSuccess
}

case class Choose[S <: ClassType[_], E <: ClassType[_]](
  by: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList],
  right: Traversal[S, E, _ <: HList],
  left: Traversal[S, E, _ <: HList]
) extends BranchStep {

  lazy val toNode: Task[Node] = this
  override def prettyPrint: String =
    "choose(_." + by + ", " + right + "," + left + ")"
}
