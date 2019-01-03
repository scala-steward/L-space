package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import shapeless.{HList, HNil}

object Union
    extends StepDef("Union", "A union-step ..", () => BranchStep.ontology :: Nil)
    with StepWrapper[Union[ClassType[Any], ClassType[Any]]] {

  def toStep(node: Node): Union[ClassType[Any], ClassType[Any]] = Union(
    node
      .out(keys.traversalTraversal)
      .map(
        Traversal
          .toTraversal(_)(DetachedGraph)
          .asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
  )

  object keys extends BranchStep.Properties {
    object traversal
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Union/traversal",
          "traversal",
          "A traversal ..",
          container = lspace.NS.types.`@list` :: Nil,
          `@range` = () => Traversal.ontology :: Nil
        )
    val traversalTraversal: TypedProperty[Node] = traversal.property + Traversal.ontology
  }
  override lazy val properties: List[Property] = keys.traversal :: BranchStep.properties
  trait Properties extends BranchStep.Properties {
    val traversal          = keys.traversal
    val traversalTraversal = keys.traversalTraversal
  }

  implicit def toNode(union: Union[_ <: ClassType[_], _ <: ClassType[_]]): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    union.traversals.map(_.toNode).foreach(node.addOut(keys.traversal, _))
    node
  }
}

case class Union[S <: ClassType[_], E <: ClassType[_]](traversals: List[Traversal[S, E, _ <: HList]])
    extends BranchStep {

  lazy val toNode: Node = this
  override def prettyPrint: String =
    "union(" + traversals.map(_.toString).map("_." + _).mkString(", ") + ")"
}
