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

  def wrap(node: Node): Union[ClassType[Any], ClassType[Any]] = node match {
    //    case node: Union[Any, Any, F] => node
    case _ =>
      Union(
        node
          .out(keys.traversalTraversal)
          .map(
            Traversal
              .wrap(_)(DetachedGraph)
              .asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]]),
        node
      )
  }

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

  def apply[S <: ClassType[_], E <: ClassType[_]](traversals: List[Traversal[S, E, _ <: HList]]): Union[S, E] = {
    val node = DetachedGraph.nodes.create(ontology)

    traversals.map(_.self).foreach(node.addOut(keys.traversal, _))
    Union[S, E](traversals, node)
  }
}

case class Union[S <: ClassType[_], E <: ClassType[_]] private (traversals: List[Traversal[S, E, _ <: HList]],
                                                                override val value: Node)
    extends WrappedNode(value)
    with BranchStep {
  override def prettyPrint: String =
    "union(" + traversals.map(_.toString).map("_." + _).mkString(", ") + ")"
}
