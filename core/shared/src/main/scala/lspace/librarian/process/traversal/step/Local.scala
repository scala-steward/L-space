package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import shapeless.{HList, HNil}

object Local extends StepDef("Local", "A local-step ..", () => BranchStep.ontology :: Nil) with StepWrapper[Local] {

  def wrap(node: Node): Local = node match {
    //    case node: Local[F] => node
    case _ =>
      Local(
        node
          .out(keys.traversalTraversal)
          .take(1)
          .map(
            Traversal
              .wrap(_)(DetachedGraph)
              .asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
          .head,
        node
      )
  }

  object keys extends BranchStep.Properties {
    object traversal
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Local/traversal",
          "traversal",
          "A traversal ..",
          `@range` = () => Traversal.ontology :: Nil
        )
    val traversalTraversal: TypedProperty[Node] = traversal.property + Traversal.ontology
  }
  override lazy val properties: List[Property] = keys.traversal :: BranchStep.properties
  trait Properties extends BranchStep.Properties {
    val traversal          = keys.traversal
    val traversalTraversal = keys.traversalTraversal
  }

  def apply(traversal: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]): Local = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.traversalTraversal, traversal.self)
    Local(traversal, node)
  }

}

case class Local private (traversal: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList],
                          override val value: Node)
    extends WrappedNode(value)
    with BranchStep {
  override def prettyPrint: String = "local(_." + traversal.toString + ")"
}
