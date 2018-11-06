package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import shapeless.{HList, HNil}

object Local extends StepDef("Local") with StepWrapper[Local] {

  def wrap(node: Node): Local = node match {
    //    case node: Local[F] => node
    case _ =>
      Local(
        node
          .out(keys.traversalTraversal)
          .take(1)
          .map(
            Traversal
              .wrap(_)(DetachedGraph)(ClassType.default[Any])
              .asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]])
          .head,
        node
      )
  }

  object keys {
    private val traversalNode = MemGraphDefault.ns.upsertNode("sptth/tbd.tld/librarian/step/Local/traversal")
    traversalNode.addLabel(Property.ontology)
    traversalNode --- Property.default.label --> "traversal" --- Property.default.language --> "en"
    traversalNode --- Property.default.comment --> "A traversal .." --- Property.default.language --> "en"
    traversalNode --- Property.default.range --> Traversal.ontology

    lazy val traversal: Property                = Property(traversalNode)
    val traversalTraversal: TypedProperty[Node] = traversal + Traversal.ontology
  }

  def apply(traversal: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]): Local = {
    val node = DetachedGraph.createNode(ontology)

    node.addOut(keys.traversalTraversal, traversal.self)
    Local(traversal, node)
  }

  ontologyNode --- Property.default.properties --> keys.traversal
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Local private (traversal: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList],
                          override val value: Node)
    extends WrappedNode(value)
    with BranchStep {
  override def prettyPrint: String = "local(_." + traversal.toString + ")"
}
