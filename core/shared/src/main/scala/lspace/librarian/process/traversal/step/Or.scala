package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import shapeless.{HList, HNil}

object Or extends StepDef("Or") with StepWrapper[Or] {

  def wrap(node: Node): Or = node match {
    case node: Or => node
    case _ =>
      Or(
        node
          .out(keys.traversalTraversal)
          .map(
            Traversal
              .wrap(_)(DetachedGraph)(ClassType.default[Any])
              .asInstanceOf[Traversal[ClassType[Any], ClassType[Any], HList]]),
        node
      )
  }

  object keys {
    private val traversalNode = MemGraphDefault.ns.upsertNode("sptth/tbd.tld/librarian/step/Or/traversal")
    traversalNode.addLabel(Property.ontology)
    traversalNode --- Property.default.label --> "traversal" --- Property.default.language --> "en"
    traversalNode --- Property.default.comment --> "A traversal .." --- Property.default.language --> "en"
    traversalNode --- Property.default.container --> types.list
    traversalNode --- Property.default.range --> Traversal.ontology

    lazy val traversal: Property                = Property(traversalNode)
    val traversalTraversal: TypedProperty[Node] = traversal + Traversal.ontology
  }

  def apply(traversals: List[Traversal[_, _, _ <: HList]]): Or = {
    val node = DetachedGraph.createNode(ontology)

    traversals.map(_.self).foreach(node.addOut(keys.traversal, _))

    Or(traversals, node)
  }

  ontologyNode --- Property.default.properties --> keys.traversal
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Or private (traversals: List[Traversal[_, _, _ <: HList]], override val value: Node)
    extends WrappedNode(value)
    with FilterStep {
  override def prettyPrint: String =
    "or(" + traversals.map(_.toString).map("_." + _).mkString(", ") + ")"
}
