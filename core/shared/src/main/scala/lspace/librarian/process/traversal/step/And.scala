package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import shapeless.{HList, HNil}

object And extends StepDef("And") with StepWrapper[And] {

  def wrap(node: Node): And = node match {
    case node: And => node
    case _ =>
      And(
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
    private val traversalNode = MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/step/And/traversal")
    traversalNode.addLabel(Property.ontology)
    traversalNode --- Property.default.`@label` --> "traversal" --- Property.default.`@language` --> "en"
    traversalNode --- Property.default.`@comment` --> "A traversal which must have a non-empty result" --- Property.default.`@language` --> "en"
    traversalNode --- Property.default.`@container` --> types.`@list`
    traversalNode --- Property.default.`@range` --> Traversal.ontology

    lazy val traversal: Property                = Property(traversalNode)
    val traversalTraversal: TypedProperty[Node] = traversal + Traversal.ontology
  }

  trait Properties {
    lazy val `ns.l-space.eu/librarian/step/And/traversal`: Property                  = keys.traversal
    lazy val `ns.l-space.eu/librarian/step/And/traversal @Traversal`: TypedKey[Node] = keys.traversalTraversal
  }

  def apply(traversals: List[Traversal[_, _, _ <: HList]]): And = {
    val node = DetachedGraph.nodes.create(ontology)

    traversals.map(_.self).foreach(node.addOut(keys.traversal, _))
    And(traversals, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.traversal
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class And private (traversals: List[Traversal[_, _, _ <: HList]], override val value: Node)
    extends WrappedNode(value)
    with FilterStep {
  override def prettyPrint: String =
    "and(" + traversals.map(_.toString).map("_." + _).mkString(", ") + ")"
}
