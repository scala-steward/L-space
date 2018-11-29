package lspace.librarian.process.traversal.step

import lspace.NS.types
import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import shapeless.{HList, HNil}

object Coalesce extends StepDef("Coalesce") with StepWrapper[Coalesce[ClassType[Any], ClassType[Any]]] {

  def wrap(node: Node): Coalesce[ClassType[Any], ClassType[Any]] = node match {
    //    case node: Union[Any, Any, F] => node
    case _ =>
      Coalesce[ClassType[Any], ClassType[Any]](
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
    private val traversalNode = MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/step/Coalesce/traversal")
    traversalNode.addLabel(Property.ontology)
    traversalNode --- Property.default.`@label` --> "traversal" --- Property.default.`@language` --> "en"
    traversalNode --- Property.default.`@comment` --> "A traversal .." --- Property.default.`@language` --> "en"
    traversalNode --- Property.default.`@container` --> types.`@list`
    traversalNode --- Property.default.`@range` --> Traversal.ontology

    lazy val traversal: Property                = Property(traversalNode)
    val traversalTraversal: TypedProperty[Node] = traversal + Traversal.ontology
  }

  trait Properties extends BranchStep {
    lazy val `ns.l-space.eu/librarian/step/Coalesce/traversal`: Property                  = keys.traversal
    lazy val `ns.l-space.eu/librarian/step/Coalesce/traversal @Traversal`: TypedKey[Node] = keys.traversalTraversal
  }

  def apply[S <: ClassType[_], E <: ClassType[_]](traversals: List[Traversal[S, E, _ <: HList]]): Coalesce[S, E] = {
    val node = DetachedGraph.nodes.create(ontology)

    traversals.map(_.self).foreach(node.addOut(keys.traversal, _))
    Coalesce[S, E](traversals, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.traversal
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Coalesce[S <: ClassType[_], E <: ClassType[_]] private (traversals: List[Traversal[S, E, _ <: HList]],
                                                                   override val value: Node)
    extends WrappedNode(value)
    with BranchStep {
  override def prettyPrint: String =
    "coalesce(" + traversals.map(_.toString).map("_." + _).mkString(", ") + ")"
}
