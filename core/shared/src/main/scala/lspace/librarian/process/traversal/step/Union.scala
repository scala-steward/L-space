package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import shapeless.{HList, HNil}

object Union extends StepDef("Union") with StepWrapper[Union[ClassType[Any], ClassType[Any]]] {

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

  object keys {
    private val traversalNode =
      MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/step/Union/traversal")
    traversalNode.addLabel(Property.ontology)
    traversalNode --- Property.default.`@label` --> "traversal" --- Property.default.`@language` --> "en"
    traversalNode --- Property.default.`@comment` --> "A traversal .." --- Property.default.`@language` --> "en"
    traversalNode --- Property.default.`@container` --> types.`@list`
    traversalNode --- Property.default.`@range` --> Traversal.ontology

    lazy val traversal: Property                = Property(traversalNode)
    val traversalTraversal: TypedProperty[Node] = traversal + Traversal.ontology
  }

  def apply[S <: ClassType[_], E <: ClassType[_]](traversals: List[Traversal[S, E, _ <: HList]]): Union[S, E] = {
    val node = DetachedGraph.nodes.create(ontology)

    traversals.map(_.self).foreach(node.addOut(keys.traversal, _))
    Union[S, E](traversals, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.traversal
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Union[S <: ClassType[_], E <: ClassType[_]] private (traversals: List[Traversal[S, E, _ <: HList]],
                                                                override val value: Node)
    extends WrappedNode(value)
    with BranchStep {
  override def prettyPrint: String =
    "union(" + traversals.map(_.toString).map("_." + _).mkString(", ") + ")"
}
