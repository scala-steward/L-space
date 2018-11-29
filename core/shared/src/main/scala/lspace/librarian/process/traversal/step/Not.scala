package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import shapeless.{HList, HNil}

object Not extends StepDef("Not") with StepWrapper[Not] {

  def wrap(node: Node): Not = node match {
    case node: Not => node
    case _ =>
      Not(
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
    private val traversalNode = MemGraphDefault.ns.nodes.upsert("sptth/tbd.tld/librarian/step/Not/traversal")
    traversalNode.addLabel(Property.ontology)
    traversalNode --- Property.default.`@label` --> "traversal" --- Property.default.`@language` --> "en"
    traversalNode --- Property.default.`@comment` --> "A traversal which must have a empty result" --- Property.default.`@language` --> "en"
    traversalNode --- Property.default.`@range` --> Traversal.ontology

    lazy val traversal: Property                = Property(traversalNode)
    val traversalTraversal: TypedProperty[Node] = traversal + Traversal.ontology
  }

  def apply(traversal: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList]): Not = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.traversal, traversal.self)
    Not(traversal, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.traversal
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Not private (traversal: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList],
                        override val value: Node)
    extends WrappedNode(value)
    with FilterStep {
  override def prettyPrint: String = "not(_." + traversal.toString + ")"
}
