package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import shapeless.{HList, HNil}

//trait GroupType[A, C[+Z]] {
//  type Out[+T]
//}
//object GroupType {
//  type Aux[A, C[+Z]] = GroupType[A, C] {
//    type Out[+T] = C[Map[A, List[T]]]
//  }
//  implicit def wrapgroup[A, C[+Z]]: GroupType[A, C] = new GroupType[A, C] {
//    type Out[+T] = C[Map[A, List[T]]]
//  }
//}

object Group extends StepDef("Group") with StepWrapper[Group[ClassType[Any]]] {

  def wrap(node: Node): Group[ClassType[Any]] = node match {
    case node: Group[ClassType[Any]] => node
    case _ =>
      Group(node
              .out(keys.byTraversal)
              .take(1)
              .map(Traversal.wrap(_)(DetachedGraph))
              .head,
            node)
  }

  object keys {
    private val byNode =
      MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/step/Group/by")
    byNode.addLabel(Property.ontology)
    byNode --- Property.default.`@label` --> "by" --- Property.default.`@language` --> "en"
    byNode --- Property.default.`@comment` --> "A traversal .." --- Property.default.`@language` --> "en"
    byNode --- Property.default.`@range` --> Traversal.ontology

    lazy val by = Property(byNode)
    //    val byProperty: TypedPropertyKey[Node] = by.addRange(Property.ontology)
    val byTraversal: TypedProperty[Node] = by + Traversal.ontology
  }

  trait Properties {
    lazy val `ns.l-space.eu/librarian/step/Group/by`: Property                  = keys.by
    lazy val `ns.l-space.eu/librarian/step/Group/by @Traversal`: TypedKey[Node] = keys.byTraversal
  }

  //  def apply(property: Property): Local = {
  //    val node = DetachedGraph.newNode(ontology)
  //
  //    node.property(keys.by, property)
  //    Group(property, node)
  //  }

  def apply[A <: ClassType[_]](by: Traversal[_ <: ClassType[_], A, _ <: HList]): Group[A] = {
    val node = DetachedGraph.nodes.create(ontology)

    node.addOut(keys.by, by.self)
    Group[A](by, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.by
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Group[A <: ClassType[_]] private (by: Traversal[_ <: ClassType[_], A, _ <: HList], override val value: Node)
    extends WrappedNode(value)
    with CollectingBarrierStep {
  override def prettyPrint: String = "group(_." + by.toString + ")"
}
