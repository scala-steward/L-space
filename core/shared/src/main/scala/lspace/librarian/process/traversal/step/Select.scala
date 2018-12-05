package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import shapeless.{HList, Poly1}

object Select extends StepDef("Select") with StepWrapper[Select[Any]] {

  case class Selection[SelectedLabels <: HList, TypesTuple](labels: SelectedLabels)

//  trait FilterA[A] {
//    type Out <: Poly1
//    def p: Out
//  }
//  object FilterA {
//    type Aux[A, Out0 <: Poly1] = FilterA[A] { type Out = Out0 }
//
//    implicit def filter[A] = new FilterA[A] {
//      val p1 = new Poly1 {
//        implicit def as[T] = at[As[T, A]](s => s)
//      }
//      type Out = p1.type
//      val p: Out = p1
//    }
//  }

//  object LabelPoly {
//    def apply[Label] = new LabelPoly[Label] {}
//  }
////  object LabelA extends LabelPoly["b"]
//  trait LabelPoly[Label] extends Poly1 {
//    implicit def as[T] = at[As[T, Label]](s => s)
//  }

  def wrap(node: Node): Select[Any] = node match {
    case node: Select[Any] => node
    case _                 => Select[Any](node.out(Select.keys.nameString), node)
  }

  object keys {
    private val nameNode = MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/step/Select/name")
    nameNode.addLabel(Property.ontology)
    nameNode --- Property.default.`@label` --> "name" --- Property.default.`@language` --> "en"
    nameNode --- Property.default.`@comment` --> "The name of the result to retrieve" --- Property.default.`@language` --> "en"
    nameNode --- Property.default.`@container` --> types.`@listset`
    nameNode --- Property.default.`@range` --> DataType.default.`@string`

    lazy val name: Property               = Property(nameNode)
    val nameString: TypedProperty[String] = name + DataType.default.`@string`
  }

  def apply[E](names: List[String]): Select[E] = {
    val node = DetachedGraph.nodes.create(ontology)

    names.foreach(node.addOut(keys.name, _))
    Select[E](names, node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.name
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class Select[E] private (names: List[String], override val value: Node)
    extends WrappedNode(value)
    with TraverseStep
    with ModulateStep {
  override def prettyPrint: String = s"select(${names.mkString("a")}"
}
