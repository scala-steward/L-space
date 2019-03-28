package lspace.librarian.traversal.step

import lspace.datatype.DataType
import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import lspace.NS.types
import monix.eval.Task
import shapeless.{HList, Poly1}

object Select
    extends StepDef("Select",
                    "A select-step selects the preliminary result from marked steps in the traversal path.",
                    `@extends` = () => TraverseStep.ontology :: Nil)
    with StepWrapper[Select[Any]] {

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

  def toStep(node: Node): Select[Any] = Select[Any](node.out(Select.keys.nameString))

  object keys {
    object name
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Select/name",
          "name",
          "The name of the result to retrieve",
          container = types.`@listset` :: Nil,
          `@range` = () => DataType.default.`@string` :: Nil
        )
    val nameString: TypedProperty[String] = name.property + DataType.default.`@string`
  }
  override lazy val properties: List[Property] = keys.name :: Nil

  implicit def toNode(step: Select[_]): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _    <- Task.gather(step.names.map(node.addOut(keys.name, _)))
    } yield node
  }
}

case class Select[E](names: List[String]) extends TraverseStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = s"select(${names.mkString("a")}"
}
