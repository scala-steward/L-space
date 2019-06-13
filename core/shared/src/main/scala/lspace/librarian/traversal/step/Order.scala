package lspace.librarian.traversal.step

import lspace.datatype._
import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.datatype.DataType
import lspace.structure._
import monix.eval.Task
import shapeless.{HList, HNil}

object Order
    extends StepDef("Order", "An order-step ..", () => GroupingBarrierStep.ontology :: Nil)
    with StepWrapper[Order] {

  sealed trait Orderable[T]
  object Orderable {
    implicit def IsNumeric[T <: NumericType[_]]: Orderable[T]   = new Orderable[T] {}
    implicit def IsString[T <: TextType[_]]: Orderable[T]       = new Orderable[T] {}
    implicit def IsTemporal[T <: CalendarType[_]]: Orderable[T] = new Orderable[T] {}
    implicit def IsQuantity[T <: QuantityType[_]]: Orderable[T] = new Orderable[T] {}
    implicit def IsColor[T <: ColorType[_]]: Orderable[T]       = new Orderable[T] {}
  }

  def toStep(node: Node): Task[Order] =
    for {
      by <- Task
        .gather(
          node
            .out(Order.keys.byTraversal)
            .map(Traversal.toTraversal(_).map(_.asInstanceOf[Traversal[ClassType[Any], DataType[Any], HNil]])))
        .map(_.filter(_.et.isInstanceOf[DataType[_]]).head)
    } yield Order(by, node.out(Order.keys.increasingBoolean).take(1).headOption.getOrElse(true))

  object keys extends GroupingBarrierStep.Properties {
    object by
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Order/by",
          "by",
          "A traversal ..",
          `@range` = () => Traversal.ontology :: Nil
        )
    val byTraversal: TypedProperty[Node] = by.property + Traversal.ontology

    object increasing
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Order/increasing",
          "increasing",
          "Set to true to sort ascending",
          `@range` = () => DataType.default.`@string` :: Nil
        )
    val increasingBoolean: TypedProperty[Boolean] = increasing.property + DataType.default.`@boolean`
  }
  override lazy val properties: List[Property] = keys.by :: keys.increasing.property :: GroupingBarrierStep.properties
  trait Properties extends GroupingBarrierStep.Properties {
    val by                = keys.by
    val byTraversal       = keys.byTraversal
    val increasing        = keys.increasing
    val increasingBoolean = keys.increasingBoolean
  }

  implicit def toNode(step: Order): Task[Node] = {
    for {
      node      <- DetachedGraph.nodes.create(ontology)
      traversal <- step.by.toNode
      _         <- node.addOut(keys.byTraversal, traversal)
      _         <- if (!step.increasing) node.addOut(keys.increasingBoolean, step.increasing) else Task.unit
    } yield node
  }.memoizeOnSuccess

}

//case class Order(key: PropertyKey, increasing: Boolean = true) extends TraverseStep /*with ModulateStep[ZeroOrMoreBy]*/ {
case class Order(by: Traversal[_ <: ClassType[_], _ <: ClassType[_], _ <: HList], increasing: Boolean = true)
    extends RearrangeBarrierStep /*with ModulateStep[ZeroOrMoreBy]*/ {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "order(" + by.toString + ")"
}
