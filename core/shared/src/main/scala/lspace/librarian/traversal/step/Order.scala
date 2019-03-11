package lspace.librarian.traversal.step

import lspace.datatype._
import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.datatype.DataType
import lspace.structure._
import shapeless.{HList, HNil}

object Order
    extends StepDef("Order", "An order-step ..", () => CollectingBarrierStep.ontology :: Nil)
    with StepWrapper[Order] {

  sealed trait Orderable[T]
  object Orderable {
    implicit def IsNumeric[T <: NumericType[_]]: Orderable[T]   = new Orderable[T] {}
    implicit def IsString[T <: TextType[_]]: Orderable[T]       = new Orderable[T] {}
    implicit def IsTemporal[T <: CalendarType[_]]: Orderable[T] = new Orderable[T] {}
    implicit def IsQuantity[T <: QuantityType[_]]: Orderable[T] = new Orderable[T] {}
    implicit def IsColor[T <: ColorType[_]]: Orderable[T]       = new Orderable[T] {}
  }

  def toStep(node: Node): Order = Order(
    node
      .out(Order.keys.byTraversal)
      .map(Traversal.toTraversal(_))
      .filter(_.et.isInstanceOf[DataType[_]])
      .map(_.asInstanceOf[Traversal[ClassType[Any], DataType[Any], HNil]])
      .head,
    node.out(Order.keys.increasingBoolean).take(1).headOption.getOrElse(true)
  )

  object keys extends CollectingBarrierStep.Properties {
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
  override lazy val properties: List[Property] = keys.by :: keys.increasing.property :: CollectingBarrierStep.properties
  trait Properties extends CollectingBarrierStep.Properties {
    val by                = keys.by
    val byTraversal       = keys.byTraversal
    val increasing        = keys.increasing
    val increasingBoolean = keys.increasingBoolean
  }

  implicit def toNode(order: Order): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(keys.byTraversal, order.by.toNode)
    if (!order.increasing) node.addOut(keys.increasingBoolean, order.increasing)
    node
  }

}

//case class Order(key: PropertyKey, increasing: Boolean = true) extends TraverseStep /*with ModulateStep[ZeroOrMoreBy]*/ {
case class Order(by: Traversal[_ <: ClassType[_], _ <: DataType[_], _ <: HList], increasing: Boolean)
    extends RearrangeBarrierStep /*with ModulateStep[ZeroOrMoreBy]*/ {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "order(" + by.toString + ")"
}