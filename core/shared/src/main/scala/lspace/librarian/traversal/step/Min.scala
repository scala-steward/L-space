package lspace.librarian.traversal.step

import lspace.datatype._
import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import shapeless.{HList, HNil}

object Min
    extends StepDef(
      "Min",
      "A min-step finds the traverser with the resource with the smallest value within all traversers in-scope.",
      () => FilterBarrierStep.ontology :: Nil
    )
    with StepWrapper[Min] {

  sealed trait Minable[T]
  object Minable {
    implicit def IsNumeric[T <: NumericType[_]]: Minable[T]   = new Minable[T] {}
    implicit def IsString[T <: TextType[_]]: Minable[T]       = new Minable[T] {}
    implicit def IsTemporal[T <: CalendarType[_]]: Minable[T] = new Minable[T] {}
    implicit def IsQuantity[T <: QuantityType[_]]: Minable[T] = new Minable[T] {}
    implicit def IsColor[T <: ColorType[_]]: Minable[T]       = new Minable[T] {}
  }

  def toStep(node: Node): Min = Min(
    node
      .out(Order.keys.byTraversal)
      .map(Traversal.toTraversal(_))
      .filter(_.et.isInstanceOf[DataType[_]])
      .map(_.asInstanceOf[Traversal[ClassType[Any], DataType[Any], HNil]])
      .head
  )

  object keys extends FilterBarrierStep.Properties {
    object by
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Min/by",
          "by",
          "A traversal ..",
          `@range` = () => Traversal.ontology :: Nil
        )
    val byTraversal: TypedProperty[Node] = by.property + Traversal.ontology
  }
  override lazy val properties: List[Property] = keys.by :: FilterBarrierStep.properties
  trait Properties extends FilterBarrierStep.Properties {
    val by          = keys.by
    val byTraversal = keys.byTraversal
  }

  implicit def toNode(min: Min): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(keys.byTraversal, min.by.toNode)
    node
  }
}

case class Min(by: Traversal[_ <: ClassType[_], _ <: DataType[_], _ <: HList]) extends FilterBarrierStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "min(" + by.toString + ")"
}
