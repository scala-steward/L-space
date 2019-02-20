package lspace.librarian.traversal.step

import lspace.datatype._
import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import shapeless.{HList, HNil}

object Max
    extends StepDef("Max",
                    "A max-step finds the traverser with the largest value within all traversers in-scope.",
                    () => FilterBarrierStep.ontology :: Nil)
    with StepWrapper[Max] {

  sealed trait Maxable[T]
  object Maxable {
    implicit def IsNumeric[T <: NumericType[_]]: Maxable[T]   = new Maxable[T] {}
    implicit def IsString[T <: TextType[_]]: Maxable[T]       = new Maxable[T] {}
    implicit def IsTemporal[T <: CalendarType[_]]: Maxable[T] = new Maxable[T] {}
    implicit def IsQuantity[T <: QuantityType[_]]: Maxable[T] = new Maxable[T] {}
    implicit def IsColor[T <: ColorType[_]]: Maxable[T]       = new Maxable[T] {}
  }

  def toStep(node: Node): Max = Max(
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
          lspace.NS.vocab.Lspace + "librarian/step/Max/by",
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

  implicit def toNode(max: Max): Node = {
    val node = DetachedGraph.nodes.create(ontology)
    node.addOut(keys.byTraversal, max.by.toNode)
    node
  }
}

case class Max(by: Traversal[_ <: ClassType[_], _ <: DataType[_], _ <: HList]) extends FilterBarrierStep {

  lazy val toNode: Node            = this
  override def prettyPrint: String = "max(" + by.toString + ")"
}
