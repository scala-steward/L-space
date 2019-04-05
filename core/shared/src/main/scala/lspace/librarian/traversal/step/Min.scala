package lspace.librarian.traversal.step

import lspace.datatype._
import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import monix.eval.Task
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

  def toStep(node: Node): Task[Min] =
    for {
      by <- Task
        .gather(
          node
            .out(Min.keys.byTraversal)
            .map(Traversal.toTraversal(_).map(_.asInstanceOf[Traversal[ClassType[Any], DataType[Any], HNil]])))
        .map(_.filter(_.et.isInstanceOf[DataType[_]]).head)
    } yield Min(by)

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

  implicit def toNode(step: Min): Task[Node] = {
    for {
      node      <- DetachedGraph.nodes.create(ontology)
      traversal <- step.by.toNode
      _         <- node.addOut(keys.byTraversal, traversal)
    } yield node
  }.memoizeOnSuccess
}

case class Min(by: Traversal[_ <: ClassType[_], _ <: DataType[_], _ <: HList]) extends FilterBarrierStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "min(" + by.toString + ")"
}
