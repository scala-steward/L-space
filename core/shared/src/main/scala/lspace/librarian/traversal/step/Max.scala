package lspace.librarian.traversal.step

import lspace.datatype._
import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.provider.mem.MemResource
import lspace.structure._
import monix.eval.Task
import shapeless.{HList, HNil}

object Max
    extends StepDef(
      "Max",
      "A max-step finds the traverser with the largest value within all traversers in-scope.",
      () => FilterBarrierStep.ontology :: ReducingStep.ontology :: Nil
    )
    with StepWrapper[Max] {

  sealed trait Maxable[T]
  object Maxable {
    implicit def IsNumeric[T <: NumericType[_]]: Maxable[T]   = new Maxable[T] {}
    implicit def IsString[T <: TextType[_]]: Maxable[T]       = new Maxable[T] {}
    implicit def IsTemporal[T <: CalendarType[_]]: Maxable[T] = new Maxable[T] {}
    implicit def IsQuantity[T <: QuantityType[_]]: Maxable[T] = new Maxable[T] {}
    implicit def IsColor[T <: ColorType[_]]: Maxable[T]       = new Maxable[T] {}
  }

  def toStep(node: Node): Task[Max] =
    for {
      by <- Task
        .gather(
          node
            .out(Max.keys.byTraversal)
            .map(Traversal.toTraversal(_).map(_.asInstanceOf[Traversal[ClassType[Any], DataType[Any], HNil]])))
        .map(_.filter(_.et.isInstanceOf[DataType[_]]).head)
    } yield Max(by)

  object keys extends FilterBarrierStep.Properties with ReducingStep.Properties {
    object by
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/Max/by",
          "by",
          "A traversal ..",
          `@range` = () => Traversal.ontology :: Nil
        )
    val byTraversal: TypedProperty[Node] = by.property as Traversal.ontology
  }
  override lazy val properties: List[Property] = keys.by :: FilterBarrierStep.properties ++ ReducingStep.properties
  trait Properties extends FilterBarrierStep.Properties with ReducingStep.Properties {
    val by          = keys.by
    val byTraversal = keys.byTraversal
  }

  implicit def toNode(step: Max): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      by   <- step.by.toNode
      _    <- node.addOut(keys.byTraversal, by)
    } yield node
  }.memoizeOnSuccess
}

case class Max(by: Traversal[_ <: ClassType[_], _ <: DataType[_], _ <: HList])
    extends FilterBarrierStep
    with ReducingStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "max(" + by.toString + ")"
}
