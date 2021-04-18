package lspace.librarian.traversal.step

import lspace.datatype.{DataType, DataTypeDef}
import lspace.librarian.traversal.TypedKey
import lspace.structure._
import lspace.structure.OntologyDef
import lspace.structure.PropertyDef
import monix.eval.Task

object Step extends OntologyDef(lspace.NS.vocab.Lspace + "librarian/Step", Set(), "Step", "Step") {

  def toStep(node: Node): Task[Step] = node match {
    case step: Step => Task.now(step)
    case _ =>
      node.labels match {
        case list if list.contains(G.ontology)         => G.toStep(node)
        case list if list.contains(N.ontology)         => N.toStep(node)
        case list if list.contains(V.ontology)         => V.toStep(node)
        case list if list.contains(R.ontology)         => R.toStep(node)
        case list if list.contains(E.ontology)         => E.toStep(node)
//        case list if list.contains(Drop.ontology)      => Drop.toStep(node)
        case list if list.contains(Dedup.ontology)     => Dedup.toStep(node)
        case list if list.contains(Out.ontology)       => Out.toStep(node)
        case list if list.contains(OutMap.ontology)    => OutMap.toStep(node)
        case list if list.contains(OutE.ontology)      => OutE.toStep(node)
        case list if list.contains(From.ontology)      => From.toStep(node)
        case list if list.contains(OutEMap.ontology)   => OutEMap.toStep(node)
        case list if list.contains(Group.ontology)     => Group.toStep(node)
        case list if list.contains(Path.ontology)      => Path.toStep(node)
        case list if list.contains(Id.ontology)        => Id.toStep(node)
        case list if list.contains(In.ontology)        => In.toStep(node)
        case list if list.contains(InMap.ontology)     => InMap.toStep(node)
        case list if list.contains(InE.ontology)       => InE.toStep(node)
        case list if list.contains(To.ontology)        => To.toStep(node)
        case list if list.contains(InEMap.ontology)    => InEMap.toStep(node)
        case list if list.contains(Has.ontology)       => Has.toStep(node)
        case list if list.contains(HasNot.ontology)    => HasNot.toStep(node)
        case list if list.contains(HasId.ontology)     => HasId.toStep(node)
        case list if list.contains(HasIri.ontology)    => HasIri.toStep(node)
        case list if list.contains(HasLabel.ontology)  => HasLabel.toStep(node)
        case list if list.contains(HasValue.ontology)  => HasValue.toStep(node)
        case list if list.contains(Coin.ontology)      => Coin.toStep(node)
        case list if list.contains(Constant.ontology)  => Constant.toStep(node)
        case list if list.contains(As.ontology)        => As.toStep(node)
        case list if list.contains(Repeat.ontology)    => Repeat.toStep(node)
        case list if list.contains(Select.ontology)    => Select.toStep(node)
        case list if list.contains(Project.ontology)   => Project.toStep(node)
        case list if list.contains(Where.ontology)     => Where.toStep(node)
        case list if list.contains(And.ontology)       => And.toStep(node)
        case list if list.contains(Or.ontology)        => Or.toStep(node)
        case list if list.contains(Not.ontology)       => Not.toStep(node)
        case list if list.contains(Union.ontology)     => Union.toStep(node)
        case list if list.contains(Choose.ontology)    => Choose.toStep(node)
        case list if list.contains(Coalesce.ontology)  => Coalesce.toStep(node)
        case list if list.contains(Local.ontology)     => Local.toStep(node)
        case list if list.contains(Range.ontology)     => Range.toStep(node)
        case list if list.contains(Label.ontology)     => Label.toStep(node)
        case list if list.contains(Head.ontology)      => Head.toStep(node)
        case list if list.contains(Last.ontology)      => Last.toStep(node)
        case list if list.contains(Limit.ontology)     => Limit.toStep(node)
        case list if list.contains(Skip.ontology)      => Skip.toStep(node)
        case list if list.contains(Tail.ontology)      => Tail.toStep(node)
        case list if list.contains(Order.ontology)     => Order.toStep(node)
        case list if list.contains(Count.ontology)     => Count.toStep(node)
        case list if list.contains(Is.ontology)        => Is.toStep(node)
        case list if list.contains(Sum.ontology)       => Sum.toStep(node)
        case list if list.contains(Max.ontology)       => Max.toStep(node)
        case list if list.contains(Min.ontology)       => Min.toStep(node)
        case list if list.contains(Mean.ontology)      => Mean.toStep(node)
        case list if list.contains(TimeLimit.ontology) => TimeLimit.toStep(node)
        case list =>
          throw new Exception(s"No valid Step-ontology found for types $list")
      }
  }

  object keys {}
  trait Properties extends OntologyDef.Properties

  lazy val steps: List[StepDef] = List(
    G,
    N,
    V,
    R,
    E,
//    Drop,
    Dedup,
    Out,
    OutMap,
    OutE,
    From,
    OutEMap,
    Group,
    Path,
    Id,
    In,
    InMap,
    InE,
    To,
    InEMap,
    Has,
    HasNot,
    HasId,
    HasIri,
    HasLabel,
    HasValue,
    Coin,
    Constant,
    As,
    Repeat,
    Select,
    Project,
    Where,
    And,
    Or,
    Not,
    Union,
    Choose,
    Coalesce,
    Local,
    Range,
    Label,
    Head,
    Last,
    Limit,
    Skip,
    Tail,
    Order,
    Count,
    Is,
    Sum,
    Max,
    Min,
    Mean
  )
}
trait Step extends Product with Serializable {

  def toNode: Task[Node]
  def prettyPrint: String
}

trait GraphStep extends ResourceStep
object GraphStep extends StepDef(label = "GraphStep", comment = "GraphStep", ResourceStep.ontology :: Nil) {
  object keys extends ResourceStep.Properties
  override lazy val properties: List[Property] = ResourceStep.properties
  trait Properties extends ResourceStep.Properties
}
trait Terminate extends Step
object Terminate extends StepDef(label = "Terminate", comment = "Terminate") {
  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties
}
trait TraverseStep extends Step
object TraverseStep
    extends StepDef(
      label = "TraverseStep",
      comment =
        "In a traverse-step the librarian can end the traversal or move along to one or more other states (traversers)."
    ) {
  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties
}
trait LabelStep extends Step
object LabelStep extends StepDef(label = "LabelStep", comment = "LabelStep", Step.ontology :: Nil) {
  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties
}
trait ProjectionStep extends Step
object ProjectionStep extends StepDef(label = "ProjectionStep", comment = "ProjectionStep", Step.ontology :: Nil) {
  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties
}
trait ResourceStep extends Step
object ResourceStep extends StepDef(label = "ResourceStep", comment = "ResourceStep", Step.ontology :: Nil) {
  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties
}
trait MoveStep extends BranchStep
object MoveStep extends StepDef(label = "MoveStep", comment = "MoveStep", BranchStep.ontology :: Nil) {

  object keys extends BranchStep.Properties {
    object label
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/MoveStep/label",
          "label",
          "A label",
          `@range` = Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil
        ) {}
    lazy val labelUrl: TypedProperty[IriResource] = label.property.as(DataType.default.`@url`)
  }

  override lazy val properties: List[Property] = keys.label.property :: BranchStep.properties

  /**
    * mirror of properties in object keys
    */
  trait Properties extends BranchStep.Properties {
    lazy val `ns.l-space.eu/librarian/MoveStep/label`: Property                   = keys.label
    lazy val `ns.l-space.eu/librarian/MoveStep/label @Url`: TypedKey[IriResource] = keys.labelUrl
  }
}
trait MapStep extends ProjectionStep with GroupingStep
object MapStep
    extends StepDef(label = "MapStep",
                    comment = "Property MapStep",
                    ProjectionStep.ontology :: GroupingStep.ontology :: Nil) {
  object keys extends ProjectionStep.Properties with GroupingStep.Properties
  override lazy val properties: List[Property] = ProjectionStep.properties ++ GroupingStep.properties
  trait Properties extends ProjectionStep.Properties with GroupingStep.Properties
}
trait BarrierStep extends Step
object BarrierStep extends StepDef(label = "BarrierStep", comment = "BarrierStep", Step.ontology :: Nil) {
  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties
}
trait GroupingStep extends Step
object GroupingStep extends StepDef(label = "GroupingStep", comment = "GroupingStep", Step.ontology :: Nil) {
  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties
}
trait RearrangeStep extends Step
object RearrangeStep extends StepDef(label = "RearrangeStep", comment = "RearrangeStep", Step.ontology :: Nil) {
  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties
}
//trait ReducingStep extends Step
//object ReducingStep extends StepDef(label = "ReducingStep", comment = "ReducingStep", Step.ontology :: Nil) {
//  object keys extends Step.Properties
//  override lazy val properties: List[Property] = Step.properties
//  trait Properties extends Step.Properties
//}
trait GroupingBarrierStep extends BarrierStep with GroupingStep //with TraverseStep
object GroupingBarrierStep
    extends StepDef(label = "CollectingBarrierStep",
                    comment = "CollectingBarrierStep",
                    BarrierStep.ontology :: GroupingStep.ontology :: TraverseStep.ontology :: Nil) {
  object keys extends BarrierStep.Properties with GroupingStep.Properties with TraverseStep.Properties
  override lazy val properties
    : List[Property] = BarrierStep.properties ++ GroupingStep.properties ++ TraverseStep.properties
  trait Properties extends BarrierStep.Properties with GroupingStep.Properties with TraverseStep.Properties
}
trait ReducingBarrierStep extends BarrierStep with ReducingStep //with TraverseStep
object ReducingBarrierStep
    extends StepDef(label = "ReducingBarrierStep",
                    comment = "ReducingBarrierStep",
                    BarrierStep.ontology :: ReducingStep.ontology :: Nil) {
  object keys extends BarrierStep.Properties with ReducingStep.Properties
  override lazy val properties: List[Property] = BarrierStep.properties ::: ReducingStep.properties
  trait Properties extends BarrierStep.Properties with ReducingStep.Properties
}
trait FilterBarrierStep extends BarrierStep with FilterStep //with ReducingStep
object FilterBarrierStep
    extends StepDef(label = "FilterBarrierStep",
                    comment = "FilterBarrierStep",
                    BarrierStep.ontology :: FilterStep.ontology :: Nil) {
  object keys extends BarrierStep.Properties with FilterStep.Properties
  override lazy val properties: List[Property] = BarrierStep.properties ::: FilterStep.properties
  trait Properties extends BarrierStep.Properties with FilterStep.Properties
}
trait RearrangeBarrierStep extends RearrangeStep with BarrierStep
object RearrangeBarrierStep
    extends StepDef(
      label = "RearrangeBarrierStep",
      comment = "A rearrange-barrier-step can change the order of the traversers.",
      RearrangeStep.ontology :: BarrierStep.ontology :: Nil
    ) {
  object keys extends RearrangeStep.Properties with BarrierStep.Properties
  override lazy val properties: List[Property] = RearrangeStep.properties ++ BarrierStep.properties
  trait Properties extends RearrangeStep.Properties with BarrierStep.Properties
}
trait EnvironmentStep extends Step
object EnvironmentStep extends StepDef(label = "EnvironmentStep", comment = "EnvironmentStep", Step.ontology :: Nil) {
  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties
}

trait FilterStep extends Step
object FilterStep extends StepDef(label = "FilterStep", comment = "FilterStep", Step.ontology :: Nil) {
  object keys extends TraverseStep.Properties
  override lazy val properties: List[Property] = TraverseStep.properties
  trait Properties extends TraverseStep.Properties
}
trait GlobalFilterStep extends FilterStep
object GlobalFilterStep
    extends StepDef(label = "GlobalFilterStep", comment = "GlobalFilterStep", FilterStep.ontology :: Nil) {
  object keys extends FilterStep.Properties
  override lazy val properties: List[Property] = FilterStep.properties
  trait Properties extends FilterStep.Properties
}
//trait ReduceStep extends Step
trait ReducingStep extends Step
object ReducingStep extends StepDef(label = "ReducingStep", comment = "ReducingStep", Step.ontology :: Nil) {
  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties
}
trait ClipStep extends FilterStep
object ClipStep extends StepDef(label = "ClipStep", comment = "ClipStep", FilterStep.ontology :: Nil) {
  object keys extends FilterStep.Properties
  override lazy val properties: List[Property] = FilterStep.properties
  trait Properties extends FilterStep.Properties
}
trait HasStep extends FilterStep
object HasStep extends StepDef(label = "HasStep", comment = "HasStep", FilterStep.ontology :: Nil) {

  sealed trait ClassTypeLabel[T]

  sealed trait PropertyLabel[T] extends ClassTypeLabel[T]

  implicit def IsPropertyDef[T <: PropertyDef] = new PropertyLabel[T] {}
  implicit object IsProperty extends PropertyLabel[Property]
  implicit object IsString   extends PropertyLabel[String]

  sealed trait DataTypeLabel[T] extends ClassTypeLabel[T]

  implicit def IsDataTypeDef[T <: DataTypeDef[_]] = new DataTypeLabel[T] {}
  implicit object IsDataType extends DataTypeLabel[DataType[_]]
  implicit object IsString1  extends DataTypeLabel[String]

  sealed trait OntologyLabel[T] extends ClassTypeLabel[T]

  implicit def IsOntologyDef[T <: OntologyDef] = new OntologyLabel[T] {}
  implicit object IsOntology extends OntologyLabel[Ontology]
  implicit object IsString2  extends OntologyLabel[String]

  object keys extends FilterStep.Properties
  override lazy val properties: List[Property] = FilterStep.properties
  trait Properties extends FilterStep.Properties
}

trait BranchStep extends Step
object BranchStep extends StepDef(label = "BranchStep", comment = "BranchStep", Step.ontology :: Nil) {
  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties
}

abstract class StepDef(label: String, comment: String = "", `@extends`: => List[Ontology] = List(Step.ontology))
    extends OntologyDef(lspace.NS.vocab.Lspace + s"librarian/step/$label", Set(), label, comment, `@extends`)

trait StepWrapper[T <: Step] {
  def toStep(node: Node): Task[T]
}
