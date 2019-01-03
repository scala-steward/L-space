package lspace.librarian.process.traversal

import lspace.librarian.process.traversal.step._
import lspace.librarian.structure._
import lspace.librarian.structure.Ontology.OntologyDef

object Step extends OntologyDef(lspace.NS.vocab.Lspace + "librarian/Step", Set(), "Step", "Step") {

  def toStep(node: Node): Step = node match {
    case step: Step => step
    case _ =>
      node.labels match {
        case list if list.contains(step.G.ontology)        => G.toStep(node)
        case list if list.contains(step.N.ontology)        => N.toStep(node)
        case list if list.contains(step.V.ontology)        => V.toStep(node)
        case list if list.contains(step.R.ontology)        => R.toStep(node)
        case list if list.contains(step.E.ontology)        => E.toStep(node)
        case list if list.contains(step.Drop.ontology)     => Drop.toStep(node)
        case list if list.contains(step.Dedup.ontology)    => Dedup.toStep(node)
        case list if list.contains(step.Out.ontology)      => Out.toStep(node)
        case list if list.contains(step.OutMap.ontology)   => OutMap.toStep(node)
        case list if list.contains(step.OutE.ontology)     => OutE.toStep(node)
        case list if list.contains(step.OutV.ontology)     => OutV.toStep(node)
        case list if list.contains(step.OutEMap.ontology)  => OutEMap.toStep(node)
        case list if list.contains(step.Group.ontology)    => Group.toStep(node)
        case list if list.contains(step.Path.ontology)     => Path.toStep(node)
        case list if list.contains(step.Id.ontology)       => Id.toStep(node)
        case list if list.contains(step.In.ontology)       => In.toStep(node)
        case list if list.contains(step.InMap.ontology)    => InMap.toStep(node)
        case list if list.contains(step.InE.ontology)      => InE.toStep(node)
        case list if list.contains(step.InV.ontology)      => InV.toStep(node)
        case list if list.contains(step.InEMap.ontology)   => InEMap.toStep(node)
        case list if list.contains(step.Has.ontology)      => Has.toStep(node)
        case list if list.contains(step.HasNot.ontology)   => HasNot.toStep(node)
        case list if list.contains(step.HasId.ontology)    => HasId.toStep(node)
        case list if list.contains(step.HasIri.ontology)   => HasIri.toStep(node)
        case list if list.contains(step.HasLabel.ontology) => HasLabel.toStep(node)
        case list if list.contains(step.HasValue.ontology) => HasValue.toStep(node)
        case list if list.contains(step.Coin.ontology)     => Coin.toStep(node)
        case list if list.contains(step.As.ontology)       => As.toStep(node)
        case list if list.contains(step.Repeat.ontology)   => Repeat.toStep(node)
        case list if list.contains(step.Select.ontology)   => Select.toStep(node)
        case list if list.contains(step.Project.ontology)  => Project.toStep(node)
        case list if list.contains(step.Where.ontology)    => Where.toStep(node)
        case list if list.contains(step.And.ontology)      => And.toStep(node)
        case list if list.contains(step.Or.ontology)       => Or.toStep(node)
        case list if list.contains(step.Not.ontology)      => Not.toStep(node)
        case list if list.contains(step.Union.ontology)    => Union.toStep(node)
        case list if list.contains(step.Coalesce.ontology) => Coalesce.toStep(node)
        case list if list.contains(step.Local.ontology)    => Local.toStep(node)
        case list if list.contains(step.Range.ontology)    => Range.toStep(node)
        case list if list.contains(step.Label.ontology)    => Label.toStep(node)
        case list if list.contains(step.Limit.ontology)    => Limit.toStep(node)
        case list if list.contains(step.Tail.ontology)     => Tail.toStep(node)
        case list if list.contains(step.Order.ontology)    => Order.toStep(node)
        case list if list.contains(step.Count.ontology)    => Count.toStep(node)
        case list if list.contains(step.Is.ontology)       => Is.toStep(node)
        case list if list.contains(step.Sum.ontology)      => Sum.toStep(node)
        case list if list.contains(step.Max.ontology)      => Max.toStep(node)
        case list if list.contains(step.Min.ontology)      => Min.toStep(node)
        case list if list.contains(step.Mean.ontology)     => Mean.toStep(node)
        case list =>
          throw new Exception(s"No valid Step-ontology found for types ${list}")
      }
  }

  object keys {}

  lazy val steps: List[StepDef] = List(
    G,
    N,
    V,
    R,
    E,
    Drop,
    Dedup,
    Out,
    OutMap,
    OutE,
    OutV,
    OutEMap,
    Group,
    Path,
    Id,
    In,
    InMap,
    InE,
    InV,
    InEMap,
    Has,
    HasNot,
    HasId,
    HasIri,
    HasLabel,
    HasValue,
    Coin,
    As,
    Repeat,
    Select,
    Project,
    Where,
    And,
    Or,
    Not,
    Union,
    Coalesce,
    Local,
    Range,
    Label,
    Limit,
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

  def toNode: Node
  def prettyPrint: String
}

trait GraphStep extends Step
object GraphStep extends StepDef(label = "GraphStep", comment = "GraphStep", () => Step.ontology :: Nil) {
  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties
}
trait Terminate extends Step
object Terminate extends StepDef(label = "Terminate", comment = "Terminate", () => Step.ontology :: Nil) {
  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties
}
trait Mutator
trait Aggregator
trait TraverseStep extends Step
object TraverseStep extends StepDef(label = "TraverseStep", comment = "TraverseStep", () => Step.ontology :: Nil) {
  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties
}
trait ResourceStep extends TraverseStep
object ResourceStep
    extends StepDef(label = "ResourceStep", comment = "ResourceStep", () => TraverseStep.ontology :: Nil) {
  object keys extends TraverseStep.Properties
  override lazy val properties: List[Property] = TraverseStep.properties
  trait Properties extends TraverseStep.Properties
}
trait MoveStep extends TraverseStep
object MoveStep extends StepDef(label = "MoveStep", comment = "MoveStep", () => TraverseStep.ontology :: Nil) {

  object keys extends Step.Properties {
    object label
        extends Property.PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/MoveStep/label",
          "label",
          "A label",
          container = List(lspace.NS.types.`@set`),
          `@range` = () => Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil
        ) {}
    lazy val labelUrl: TypedProperty[IriResource] = label.property + DataType.default.`@url`
  }

  override lazy val properties: List[Property] = List(keys.label).map(_.property)

  /**
    * mirror of properties in object keys
    */
  trait Properties extends TraverseStep.Properties {
    lazy val `ns.l-space.eu/librarian/MoveStep/label`: Property                   = keys.label
    lazy val `ns.l-space.eu/librarian/MoveStep/label @Url`: TypedKey[IriResource] = keys.labelUrl
  }
}
trait MapStep extends MoveStep with TraverseStep
object MapStep
    extends StepDef(label = "MapStep", comment = "MapStep", () => MoveStep.ontology :: TraverseStep.ontology :: Nil) {
  object keys extends MoveStep.Properties with TraverseStep.Properties
  override lazy val properties: List[Property] = MoveStep.properties ++ TraverseStep.properties
  trait Properties extends MoveStep.Properties with TraverseStep.Properties
}
trait BarrierStep extends TraverseStep
object BarrierStep extends StepDef(label = "BarrierStep", comment = "BarrierStep", () => TraverseStep.ontology :: Nil) {
  object keys extends TraverseStep.Properties
  override lazy val properties: List[Property] = TraverseStep.properties
  trait Properties extends TraverseStep.Properties
}
trait CollectingStep extends TraverseStep
object CollectingStep
    extends StepDef(label = "CollectingStep", comment = "CollectingStep", () => TraverseStep.ontology :: Nil) {
  object keys extends TraverseStep.Properties
  override lazy val properties: List[Property] = TraverseStep.properties
  trait Properties extends TraverseStep.Properties
}
trait CollectingBarrierStep extends BarrierStep with CollectingStep
object CollectingBarrierStep
    extends StepDef(label = "CollectingBarrierStep",
                    comment = "CollectingBarrierStep",
                    () => BarrierStep.ontology :: CollectingStep.ontology :: Nil) {
  object keys extends BarrierStep.Properties with CollectingStep.Properties
  override lazy val properties: List[Property] = BarrierStep.properties ++ CollectingStep.properties
  trait Properties extends BarrierStep.Properties with CollectingStep.Properties
}
trait ReducingBarrierStep extends BarrierStep
object ReducingBarrierStep
    extends StepDef(label = "ReducingBarrierStep", comment = "ReducingBarrierStep", () => BarrierStep.ontology :: Nil) {
  object keys extends BarrierStep.Properties
  override lazy val properties: List[Property] = BarrierStep.properties
  trait Properties extends BarrierStep.Properties
}
trait SupplyingBarrierStep extends BarrierStep
object SupplyingBarrierStep
    extends StepDef(label = "SupplyingBarrierStep", comment = "SupplyingBarrierStep", () => BarrierStep.ontology :: Nil) {
  object keys extends BarrierStep.Properties
  override lazy val properties: List[Property] = BarrierStep.properties
  trait Properties extends BarrierStep.Properties
}
trait EnvironmentStep extends Step
object EnvironmentStep
    extends StepDef(label = "EnvironmentStep", comment = "EnvironmentStep", () => Step.ontology :: Nil) {
  object keys extends Step.Properties
  override lazy val properties: List[Property] = Step.properties
  trait Properties extends Step.Properties
}

trait FilterStep extends TraverseStep
object FilterStep extends StepDef(label = "FilterStep", comment = "FilterStep", () => TraverseStep.ontology :: Nil) {
  object keys extends TraverseStep.Properties
  override lazy val properties: List[Property] = TraverseStep.properties
  trait Properties extends TraverseStep.Properties
}
trait ClipStep extends FilterStep
object ClipStep extends StepDef(label = "ClipStep", comment = "ClipStep", () => FilterStep.ontology :: Nil) {
  object keys extends FilterStep.Properties
  override lazy val properties: List[Property] = FilterStep.properties
  trait Properties extends FilterStep.Properties
}
trait HasStep extends FilterStep
object HasStep extends StepDef(label = "HasStep", comment = "HasStep", () => FilterStep.ontology :: Nil) {
  sealed trait PropertyLabel[T]
  implicit object IsProperty extends PropertyLabel[Property]
  implicit object IsString   extends PropertyLabel[String]
  sealed trait DataTypeLabel[T]
  implicit object IsDataType extends DataTypeLabel[DataType[_]]
  implicit object IsString1  extends DataTypeLabel[String]

  object keys extends FilterStep.Properties
  override lazy val properties: List[Property] = FilterStep.properties
  trait Properties extends FilterStep.Properties
}

trait ModulateStep extends Step
object ModulateStep extends StepDef(label = "ModulateStep", comment = "ModulateStep", () => Step.ontology :: Nil) {
  object keys extends Step.Properties
  override lazy val properties: List[Property] = FilterStep.properties
  trait Properties extends FilterStep.Properties
}
trait BranchStep extends TraverseStep
object BranchStep extends StepDef(label = "BranchStep", comment = "BranchStep", () => TraverseStep.ontology :: Nil) {
  object keys extends TraverseStep.Properties
  override lazy val properties: List[Property] = TraverseStep.properties
  trait Properties extends TraverseStep.Properties
}

abstract class StepDef(label: String,
                       comment: String = "",
                       `@extends`: () => List[Ontology] = () => List(Step.ontology))
    extends OntologyDef(lspace.NS.vocab.Lspace + s"librarian/step/${label}", Set(), label, comment, `@extends`)

trait StepWrapper[T <: Step] {
  def toStep(node: Node): T
}
