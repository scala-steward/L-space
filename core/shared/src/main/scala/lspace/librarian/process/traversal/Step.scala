package lspace.librarian.process.traversal

import lspace.librarian.process.traversal.step._
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.datatype.NodeURLType
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure.util.OntologyDef

object Step extends OntologyDef {
  trait Properties {}
  private val ontologyNode =
    MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/Step")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- Property.default.`@label` --> "Step" --- Property.default.`@language` --> "en"
  ontologyNode --- Property.default.`@comment` --> "Step" --- Property.default.`@language` --> "en"
  lazy val ontology: Ontology = Ontology(ontologyNode)

  def toStep(node: Node): Step = node match {
    case step: Step => step
    case _ =>
      node.labels match {
        case list if list.contains(step.G.ontology)        => G.wrap(node)
        case list if list.contains(step.N.ontology)        => N.wrap(node)
        case list if list.contains(step.V.ontology)        => V.wrap(node)
        case list if list.contains(step.R.ontology)        => R.wrap(node)
        case list if list.contains(step.E.ontology)        => E.wrap(node)
        case list if list.contains(step.Drop.ontology)     => Drop.wrap(node)
        case list if list.contains(step.Dedup.ontology)    => Dedup.wrap(node)
        case list if list.contains(step.Out.ontology)      => Out.wrap(node)
        case list if list.contains(step.OutMap.ontology)   => OutMap.wrap(node)
        case list if list.contains(step.OutE.ontology)     => OutE.wrap(node)
        case list if list.contains(step.OutV.ontology)     => OutV.wrap(node)
        case list if list.contains(step.OutEMap.ontology)  => OutEMap.wrap(node)
        case list if list.contains(step.Group.ontology)    => Group.wrap(node)
        case list if list.contains(step.Path.ontology)     => Path.wrap(node)
        case list if list.contains(step.Id.ontology)       => Id.wrap(node)
        case list if list.contains(step.In.ontology)       => In.wrap(node)
        case list if list.contains(step.InMap.ontology)    => InMap.wrap(node)
        case list if list.contains(step.InE.ontology)      => InE.wrap(node)
        case list if list.contains(step.InV.ontology)      => InV.wrap(node)
        case list if list.contains(step.InEMap.ontology)   => InEMap.wrap(node)
        case list if list.contains(step.Has.ontology)      => Has.wrap(node)
        case list if list.contains(step.HasNot.ontology)   => HasNot.wrap(node)
        case list if list.contains(step.HasId.ontology)    => HasId.wrap(node)
        case list if list.contains(step.HasIri.ontology)   => HasIri.wrap(node)
        case list if list.contains(step.HasLabel.ontology) => HasLabel.wrap(node)
        case list if list.contains(step.HasValue.ontology) => HasValue.wrap(node)
        case list if list.contains(step.Coin.ontology)     => Coin.wrap(node)
        case list if list.contains(step.As.ontology)       => As.wrap(node)
        case list if list.contains(step.Repeat.ontology)   => Repeat.wrap(node)
        case list if list.contains(step.Select.ontology)   => Select.wrap(node)
        case list if list.contains(step.Project.ontology)  => Project.wrap(node)
        case list if list.contains(step.Where.ontology)    => Where.wrap(node)
        case list if list.contains(step.And.ontology)      => And.wrap(node)
        case list if list.contains(step.Or.ontology)       => Or.wrap(node)
        case list if list.contains(step.Not.ontology)      => Not.wrap(node)
        case list if list.contains(step.Union.ontology)    => Union.wrap(node)
        case list if list.contains(step.Coalesce.ontology) => Coalesce.wrap(node)
        case list if list.contains(step.Local.ontology)    => Local.wrap(node)
        case list if list.contains(step.Range.ontology)    => Range.wrap(node)
        case list if list.contains(step.Label.ontology)    => Label.wrap(node)
        case list if list.contains(step.Limit.ontology)    => Limit.wrap(node)
        case list if list.contains(step.Tail.ontology)     => Tail.wrap(node)
        case list if list.contains(step.Order.ontology)    => Order.wrap(node)
        case list if list.contains(step.Count.ontology)    => Count.wrap(node)
        case list if list.contains(step.Is.ontology)       => Is.wrap(node)
        case list if list.contains(step.Sum.ontology)      => Sum.wrap(node)
        case list if list.contains(step.Max.ontology)      => Max.wrap(node)
        case list if list.contains(step.Min.ontology)      => Min.wrap(node)
        case list if list.contains(step.Mean.ontology)     => Mean.wrap(node)
        case list =>
          throw new Exception(s"No valid Step-ontology found for types ${list}")
      }
  }

  //  object keys {
  //    val traversal = Property(lspace.NS.vocab.Lspace + "librarian/Step/traversal")
  //    traversal.property(Property.default.label, "traversal").head.property(MemGraphDefault.language, "en")
  //    val traversalNode: TypedPropertyKey[Node] = traversal.addRange(Traversal.ontology)
  //  }
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

  //  MemGraphDefault.ns.storeOntology(ontology)
}
trait Step extends Node

trait GraphStep extends Step
trait Terminate extends Step
trait Mutator
trait Aggregator
trait TraverseStep extends Step
trait ResourceStep extends TraverseStep
trait MoveStep     extends TraverseStep
object MoveStep extends OntologyDef {
  private val ontologyNode =
    MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/MoveStep")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- Property.default.`@label` --> "MoveStep" --- Property.default.`@language` --> "en"
  ontologyNode --- Property.default.`@comment` --> "MoveStep" --- Property.default.`@language` --> "en"
  lazy val ontology: Ontology = Ontology(ontologyNode)

  object keys extends Step.Properties {
    private val labelNode =
      MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/MoveStep/label")
    labelNode.addLabel(Property.ontology)
    labelNode --- Property.default.`@label` --> "label" --- Property.default.`@language` --> "en"
    labelNode --- Property.default.`@comment` --> "A label" --- Property.default.`@language` --> "en"
    labelNode --- Property.default.`@container` --> types.`@set`
//    labelNode --- Property.default.`@range` --> DataType.default.nodeURLType
    labelNode --- Property.default.`@range` --> Ontology.ontology
    labelNode --- Property.default.`@range` --> Property.ontology
    labelNode --- Property.default.`@range` --> DataType.ontology

    lazy val label: Property                      = Property(labelNode)
    lazy val labelUrl: TypedProperty[IriResource] = label + DataType.default.`@url`
  }

  /**
    * mirror of properties in object keys
    */
  trait Properties extends Step.Properties {
    lazy val `ns.l-space.eu/librarian/MoveStep/label`: Property                   = keys.label
    lazy val `ns.l-space.eu/librarian/MoveStep/label @Url`: TypedKey[IriResource] = keys.labelUrl
  }

//  implicit def dt[T, CT[Z] <: NodeURLType[Z]](implicit ev: CT[T] <:< NodeURLType[T]) = DataType.urlType[CT[T]]
}
trait MapStep               extends MoveStep with TraverseStep
trait BarrierStep           extends TraverseStep
trait CollectingStep        extends TraverseStep
trait CollectingBarrierStep extends BarrierStep with CollectingStep
trait ReducingBarrierStep   extends BarrierStep
trait SupplyingBarrierStep  extends BarrierStep
trait EnvironmentStep       extends Step

trait FilterStep extends TraverseStep
trait ClipStep   extends FilterStep
trait HasStep    extends FilterStep
object HasStep {
  sealed trait PropertyLabel[T]
  implicit object IsProperty extends PropertyLabel[Property]
  implicit object IsString   extends PropertyLabel[String]
  sealed trait DataTypeLabel[T]
  implicit object IsDataType extends DataTypeLabel[DataType[_]]
  implicit object IsString1  extends DataTypeLabel[String]
}

trait ModulateStep extends Step
trait BranchStep   extends TraverseStep

abstract class StepDef(label: String, comment: String = "") extends OntologyDef {
  protected[traversal] val ontologyNode =
    MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + s"librarian/step/${label}")
  ontologyNode.addLabel(Ontology.ontology)
  ontologyNode --- Property.default.`@extends` --> Step.ontology
  if (label != "")
    ontologyNode --- Property.default.`@label` --> label --- Property.default.`@language` --> "en"
  if (comment != "")
    ontologyNode --- Property.default.`@comment` --> comment --- Property.default.`@language` --> "en"
  //  ontologyNode --- Property.default.comment --> "" --- Property.default.language --> "en"
  lazy val ontology: Ontology = Ontology(ontologyNode)
}

trait StepWrapper[T <: Step] {
  def wrap(node: Node): T
}
