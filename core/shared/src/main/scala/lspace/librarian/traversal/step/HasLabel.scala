package lspace.librarian.traversal.step

import lspace.librarian.traversal._
import lspace.provider.detached.DetachedGraph
import lspace.structure._
import lspace.NS.types
import lspace.datatype.DataType
import monix.eval.Task

object HasLabel
    extends StepDef("HasLabel", "A hasLabel-step filters resources by label.", HasStep.ontology :: Nil)
    with StepWrapper[HasLabel] {

  def toStep(node: Node): Task[HasLabel] =
    for {
      labels <-
//        Task {
//        node
//          .out(keys.label)
//          .collect {
//            case node: Node => node //.iri
//          }
//          .map(node.graph.ns.classtypes.get(_))
//      }
      Task
        .gather(
          node
            .out(keys.label)
            .collect {
              case node: Node => node.iri
            }
            .map(node.graph.ns.classtypes.get(_)))
        .map(_.flatten) //.map(_.getOrElse(ClassType.stubNothing)))) //stubNothing is inserted when the ClassType is expected to be non-existing within the graph
    } yield HasLabel(labels)
//      node
//        .out(keys.label)
//        .collect {
//          case node: Node => node
//        }
//        .flatMap(node => node.graph.ns.classtypes.cached(node.iri))
//        .collect {
//          case ct if ct == DataType.ontology => DataType.default.`@datatype`
//          case ct if ct == Ontology.ontology => DataType.default.`@class`
//          case ct if ct == Property.ontology => DataType.default.`@property`
//          case ct                            => ct
//          //TODO:           .getOrElse(throw new Exception("HasLabel with unknown/uncached ontology")))
//        })

  object keys {
    object label
        extends PropertyDef(
          lspace.NS.vocab.Lspace + "librarian/step/HasLabel/Label",
          "Label",
          "A label",
          container = types.`@set` :: Nil,
          `@range` = Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil
        )
    val labelOntologyNode: TypedProperty[Node] = label.property as Ontology.ontology
    val labelPropertyNode: TypedProperty[Node] = label.property as Property.ontology
    val labelDataTypeNode: TypedProperty[Node] = label.property as DataType.ontology
  }
  override lazy val properties: List[Property] = keys.label :: HasStep.properties
  trait Properties extends HasStep.Properties {
    val label             = keys.label
    val labelOntologyNode = keys.labelOntologyNode
    val labelPropertyNode = keys.labelPropertyNode
    val labelDataTypeNode = keys.labelDataTypeNode
  }

  implicit def toNode(step: HasLabel): Task[Node] = {
    for {
      node <- DetachedGraph.nodes.create(ontology)
      _ <- Task.gather(step.label.map {
        case ontology: Ontology =>
          node.addOut(keys.label, ontology.asInstanceOf[Ontology])
        case property: Property =>
          node.addOut(keys.label, property.asInstanceOf[Property])
        case classtype =>
          node.addOut(keys.label, classtype)
      })
    } yield node
  }.memoizeOnSuccess
}

case class HasLabel(label: List[ClassType[_]]) extends HasStep {

  lazy val toNode: Task[Node]      = this
  override def prettyPrint: String = "hasLabel(" + label.map(_.iri).mkString(", ") + ")"
}
