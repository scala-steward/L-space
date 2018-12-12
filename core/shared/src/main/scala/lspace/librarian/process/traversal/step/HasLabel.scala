package lspace.librarian.process.traversal.step

import lspace.librarian.process.traversal._
import lspace.librarian.provider.detached.DetachedGraph
import lspace.librarian.provider.wrapped.WrappedNode
import lspace.librarian.structure._
import lspace.NS.types
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.provider.mem.MemGraphDefault

object HasLabel extends StepDef("HasLabel") with StepWrapper[HasLabel] {

  def wrap(node: Node): HasLabel = node match {
    case node: HasLabel => node
    case _              => HasLabel(node)
  }

  object keys {
    private val labelNode =
      MemGraphDefault.ns.nodes.upsert(lspace.NS.vocab.Lspace + "librarian/step/HasLabel/Label")
    labelNode.addLabel(Property.ontology)
    labelNode --- Property.default.`@label` --> "Label" --- Property.default.`@language` --> "en"
    labelNode --- Property.default.`@comment` --> "A label" --- Property.default.`@language` --> "en"
    labelNode --- Property.default.`@container` --> types.`@set`
    labelNode --- Property.default.`@range` --> Ontology.ontology
    labelNode --- Property.default.`@range` --> Property.ontology
    labelNode --- Property.default.`@range` --> DataType.ontology

    lazy val label: Property                   = Property(labelNode)
    val labelOntologyNode: TypedProperty[Node] = label + Ontology.ontology
    val labelPropertyNode: TypedProperty[Node] = label + Property.ontology
    val labelDataTypeNode: TypedProperty[Node] = label + DataType.ontology
  }

  def apply[CT <: ClassType[_]](labels: List[CT]): HasLabel = {
    val node = DetachedGraph.nodes.create(ontology)

    labels.foreach {
      case ontology: Ontology => node.addOut(keys.label, ontology.asInstanceOf[Ontology])
      case property: Property => node.addOut(keys.label, property.asInstanceOf[Property])
      case classtype          => node.addOut(keys.label, classtype)
    }
    HasLabel(node)
  }

  ontologyNode --- Property.default.`@properties` --> keys.label
  //  MemGraphDefault.ns.storeOntology(ontology)
}

case class HasLabel private (override val value: Node) extends WrappedNode(value) with HasStep {
  def label: List[ClassType[_]] =
    out(HasLabel.keys.label)
      .collect {
        case node: Node => node
      }
      .flatMap(node => graph.ns.getClassType(node.iri))
      .collect {
        case ct if ct == DataType.ontology => DataType.default.`@datatype`
        case ct if ct == Ontology.ontology => DataType.default.`@class`
        case ct if ct == Property.ontology => DataType.default.`@property`
        case ct                            => ct
//TODO:           .getOrElse(throw new Exception("HasLabel with unknown/uncached ontology"))
      }
  override def prettyPrint: String = "hasLabel(" + label.map(_.iri).mkString(", ") + ")"
}
