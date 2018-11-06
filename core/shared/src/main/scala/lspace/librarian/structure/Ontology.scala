package lspace.librarian.structure

import lspace.NS
import lspace.librarian.datatype.NodeURLType
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.mem.MemGraphDefault
import lspace.librarian.structure.Property.default

object Ontology {
  lazy val ontology: Ontology =
    Ontology(NS.types.CLASS)(iris = Set(NS.types.rdfsClass))

  //  lazy val classType: ClassType[Ontology] = ClassType[Ontology](ldcontext.types.CLASS)
  implicit lazy val urlType: IriType[Ontology] = new IriType[Ontology] {
    val iri: String = NS.types.CLASS
  }

  implicit val defaultOntology: ClassTypeable.Aux[Ontology, Node, NodeURLType[Node]] =
    new ClassTypeable[Ontology] {
      type C  = Node
      type CT = NodeURLType[Node]
      def ct: CT = NodeURLType.nodeType[Node]
    }

  implicit def iriToOntology(iri: String): Ontology =
    MemGraphDefault.ns.getOntology(iri).getOrElse(Ontology(iri)) //todo get from remote
  implicit def ontologyToString(ontology: Ontology): String = ontology.iri

  def apply(node: Node): Ontology = {
    if (node.hasLabel(ontology).nonEmpty) {
      Ontology(node.iri)(
        iris = node.iris,
        _properties = () => node.out(default.typed.propertyProperty).map(Property.apply),
        label = node
          .outE(default.typed.labelString)
          .flatMap { edge =>
            edge.out(default.typed.languageString).map(_ -> edge.to.value)
          }
          .toMap,
        comment = node
          .outE(default.typed.commentString)
          .flatMap { edge =>
            edge.out(default.typed.languageString).map(_ -> edge.to.value)
          }
          .toMap,
        _extendedClasses = () =>
          node.out(default.EXTENDS).collect {
            case node: Node => MemGraphDefault.ns.getOntology(node.iri).getOrElse(Ontology(node))
        },
        base = node.out(default.typed.baseString).headOption
      )
    } else {
      throw new Exception(s"${node.iri} is not an ontology")
    }
  }

  val allOntologies: Map[String, Ontology] = Map[String, Ontology](ontology.iri -> ontology,
                                                                   Property.ontology.iri -> Property.ontology,
                                                                   DataType.ontology.iri -> DataType.ontology)

  def apply(iri: String)(implicit
                         iris: Set[String] = Set(),
                         _properties: () => List[Property] = () => List(),
                         label: Map[String, String] = Map(),
                         comment: Map[String, String] = Map(),
                         _extendedClasses: () => List[Ontology] = () => List(),
                         base: Option[String] = None): Ontology =
    new Ontology(iri, iris, _properties, label, comment, _extendedClasses, base) {}
}

class Ontology(val iri: String,
               val iris: Set[String] = Set(),
               protected val _properties: () => List[Property] = () => List(),
               val label: Map[String, String] = Map(),
               val comment: Map[String, String] = Map(),
               protected val _extendedClasses: () => List[Ontology] = () => List(),
               val base: Option[String] = None)
    extends ClassType[Node] {

  type Out = Node
  type CT  = Ontology

  override lazy val extendedClasses: List[Ontology] = _extendedClasses()

  override def toString: String = s"ontology:$iri"

  override def equals(o: Any): Boolean = o match {
    case p: Ontology => iri == p.iri || iris.contains(p.iri)
    case _           => false
  }

  override def hashCode(): Int = iri.hashCode
}
