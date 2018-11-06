package lspace.librarian.structure

import java.time.Instant

import lspace.NS
import lspace.librarian.datatype.EdgeURLType
import lspace.librarian.process.traversal.helper.ClassTypeable
import lspace.librarian.provider.mem.MemGraphDefault

import scala.collection.immutable.ListSet

object Property {
  lazy val ontology: Ontology =
    Ontology(NS.types.PROPERTY)(iris = Set(NS.types.rdfProperty))

  implicit lazy val urlType: IriType[Property] = new IriType[Property] {
    val iri: String = NS.types.PROPERTY
  }

  implicit val defaultString: ClassTypeable.Aux[Property, Edge[Any, Any], EdgeURLType[Edge[Any, Any]]] =
    new ClassTypeable[Property] {
      type C  = Edge[Any, Any]
      type CT = EdgeURLType[Edge[Any, Any]]
      def ct: CT = EdgeURLType.edgeUrlType[Edge[Any, Any]]
    }

  def apply(node: Node): Property = {
    if (node.hasLabel(urlType).nonEmpty) {
      Property(node.iri)(
        iris = node.iris,
        _range = () => node.out(default.range).collect { case node: Node => node.graph.ns.getClassType(node) },
        containers = node.out(default.typed.containerString),
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
            case node: Node => MemGraphDefault.ns.getProperty(node.iri).getOrElse(Property(node))
        },
        _properties = () => node.out(default.typed.propertyProperty).map(Property.apply)
      )
    } else {
      throw new Exception(s"${node.iri} is not an ontology")
    }
  }

  object default {
    import DataType.default._

    val iri: Property = Property(NS.types.id)(_range = () => textType :: Nil)
    val iris: Property =
      Property(NS.types.ids)(_range = () => textType :: Nil, containers = NS.types.set :: Nil)
    val container: Property =
      Property(NS.types.container)(_range = () => textType :: Nil, containers = NS.types.list :: Nil)
    val range: Property = Property(NS.types.range)(
      iris = Set(NS.types.schemaRange),
      _range = () =>
        DataType.default.ontologyURLType :: DataType.default.propertyURLType :: DataType.default.dataTypeURLType :: Nil,
      containers = NS.types.listset :: Nil
    )
    val TYPE: Property = Property(NS.types.TYPE)(
      _range = () =>
        DataType.default.ontologyURLType :: DataType.default.propertyURLType :: DataType.default.dataTypeURLType :: Nil,
      containers = NS.types.listset :: Nil
    )
    val EXTENDS: Property = Property(NS.types.EXTENDS)(
      iris = Set(NS.types.rdfsSubClassOf, NS.types.rdfsSubPropertyOf),
      _range = () =>
        DataType.default.ontologyURLType :: DataType.default.propertyURLType :: DataType.default.dataTypeURLType :: Nil,
      containers = NS.types.listset :: Nil
    )
    val properties: Property = Property(NS.types.properties)(_range = () => DataType.default.propertyURLType :: Nil,
                                                             containers = NS.types.set :: Nil)
    val language: Property =
      Property(NS.types.language)(_range = () => textType :: Nil, containers = NS.types.set :: Nil)
    val index: Property =
      Property(NS.types.index)(_range = () => textType :: Nil, containers = NS.types.set :: Nil)
    val label: Property =
      Property(NS.types.label)(iris = Set(NS.types.rdfsLabel),
                               _range = () => textType :: Nil,
                               containers = NS.types.language :: Nil)
    val comment: Property =
      Property(NS.types.comment)(iris = Set(NS.types.rdfsComment),
                                 _range = () => textType :: Nil,
                                 containers = NS.types.language :: Nil)
    val base: Property      = Property(NS.types.base)(_range = () => textType :: Nil)
    val value: Property     = Property(NS.types.value)
    val pvalue: Property    = Property(NS.types.pvalue)
    val graph: Property     = Property(NS.types.graph)(containers = NS.types.set :: Nil)
    val start: Property     = Property(NS.types.start)(_range = () => dateTimeType :: Nil)
    val end: Property       = Property(NS.types.end)(_range = () => dateTimeType :: Nil)
    val createdon: Property = Property(NS.types.createdon)(_range = () => dateTimeType :: Nil)
    val modifiedon: Property =
      Property(NS.types.modifiedon)(_range = () => dateTimeType :: Nil)
    val deletedon: Property = Property(NS.types.deletedon)(_range = () => dateTimeType :: Nil)
    val transcendedOn: Property =
      Property(NS.types.transcendedon)(_range = () => dateTimeType :: Nil)

    object typed {
      lazy val iriUrlString: TypedProperty[String]    = iri as textType
      lazy val irisUrlString: TypedProperty[String]   = iris as textType
      lazy val containerString: TypedProperty[String] = container as textType
      //  lazy val entryInt: TypedPropertyKey[Int] = entry as intType)
      lazy val rangeOntology: TypedProperty[Node] = range as Ontology.ontology
      lazy val rangeDataType: TypedProperty[Node] = range as DataType.ontology

      lazy val typeOntology: TypedProperty[Node] = TYPE as Ontology.ontology //Ontology.classType
      //  TYPE.addRange(ontology)
      lazy val typeProperty: TypedProperty[Node] = TYPE as Property.ontology //Property.classType
      //  TYPE.addRange(property)
      lazy val typeDatatype: TypedProperty[Node] = TYPE as DataType.ontology //as DataType.classType
      //  TYPE.addRange(datatype)
      lazy val extendsOntology: TypedProperty[Node]  = EXTENDS as Ontology.ontology //as Ontology.classType
      lazy val extendsProperty: TypedProperty[Node]  = EXTENDS as Property.ontology //as Property.classType
      lazy val extendsDataType: TypedProperty[Node]  = EXTENDS as DataType.ontology //as DataType.classType
      lazy val propertyProperty: TypedProperty[Node] = properties as Property.ontology //as Property.classType
      lazy val languageString: TypedProperty[String] = language as textType
      lazy val indexString: TypedProperty[String]    = index as textType
      lazy val labelString: TypedProperty[String]    = label as textType
      lazy val commentString: TypedProperty[String]  = comment as textType
      lazy val baseString: TypedProperty[String]     = base as textType
      lazy val pvalueString: TypedProperty[String]   = pvalue as textType

      lazy val startDateTime: TypedProperty[Instant]         = start as dateTimeType
      lazy val endDateTime: TypedProperty[Instant]           = end as dateTimeType
      lazy val createdonDateTime: TypedProperty[Instant]     = createdon as dateTimeType
      lazy val modifiedonDateTime: TypedProperty[Instant]    = modifiedon as dateTimeType
      lazy val deletedonDateTime: TypedProperty[Instant]     = deletedon as dateTimeType
      lazy val transcendedOnDateTime: TypedProperty[Instant] = transcendedOn as dateTimeType
    }
  }

  import default._
  lazy val allProperties: Map[String, Property] = Map[String, Property](
    iri.iri           -> iri,
    iris.iri          -> iris,
    container.iri     -> container, /*entry.iri -> entry, */
    range.iri         -> range,
    TYPE.iri          -> TYPE,
    EXTENDS.iri       -> EXTENDS,
    properties.iri    -> properties,
    language.iri      -> language,
    index.iri         -> index,
    label.iri         -> label,
    comment.iri       -> comment,
    base.iri          -> base,
    value.iri         -> value,
    pvalue.iri        -> pvalue,
    graph.iri         -> graph,
    start.iri         -> start,
    end.iri           -> end,
    createdon.iri     -> createdon,
    modifiedon.iri    -> modifiedon,
    deletedon.iri     -> deletedon,
    transcendedOn.iri -> transcendedOn
  )

  import default.typed._
  lazy val allTypedProperties: Map[String, TypedProperty[_]] = Map(
    iriUrlString.iri          -> iriUrlString,
    irisUrlString.iri         -> irisUrlString,
    containerString.iri       -> containerString,
    rangeOntology.iri         -> rangeOntology,
    rangeDataType.iri         -> rangeDataType,
    typeOntology.iri          -> typeOntology,
    typeProperty.iri          -> typeProperty,
    typeDatatype.iri          -> typeDatatype,
    extendsOntology.iri       -> extendsOntology,
    extendsProperty.iri       -> extendsProperty,
    extendsDataType.iri       -> extendsDataType,
    propertyProperty.iri      -> propertyProperty,
    languageString.iri        -> languageString,
    indexString.iri           -> indexString,
    labelString.iri           -> labelString,
    commentString.iri         -> commentString,
    baseString.iri            -> baseString,
    pvalueString.iri          -> pvalueString,
    startDateTime.iri         -> startDateTime,
    endDateTime.iri           -> endDateTime,
    createdonDateTime.iri     -> createdonDateTime,
    modifiedonDateTime.iri    -> modifiedonDateTime,
    deletedonDateTime.iri     -> deletedonDateTime,
    transcendedOnDateTime.iri -> transcendedOnDateTime
  )

  def apply(iri: String)(implicit
                         iris: Set[String] = Set(),
                         _range: () => List[ClassType[_]] = () => List(),
                         containers: List[String] = List(),
                         label: Map[String, String] = Map(),
                         comment: Map[String, String] = Map(),
                         _extendedClasses: () => List[Property] = () => List(),
                         _properties: () => List[Property] = () => List(),
                         base: Option[String] = None) =
    new Property(iri, iris, _range, containers, label, comment, _extendedClasses, _properties, base) {}
}

class Property(val iri: String,
               val iris: Set[String] = Set(),
               _range: () => List[ClassType[_]] = () => List(),
               val containers: List[String] = List(),
               val label: Map[String, String] = Map(),
               val comment: Map[String, String] = Map(),
               protected val _extendedClasses: () => List[Property] = () => List(),
               protected val _properties: () => List[Property] = () => List(),
               val base: Option[String] = None)
    extends ClassType[Edge[_, _]] {
  type Out = Edge[_, _]
  type CT  = Property

  def as[T](range: ClassType[T]): TypedProperty[T] = TypedProperty(this, range)
  def +[T](range: ClassType[T]): TypedProperty[T]  = as(range)
  def ::[T](range: ClassType[T]): TypedProperty[T] = as(range)

  lazy val range: ListSet[ClassType[_]] = _range().to[ListSet] ++ extendedClasses.flatMap(_.range)

  def container: Option[String] = containers.headOption

  override lazy val extendedClasses: List[Property] = _extendedClasses()

  override def toString: String = s"property:$iri"

  override def equals(o: Any): Boolean = o match {
    case p: Property => iri == p.iri || iris.contains(p.iri)
    case _           => false
  }

  override def hashCode(): Int = iri.hashCode
}
