package lspace.structure

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap

import lspace.NS
import lspace.datatype._
import lspace.structure.util.ClassTypeable
import monix.eval.{Coeval, Task}

import scala.collection.concurrent
import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration

object Property {
  lazy val ontology: Ontology =
    Ontology(NS.types.`@property`, iris = Set(NS.types.rdfProperty))

  implicit lazy val urlType: IriType[Property] = new IriType[Property] {
    val iri: String = NS.types.`@property`
  }

  implicit val defaultProperty: ClassTypeable.Aux[Property, Property, IriType[Property]] =
    new ClassTypeable[Property] {
      type C  = Property
      type CT = IriType[Property]
      def ct: CT = urlType
    }

//  implicit val defaultString: ClassTypeable.Aux[Property, Edge[Any, Any], EdgeURLType[Edge[Any, Any]]] =
//    new ClassTypeable[Property] {
//      type C  = Edge[Any, Any]
//      type CT = EdgeURLType[Edge[Any, Any]]
//      def ct: CT = EdgeURLType.apply[Edge[Any, Any]]
//    }
  import scala.concurrent.duration._
  def build(node: Node): Coeval[Property] = {
    if (node.hasLabel(urlType).nonEmpty) {
      Coeval.delay {
        val range = Coeval.defer(
          Coeval
            .sequence(node
              .out(default.`@range`)
              .collect {
                case nodes: List[_] =>
                  nodes.map {
                    case node: Node =>
                      ClassType.classtypes
                        .get(node.iri)
                        .getOrElse {
                          ClassType.build(node)
                        } //orElse???
                    case iri: String =>
                      ClassType.classtypes
                        .get(iri)
                        .getOrElse(throw new Exception("@range looks like an iri but cannot be wrapped by a classtype"))
                  }
                case node: Node =>
                  List(ClassType.classtypes.get(node.iri).getOrElse(ClassType.build(node)))
              }
              .flatten))
        val properties = Coeval.defer(
          Coeval
            .sequence(
              node
                .out(default.typed.propertyProperty)
                .map(p => Property.properties.getOrBuild(p))))
        val extended = Coeval.defer(
          Coeval
            .sequence(node.out(default.`@extends`).collect {
              case node: Node =>
                Property.properties.getOrBuild(node)
            }))

        _Property(node.iri)(
          iris = node.iris,
          _range = () => range.value(),
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
          _extendedClasses = () => extended.value(),
          _properties = () => properties.value(),
          node.out(default.typed.baseString).headOption
        )
      }.memoizeOnSuccess
    } else {
      Coeval.raiseError(new Exception(s"${node.iri} is not a property"))
    }
  }

  object properties {
    object default {
      import Property.default._
      val properties = List(
        `@id`,
        `@ids`,
        `@container`, /*entry.iri -> entry, */
        `@range`,
        `@type`,
        `@extends`,
        `@properties`,
        `@language`,
        `@index`,
        `@label`,
        `@comment`,
        `@base`,
        `@value`,
        `@pvalue`,
        `@graph`,
        `@start`,
        `@end`,
        `@createdon`,
        `@modifiedon`,
        `@deletedon`,
        `@transcendedon`,
        `@valueRange`,
        `@keyRange`,
        `@ARange`,
        `@BRange`,
        `@CRange`,
        `@DRange`
      )

      if (properties.size > 99) throw new Exception("extend default-property-id range!")
      val byId    = (100l to 100l + properties.size - 1 toList).zip(properties).toMap
      val byIri   = byId.toList.flatMap { case (id, p) => p.iri :: p.iris.toList map (_ -> p) }.toMap
      val idByIri = byId.toList.flatMap { case (id, p) => p.iri :: p.iris.toList map (_ -> id) }.toMap
    }
    private[lspace] val byIri: concurrent.Map[String, Property] =
      new ConcurrentHashMap[String, Property]().asScala
    private[lspace] val building: concurrent.Map[String, Coeval[Property]] =
      new ConcurrentHashMap[String, Coeval[Property]]().asScala
//    private[lspace] val preparing: concurrent.Map[String, Task[Coeval[Property]]] =
//      new ConcurrentHashMap[String, Task[Coeval[Property]]]().asScala

    def all: List[Property] = byIri.values.toList.distinct
    def get(iri: String): Option[Coeval[Property]] =
      default.byIri
        .get(iri)
        .orElse(byIri.get(iri))
        .map(p => Coeval.now(p))
        .orElse(building.get(iri))
    def getOrBuild(node: Node): Coeval[Property] =
      default.byIri
        .get(node.iri)
        .map(Coeval.now(_))
        .getOrElse(building.getOrElseUpdate(node.iri, build(node).map { p =>
          byIri += p.iri -> p
          p.iris.foreach { iri =>
            properties.byIri += iri -> p
          }
          building.remove(node.iri)
          p
        }.memoizeOnSuccess))
//    def getOrConstruct(iri: String)(constructTask: Task[Coeval[Property]]): Task[Coeval[Property]] = {
//      println(s"getorconstruct ${iri}")
//      default.byIri
//        .get(iri)
//        .map(p => Task.now(Coeval.now(p)))
//        .getOrElse(preparing.getOrElseUpdate(
//          iri,
//          constructTask
//            .map { f =>
//              println(s"constructed ${f.value().iri}"); f
//            }
//            .flatMap { p =>
//              Task {
//                byIri += p.value().iri -> p.value()
//                p.value().iris.foreach { iri =>
//                  properties.byIri += iri -> p.value()
//                }
//                preparing.remove(iri)
//                p
//              }
//            //                .delayExecution(FiniteDuration(1, "s"))
//            //                .runAsyncAndForget(monix.execution.Scheduler.global)
//            }
//            .memoizeOnSuccess
////            .delayExecution(100 millis)
////            .map { f =>
////              println(s"constructed2 ${f.value().iri}"); f
////            }
//        ))
//    }

    def cache(property: Property): Unit = {
      byIri += property.iri -> property
      property.iris.foreach { iri =>
        properties.byIri += iri -> property
      }
    }
    def cached(long: Long): Option[Property]  = default.byId.get(long)
    def cached(iri: String): Option[Property] = default.byIri.get(iri).orElse(byIri.get(iri))

    def remove(iri: String): Unit = byIri.remove(iri)
  }

  object default {
    import DataType.default._

    val `@id`: Property = _Property(NS.types.`@id`)(_range = () => `@string` :: Nil)
    val `@ids`: Property =
      _Property(NS.types.`@ids`)(_range = () => `@string` :: Nil, containers = NS.types.`@set` :: Nil)
    val `@container`: Property =
      _Property(NS.types.`@container`)(_range = () => `@string` :: Nil, containers = NS.types.`@list` :: Nil)
    val `@range`: Property = _Property(NS.types.`@range`)(
      iris = Set(NS.types.schemaRange),
      _range = () => ListType(Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil) :: Nil,
      containers = NS.types.`@listset` :: Nil
    )
    val `@type`: Property = _Property(NS.types.`@type`)(
//      _range = () => DataType.default.`@class` :: DataType.default.`@property` :: DataType.default.`@datatype` :: Nil,
      containers = NS.types.`@listset` :: Nil
    )
    val `@extends`: Property = _Property(NS.types.`@extends`)(
      iris = Set(NS.types.rdfsSubClassOf, NS.types.rdfsSubPropertyOf),
//      _range = () => DataType.default.`@class` :: DataType.default.`@property` :: DataType.default.`@datatype` :: Nil,
      containers = NS.types.`@listset` :: Nil
    )
    val `@properties`: Property = _Property(NS.types.`@properties`)(_range = () => DataType.default.`@property` :: Nil,
                                                                    containers = NS.types.`@set` :: Nil)
    val `@language`: Property =
      _Property(NS.types.`@language`)(_range = () => `@string` :: Nil, containers = NS.types.`@set` :: Nil)
    val `@index`: Property =
      _Property(NS.types.`@index`)(_range = () => `@string` :: Nil, containers = NS.types.`@set` :: Nil)
    val `@label`: Property =
      _Property(NS.types.`@label`)(iris = Set(NS.types.rdfsLabel),
                                   _range = () => `@string` :: Nil,
                                   containers = NS.types.`@language` :: Nil)
    val `@comment`: Property =
      _Property(NS.types.`@comment`)(iris = Set(NS.types.rdfsComment),
                                     _range = () => `@string` :: Nil,
                                     containers = NS.types.`@language` :: Nil)
    val `@base`: Property      = _Property(NS.types.`@base`)(_range = () => `@string` :: Nil)
    val `@value`: Property     = _Property(NS.types.`@value`)
    val `@pvalue`: Property    = _Property(NS.types.`@pvalue`)
    val `@graph`: Property     = _Property(NS.types.`@graph`)(containers = NS.types.`@set` :: Nil)
    val `@start`: Property     = _Property(NS.types.`@start`)(_range = () => `@datetime` :: Nil)
    val `@end`: Property       = _Property(NS.types.`@end`)(_range = () => `@datetime` :: Nil)
    val `@createdon`: Property = _Property(NS.types.`@createdon`)(_range = () => `@datetime` :: Nil)
    val `@modifiedon`: Property =
      _Property(NS.types.`@modifiedon`)(_range = () => `@datetime` :: Nil)
    val `@deletedon`: Property = _Property(NS.types.`@deletedon`)(_range = () => `@datetime` :: Nil)
    val `@transcendedon`: Property =
      _Property(NS.types.`@transcendedon`)(_range = () => `@datetime` :: Nil)
    lazy val `@valueRange`: Property = CollectionType.keys.valueRange
    lazy val `@keyRange`: Property   = MapType.keys.keyRange
    lazy val `@ARange`: Property     = TupleType.keys._1stRange
    lazy val `@BRange`: Property     = TupleType.keys._2ndRange
    lazy val `@CRange`: Property     = TupleType.keys._3rdRange
    lazy val `@DRange`: Property     = TupleType.keys._4rdRange

    object typed {
      lazy val iriUrlString: TypedProperty[String]    = `@id` as `@string`
      lazy val irisUrlString: TypedProperty[String]   = `@ids` as `@string`
      lazy val containerString: TypedProperty[String] = `@container` as `@string`
      //  lazy val entryInt: TypedPropertyKey[Int] = entry as intType)
      lazy val rangeOntology: TypedProperty[Node] = `@range` as Ontology.ontology
      lazy val rangeProperty: TypedProperty[Node] = `@range` as Property.ontology
      lazy val rangeDataType: TypedProperty[Node] = `@range` as DataType.ontology
      lazy val rangeListClassType: TypedProperty[List[Node]] = `@range` as ListType(
        Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil)

      lazy val typeOntology: TypedProperty[Node] = `@type` as Ontology.ontology //Ontology.classType
      //  TYPE.addRange(ontology)
      lazy val typeProperty: TypedProperty[Node] = `@type` as Property.ontology //Property.classType
      //  TYPE.addRange(property)
      lazy val typeDatatype: TypedProperty[Node] = `@type` as DataType.ontology //as DataType.classType
      //  TYPE.addRange(datatype)
      lazy val extendsOntology: TypedProperty[Node]  = `@extends` as Ontology.ontology //as Ontology.classType
      lazy val extendsProperty: TypedProperty[Node]  = `@extends` as Property.ontology //as Property.classType
      lazy val extendsDataType: TypedProperty[Node]  = `@extends` as DataType.ontology //as DataType.classType
      lazy val propertyProperty: TypedProperty[Node] = `@properties` as Property.ontology //as Property.classType
      lazy val languageString: TypedProperty[String] = `@language` as `@string`
      lazy val indexString: TypedProperty[String]    = `@index` as `@string`
      lazy val labelString: TypedProperty[String]    = `@label` as `@string`
      lazy val commentString: TypedProperty[String]  = `@comment` as `@string`
      lazy val baseString: TypedProperty[String]     = `@base` as `@string`
      lazy val pvalueString: TypedProperty[String]   = `@pvalue` as `@string`

      lazy val startDateTime: TypedProperty[Instant]         = `@start` as `@datetime`
      lazy val endDateTime: TypedProperty[Instant]           = `@end` as `@datetime`
      lazy val createdonDateTime: TypedProperty[Instant]     = `@createdon` as `@datetime`
      lazy val modifiedonDateTime: TypedProperty[Instant]    = `@modifiedon` as `@datetime`
      lazy val deletedonDateTime: TypedProperty[Instant]     = `@deletedon` as `@datetime`
      lazy val transcendedOnDateTime: TypedProperty[Instant] = `@transcendedon` as `@datetime`
      lazy val valueListClassType: TypedProperty[List[Node]] = `@valueRange` as ListType(
        Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil)
      lazy val keyListClassType: TypedProperty[List[Node]] = `@keyRange` as ListType(
        Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil)
      lazy val AListClassType: TypedProperty[List[Node]] = `@ARange` as ListType(
        Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil)
      lazy val BListClassType: TypedProperty[List[Node]] = `@BRange` as ListType(
        Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil)
      lazy val CListClassType: TypedProperty[List[Node]] = `@CRange` as ListType(
        Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil)
      lazy val DListClassType: TypedProperty[List[Node]] = `@DRange` as ListType(
        Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil)
    }
  }

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
    transcendedOnDateTime.iri -> transcendedOnDateTime,
    valueListClassType.iri    -> valueListClassType,
    keyListClassType.iri      -> keyListClassType,
    AListClassType.iri        -> AListClassType,
    BListClassType.iri        -> BListClassType,
    CListClassType.iri        -> CListClassType,
    DListClassType.iri        -> DListClassType
  )

  def _Property(iri: String)(implicit
                             iris: Set[String] = Set(),
                             _range: () => List[ClassType[_]] = () => List(),
                             containers: List[String] = List(),
                             label: Map[String, String] = Map(),
                             comment: Map[String, String] = Map(),
                             _extendedClasses: () => List[Property] = () => List(),
                             _properties: () => List[Property] = () => List(),
                             base: Option[String] = None): Property =
    new Property(iri, iris, _range, containers, label, comment, _extendedClasses, _properties, base) {}

  def apply(iri: String,
            iris: Set[String] = Set(),
            range: List[ClassType[_]] = List(),
            containers: List[String] = List(),
            label: Map[String, String] = Map(),
            comment: Map[String, String] = Map(),
            extendedClasses: List[Property] = List(),
            properties: List[Property] = List()): Property =
    new Property(iri, iris, () => range, containers, label, comment, () => extendedClasses, () => properties) {}
}

/**
  *
  * @param iri
  * @param iris
  * @param _range common class-types for the outgoing resource, the first type is implicit for de-/serialization
  * @param containers cardinality w.r.t. the incoming resource, is not used anymore, cardinality is a responsibility of an API, no restrictions at the data-layer
  * @param label a human-readable name
  * @param comment a human-readable description
  * @param _extendedClasses inherited properties
  * @param _properties common meta-properties
  * @param base base-iri of the outgoing resource
  */
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

  lazy val range: List[ClassType[Any]] = _range() ++ extendedClasses.flatMap(_.range) distinct

  def container: Option[String] = containers.headOption

  override lazy val extendedClasses: List[Property] = _extendedClasses()

  override def toString: String = s"property:$iri"

  override def equals(o: Any): Boolean = o match {
    case p: Property => iri == p.iri || iris.contains(p.iri)
    case n: Node     => iri == n.iri || iris.contains(n.iri)
    case _           => false
  }

  override def hashCode(): Int = iri.hashCode
}
