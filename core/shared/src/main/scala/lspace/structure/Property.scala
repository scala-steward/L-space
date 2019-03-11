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
    Ontology(NS.types.`@property`, iris = Set(NS.types.`@property`, NS.types.rdfProperty))
  lazy val unknownProperty: Ontology =
    Ontology("@unknownProperty", iris = Set("@unknownProperty"), extendedClasses = () => List(ontology))

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
//  import scala.concurrent.duration._
//  private def build(node: Node): Coeval[Property] = {
////    println(s"building ${node.iri}")
//    if (node.hasLabel(unknownProperty).nonEmpty)
//      Coeval(UnknownProperty(node.iri))
//        .map { p =>
//          scribe.trace(s"builded unknown property ${p.iri}"); p
//        }
//        .onErrorHandle { f =>
//          scribe.error("could not build unknown property? " + f.getMessage); throw f
//        } else if (node.hasLabel(ontology).nonEmpty) {
//      Coeval
//        .delay {
//          val range = Coeval.defer(
//            Coeval
//              .sequence(
//                node
//                  .out(default.`@range`)
//                  .headOption
//                  .collect {
//                    case nodes: List[_] =>
//                      nodes.collect {
//                        case node: Node
//                            if node.hasLabel(Ontology.ontology).orElse(node.hasLabel(Property.ontology)).isDefined =>
//                          ClassType.classtypes
//                            .get(node.iri)
//                            .getOrElse {
//                              ClassType.classtypes.getOrBuild(node)
//                            } //orElse???
//                        case iri: String =>
//                          ClassType.classtypes
//                            .get(iri)
//                            .getOrElse(
//                              throw new Exception("@range looks like an iri but cannot be wrapped by a classtype"))
//                      }
//                    case node: Node
//                        if node.hasLabel(Ontology.ontology).orElse(node.hasLabel(Property.ontology)).isDefined =>
//                      List(ClassType.classtypes.get(node.iri).getOrElse(ClassType.classtypes.getOrBuild(node)))
//                  }
//                  .toList
//                  .flatten))
//          val properties = Coeval.defer(
//            Coeval
//              .sequence(
//                (node
//                  .out(Property.default.typed.propertyProperty) ++ node
//                  .in(lspace.NS.types.schemaDomainIncludes)
//                  .collect { case node: Node => node }) //.filter(_.labels.contains(Property.ontology))
//                  .map(p => Property.properties.getAndUpdate(p))))
//          val extended = Coeval.defer(
//            Coeval
//              .sequence(
//                node
//                  .out(default.`@extends`)
//                  .headOption
//                  .collect {
//                    case nodes: List[_] =>
//                      nodes.collect {
//                        case node: Node if node.hasLabel(Property.ontology).isDefined =>
//                          Property.properties
//                            .get(node.iri)
//                            .getOrElse {
//                              Property.properties.getAndUpdate(node)
//                            } //orElse???
//                        case iri: String =>
//                          Property.properties
//                            .get(iri)
//                            .getOrElse(
//                              throw new Exception("@extends looks like an iri but cannot be wrapped by a property"))
//                      }
//                    case node: Node if node.hasLabel(Property.ontology).isDefined =>
//                      List(Property.properties.get(node.iri).getOrElse(Property.properties.getAndUpdate(node)))
//                  }
//                  .toList
//                  .flatten))
//
//          Property(
//            node.iri,
//            iris = node.iris,
//            range = () => range.value(),
//            label = node
//              .outE(default.typed.labelString)
//              .flatMap { edge =>
//                val l = edge.out(Property.default.typed.languageString)
//                if (l.nonEmpty) l.map(_ -> edge.to.value)
//                else List("en"          -> edge.to.value)
//              }
//              .toMap,
//            comment = node
//              .outE(default.typed.commentString)
//              .flatMap { edge =>
//                val l = edge.out(Property.default.typed.languageString)
//                if (l.nonEmpty) l.map(_ -> edge.to.value)
//                else List("en"          -> edge.to.value)
//              }
//              .toMap,
//            extendedClasses = () => extended.value(),
//            properties = () => properties.value()
//          )
//        }
//        .memoizeOnSuccess
//        .map { p =>
//          scribe.trace(s"builded property ${p.iri}"); p
//        }
//        .onErrorHandle { f =>
//          scribe.error(f.getMessage); throw f
//        }
//    } else {
//      scribe.warn(s"could not (yet) build ${node.iri}")
//      Coeval.raiseError(new Exception(s"${node.iri} is not a property"))
//    }
//  }

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
        `@transcendedon`
//        `@valueRange`,
//        `@keyRange`,
//        `@ranges`
      )

      if (properties.size > 99) throw new Exception("extend default-property-id range!")
      val byId    = (100l to 100l + properties.size - 1 toList).zip(properties).toMap
      val byIri   = byId.toList.flatMap { case (id, p) => p.iri :: p.iris.toList map (_ -> p) }.toMap
      val idByIri = byId.toList.flatMap { case (id, p) => p.iri :: p.iris.toList map (_ -> id) }.toMap
    }
    private[lspace] val byIri: concurrent.Map[String, Property] =
      new ConcurrentHashMap[String, Property]().asScala
//    private[lspace] val building: concurrent.Map[String, Coeval[Property]] =
//      new ConcurrentHashMap[String, Coeval[Property]]().asScala

    def all: List[Property] = byIri.values.toList.distinct
    def get(iri: String, iris: Set[String] = Set()): Option[Property] = {
      val allIris = (iris + iri)
      allIris.flatMap(iri => default.byIri.get(iri).orElse(byIri.get(iri))).toList match {
        case List(property) => Some(property)
        case Nil            => None
        case properties =>
          scribe.warn(
            "It looks like multiple properties which have some @id's in common are found, this should not happen...")
          properties.headOption
      }
    }

    /**
      * This method is thread-safe and guarantees to return any existing property (if any) or it creates one.
      * @param iri
      * @param iris
      * @return
      */
    def getOrCreate(iri: String, iris: Set[String]): Property = get(iri, iris).getOrElse {
      synchronized {
        get(iri, iris).getOrElse {
          val property = new Property(iri, iris + iri)
          property.iris.foreach(byIri.update(_, property))
          property
        }
      }
    }
    def getAndUpdate(node: Node): Property = {
      val property = getOrCreate(node.iri, node.iris)

      property.label ++ node
        .outE(Property.default.typed.labelString)
        .flatMap { edge =>
          val l = edge.out(Property.default.typed.languageString)
          if (l.nonEmpty) l.map(_ -> edge.to.value)
          else List("en"          -> edge.to.value)
        }
        .toMap
      property.comment ++ node
        .outE(Property.default.typed.commentString)
        .flatMap { edge =>
          val l = edge.out(Property.default.typed.commentString)
          if (l.nonEmpty) l.map(_ -> edge.to.value)
          else List("en"          -> edge.to.value)
        }
        .toMap
      property.range ++ node
        .out(Property.default.`@range`)
        .headOption
        .collect {
          case nodes: List[_] =>
            nodes.collect {
              case node: Node if node.hasLabel(Ontology.ontology).orElse(node.hasLabel(Property.ontology)).isDefined =>
                ClassType.classtypes
                  .get(node.iri)
                  .getOrElse {
                    ClassType.classtypes.getAndUpdate(node)
                  } //orElse???
              case iri: String =>
                ClassType.classtypes
                  .get(iri)
                  .getOrElse(throw new Exception("@range looks like an iri but cannot be wrapped by a classtype"))
            }
          case node: Node if node.hasLabel(Ontology.ontology).orElse(node.hasLabel(Property.ontology)).isDefined =>
            List(ClassType.classtypes.get(node.iri).getOrElse(ClassType.classtypes.getAndUpdate(node)))
        }
        .toList
        .flatten

      property.properties ++ (node
        .out(Property.default.typed.propertyProperty) ++ node
        .in(lspace.NS.types.schemaDomainIncludes)
        .collect { case node: Node => node })
        .filter(_.labels.contains(Property.ontology))
        .map(Property.properties.getAndUpdate)

      property.extendedClasses ++ node
        .out(Property.default.`@extends`)
        .headOption
        .collect {
          case nodes: List[_] =>
            nodes.collect {
              case node: Node if node.hasLabel(Property.ontology).isDefined =>
                Property.properties
                  .get(node.iri, node.iris)
                  .getOrElse {
                    Property.properties.getAndUpdate(node)
                  } //orElse???
              case iri: String =>
                Property.properties
                  .get(iri)
                  .getOrElse(throw new Exception("@extends looks like an iri but cannot be wrapped by a property"))
            }
          case node: Node if node.hasLabel(Property.ontology).isDefined =>
            List(Property.properties.get(node.iri, node.iris).getOrElse(Property.properties.getAndUpdate(node)))
        }
        .toList
        .flatten

      property
    }

//    def cache(property: Property): Unit = {
//      byIri += property.iri -> property
//      property.iris.foreach { iri =>
//        properties.byIri += iri -> property
//      }
//    }
    def cached(long: Long): Option[Property] = default.byId.get(long)
//    def cached(iri: String): Option[Property] = default.byIri.get(iri).orElse(byIri.get(iri))

//    def remove(iri: String): Unit = byIri.remove(iri)
  }

  object default {
    import DataType.default._

    val `@id`: Property = new Property(NS.types.`@id`) { rangeList = Coeval.delay(`@string` :: Nil).memoizeOnSuccess }
    val `@ids`: Property =
      new Property(NS.types.`@ids`, Set(NS.types.`@ids`, NS.types.schemaSameAs)) {
        rangeList = Coeval.delay(`@string` :: Nil).memoizeOnSuccess
      }
    val `@container`: Property =
      new Property(NS.types.`@container`) { rangeList = Coeval.delay(`@string` :: Nil).memoizeOnSuccess }
    val `@range`: Property = new Property(NS.types.`@range`, iris = Set(NS.types.`@range`, NS.types.schemaRange)) {
      rangeList = Coeval.delay(Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil).memoizeOnSuccess
    }
//      range = () => ListType(Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil) :: Nil
    val `@type`: Property = new Property(NS.types.`@type`, iris = Set(NS.types.`@type`, NS.types.rdfType)) {
      rangeList = Coeval.delay(Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil).memoizeOnSuccess
    }
//      range = () => Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil
    val `@extends`: Property = new Property(
      NS.types.`@extends`,
      iris = Set(NS.types.`@extends`, NS.types.rdfsSubClassOf, NS.types.rdfsSubPropertyOf)) {
      rangeList = Coeval
        .delay(ListType(Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil) :: Nil)
        .memoizeOnSuccess
    }
//      range = () => ListType(Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil) :: Nil
    val `@properties`: Property = new Property(NS.types.`@properties`) {
      rangeList = Coeval.delay(Property.ontology :: Nil).memoizeOnSuccess
    }
//    val `schema:domainIncludes`: Property =
//      new Property(NS.types.schemaDomainIncludes, iris = Set(NS.types.schemaDomainIncludes)) {
//        rangeList = Coeval.delay(Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil).memoizeOnSuccess
//        labelMap = Map("en" -> "domainIncludes")
//      }
    val `@language`: Property =
      new Property(NS.types.`@language`, iris = Set(NS.types.`@language`, NS.types.xsdLanguage)) {
        rangeList = Coeval.delay(`@string` :: Nil).memoizeOnSuccess
      }
    val `@index`: Property =
      new Property(NS.types.`@index`) { rangeList = Coeval.delay(`@string` :: Nil).memoizeOnSuccess }
    val `@label`: Property =
      new Property(NS.types.`@label`, iris = Set(NS.types.`@label`, NS.types.rdfsLabel)) {
        rangeList = Coeval.delay(`@string` :: Nil).memoizeOnSuccess
      }
    val `@comment`: Property =
      new Property(NS.types.`@comment`, iris = Set(NS.types.`@comment`, NS.types.rdfsComment)) {
        rangeList = Coeval.delay(`@string` :: Nil).memoizeOnSuccess
      }
    val `@base`: Property = new Property(NS.types.`@base`) {
      rangeList = Coeval.delay(`@string` :: Nil).memoizeOnSuccess
    }
    val `@value`: Property  = new Property(NS.types.`@value`)
    val `@pvalue`: Property = new Property(NS.types.`@pvalue`)
    val `@graph`: Property  = new Property(NS.types.`@graph`)
    val `@start`: Property = new Property(NS.types.`@start`) {
      rangeList = Coeval.delay(`@datetime` :: Nil).memoizeOnSuccess
    }
    val `@end`: Property = new Property(NS.types.`@end`) {
      rangeList = Coeval.delay(`@datetime` :: Nil).memoizeOnSuccess
    }
    val `@createdon`: Property = new Property(NS.types.`@createdon`) {
      rangeList = Coeval.delay(`@datetime` :: Nil).memoizeOnSuccess
    }
    val `@modifiedon`: Property =
      new Property(NS.types.`@modifiedon`) { rangeList = Coeval.delay(`@datetime` :: Nil).memoizeOnSuccess }
    val `@deletedon`: Property = new Property(NS.types.`@deletedon`) {
      rangeList = Coeval.delay(`@datetime` :: Nil).memoizeOnSuccess
    }
    val `@transcendedon`: Property =
      new Property(NS.types.`@transcendedon`) { rangeList = Coeval.delay(`@datetime` :: Nil).memoizeOnSuccess }
//    lazy val `@valueRange`: Property = CollectionType.keys.valueRange
//    lazy val `@keyRange`: Property   = MapType.keys.keyRange
//    lazy val `@ranges`: Property     = TupleType.keys.range

    object typed {
      lazy val iriUrlString: TypedProperty[String]    = `@id` as `@string`
      lazy val irisUrlString: TypedProperty[String]   = `@ids` as `@string`
      lazy val containerString: TypedProperty[String] = `@container` as `@string`
      //  lazy val entryInt: TypedPropertyKey[Int] = entry as intType)
      lazy val rangeOntology: TypedProperty[List[Node]] = `@range` as ListType(Ontology.ontology :: Nil)
      lazy val rangeProperty: TypedProperty[List[Node]] = `@range` as ListType(Property.ontology :: Nil)
      lazy val rangeDataType: TypedProperty[List[Node]] = `@range` as ListType(DataType.ontology :: Nil)
      lazy val rangeListClassType: TypedProperty[List[Node]] = `@range` as ListType(
        Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil)

      lazy val typeOntology: TypedProperty[Node] = `@type` as Ontology.ontology //Ontology.classType
      //  TYPE.addRange(ontology)
      lazy val typeProperty: TypedProperty[Node] = `@type` as Property.ontology //Property.classType
      //  TYPE.addRange(property)
      lazy val typeDatatype: TypedProperty[Node] = `@type` as DataType.ontology //as DataType.classType
      //  TYPE.addRange(datatype)
      lazy val extendsOntology
        : TypedProperty[List[Node]] = `@extends` as ListType(Ontology.ontology :: Nil) //as Ontology.classType
      lazy val extendsProperty
        : TypedProperty[List[Node]] = `@extends` as ListType(Property.ontology :: Nil) //as Property.classType
      lazy val extendsDataType
        : TypedProperty[List[Node]]                  = `@extends` as ListType(DataType.ontology :: Nil) //as DataType.classType
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
//      lazy val valueListClassType: TypedProperty[List[Node]] = `@valueRange` as ListType(
//        Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil)
//      lazy val keyListClassType: TypedProperty[List[Node]] = `@keyRange` as ListType(
//        Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil)
//      lazy val rangesClasstype: TypedProperty[List[List[Node]]] = `@ranges` as ListType(
//        List(ListType(Ontology.ontology :: Property.ontology :: DataType.ontology :: Nil)))
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
    transcendedOnDateTime.iri -> transcendedOnDateTime
//    valueListClassType.iri    -> valueListClassType,
//    keyListClassType.iri      -> keyListClassType,
//    rangesClasstype.iri       -> rangesClasstype
  )

  private def apply(iri: String,
                    iris: Set[String] = Set(),
                    range: () => List[ClassType[_]],
                    label: Map[String, String] = Map(),
                    comment: Map[String, String] = Map(),
                    extendedClasses: () => List[Property] = () => List(),
                    properties: () => List[Property] = () => List()): Property = {

    def label0           = label
    def comment0         = comment
    def range0           = range
    def extendedClasses0 = extendedClasses
    def properties0      = properties

    new Property(iri, iris + iri) {
      labelMap = label0
      commentMap = comment0
      rangeList = Coeval.delay(range0()).memoizeOnSuccess
      extendedClassesList = Coeval.delay(extendedClasses0()).memoizeOnSuccess
      propertiesList = Coeval.delay(properties0().toSet).memoizeOnSuccess
    }
  }

  def apply(iri: String): Property                    = Property.properties.getOrCreate(iri, Set())
  def apply(iri: String, iris: Set[String]): Property = Property.properties.getOrCreate(iri, iris)
}

/**
  * //TODO: create inverse-link if any
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
               val iris: Set[String] = Set()
//               _range: () => List[ClassType[_]] = () => List(),
//               val containers: List[String] = List(),
//               protected val labelMap: Map[String, String] = Map(),
//               protected val commentMap: Map[String, String] = Map(),
//               protected val _extendedClasses: () => List[Property] = () => List(),
//               protected val _properties: () => List[Property] = () => List(),
//               val base: Option[String] = None
) extends ClassType[Edge[_, _]] { self =>
//  type Out = Edge[_, _]
//  type CT  = Property

  def as[T](range: ClassType[T]): TypedProperty[T] = TypedProperty(this, range)
  def +[T](range: ClassType[T]): TypedProperty[T]  = as(range)

  protected var rangeList
    : Coeval[List[ClassType[Any]]] = Coeval.now(List()).memoizeOnSuccess //_range() ++ extendedClasses.flatMap(_.range) distinct

  object range {
    def apply(): List[ClassType[Any]] = rangeList()
    def apply(iri: String): Option[ClassType[Any]] = rangeList().find(_.iris.contains(iri)).orElse {
      var result: Option[ClassType[Any]] = None
      val oIt                            = extendedClasses().reverseIterator
      while (oIt.hasNext && result.isEmpty) {
        result = oIt.next().range(iri)
      }
      result
    }
    def +(range: ClassType[Any]): this.type = this.synchronized {
      if (!rangeList().contains(range)) rangeList = rangeList.map(_ :+ range).map(_.distinct).memoizeOnSuccess
      this
    }
    def ++(range: Iterable[ClassType[Any]]): this.type = this.synchronized {
      rangeList = rangeList.map(_ ++ range).map(_.distinct).memoizeOnSuccess
      this
    }
    def :=(range: Iterable[ClassType[Any]]): this.type = this.synchronized {
      rangeList = Coeval(range.toList).memoizeOnSuccess
      this
    }
    def -(range: ClassType[Any]): this.type = this.synchronized {
      rangeList = rangeList.map(_.filterNot(_ == range)).memoizeOnSuccess
      this
    }
    def --(range: Iterable[ClassType[Any]]): this.type = this.synchronized {
      rangeList = rangeList.map(_.filterNot(range.toList.contains)).memoizeOnSuccess
      this
    }
  }
  def `@range` = range

//  def container: Option[String] = containers.headOption
//  def `@container`              = container

  protected var extendedClassesList
    : Coeval[List[Property]] = Coeval.now(List()).memoizeOnSuccess //_extendedClasses().filterNot(_.`extends`(this))

//  override def extendedClasses: List[Property] = extendedClassesList.value()
  object extendedClasses {
    def apply(): List[Property] = extendedClassesList()
    def apply(iri: String): Boolean =
      extendedClassesList().exists(_.iris.contains(iri)) || extendedClassesList().exists(_.extendedClasses(iri))

    def +(parent: Property): this.type = this.synchronized {
      if (!parent.`@extends`(self))
        extendedClassesList = extendedClassesList.map(_ :+ parent).map(_.distinct).memoizeOnSuccess
      else scribe.warn(s"$iri cannot extend ${parent.iri} as ${parent.iri} already extends $iri direct or indirect")
      this
    }
    def ++(parent: Iterable[Property]): this.type = this.synchronized {
      parent.foreach(this.+)
      this
    }
    def -(parent: Property): this.type = this.synchronized {
      extendedClassesList = extendedClassesList.map(_.filterNot(_ == parent)).memoizeOnSuccess
      this
    }
    def --(parent: Iterable[Property]): this.type = this.synchronized {
      extendedClassesList = extendedClassesList.map(_.filterNot(parent.toList.contains)).memoizeOnSuccess
      this
    }
  }

  override def toString: String = s"property:$iri"

  override def equals(o: Any): Boolean = o match {
    case p: Property => iri == p.iri || iris.contains(p.iri)
    case n: Node     => iri == n.iri || iris.contains(n.iri)
    case _           => false
  }

  override def hashCode(): Int = iri.hashCode
}

case class UnknownProperty(override val iri: String) extends Property(iri)